# Replacing GHC's build system (Hadrian) with Buck2

Status:
* GHC build is complete: stage2 and all the libraries. GHCi works. All the ways: profiling, dynamic.
* You can configure stage0/stage1/stage2 separately to be static/dynamic, optimised/unoptimised, profiled/unprofiled.
* Designed to handle cross-compilation, but that's untested so far.
* Libraries build with `--make` so it might be slow in some cases.
* External build cache is supported (currently untested though)
* **TODO**: tests
* **TODO**: installation

Rough sizes of things, compared with Hadrian's 16,000 lines:

* GHC-specific Buck2 code: about 1400 lines
* `cabal-buck2`: about 4000 lines
* `haskell-buck2`: about 1500 lines

`cabal-buck2` and `haskell-buck2` include some features that had to be added specifically to support the GHC build (e.g. `cabal buck2 --later-stage`), at a guess those amount to <1000 lines.

It was about 3 days of pair programming with Claude Opus 5.5 to get the stage 2 compiler built starting from `cabal-buck2`, but there's quite a lot of loose ends to tidy up in addition to the main TODOs mentioned above.

# Using

## What you need

* [buck2](https://buck2.build/). Install the latest binary. (TODO: there will be a version requirement eventually, we may have to pin the buck2 version to a given cabal-buck2 version)
* The `buck2` branch of https://github.com/simonmar/ghc.git
* The `ghc` branch of https://github.com/simonmar/cabal-buck2. This depends on Cabal `master` because it needs an unreleased upstream fix.
* The `ghc` branch of https://github.com/simonmar/haskell-buck2, checked out as `buck2` in your `ghc` tree.

It doesn't touch anything in Hadrian so you should be able to use Buck2 alongside Hadrian (of course they don't share any build artifacts though).

## Configuration

Take a look in `.buckconfig` to see how to change the build settings for stage1/stage2. You can add settings to a `.buckconfig.local` to avoid modifying tracked files.

## Building

Build a compiler:

```
buck2 build :stage2-ghc
```

or 

```
> buck2 run :stage2-ghc -- --version
...
The Glorious Glasgow Haskell Compilation System, version 10.1.20261015
```

You can also start GHCi with `--interactive`, and check that all the packages are available with `ghc-pkg`:

```
buck2 run :stage2-ghc-pkg -- list
```

## Introspection

Useful commands for inspecting things.

### Inspecting targets

`buck2 uquery <target-pattern>`

See [target pattern](https://buck2.build/docs/concepts/target_pattern/) for the syntax of a target pattern.

for example, `buck2 uquery libraries/ghc-boot:` lists all the targets you can build in `libraries/ghc-boot`.

Each target might have multiple configurations, so to list all the available configurations:

`buck2 cquery <target-pattern>`

This shows all the available configurations of targets that match `<target-pattern>`.

To also see the rule attributes, use 

`buck2 uquery <target-pattern> --output-all-attributes`

To see the *providers* (the outputs of a rule), use

`buck2 cquery <target> --show-providers`

Note that this operates on the *configured graph*, so there might be multiple instances of a rule for different configurations. e.g. different stages for the compiler, different ways for a library.

Show the dependencies (recursively) of a target:

`buck2 cquery 'deps(<target>)'` 

# Design

## High-level design

* Use [cabal-buck2](https://hackage.haskell.org/package/cabal-buck2) to translate GHC's Cabal packages into Buck2 rules.
* We will need some custom Buck2 code to handle GHC's requirements (stages, ways, cross-compilation, ...) but we will try to keep that to a minimum.

Important concepts:

* Note that while Buck2 has a form of [dynamic dependencies](https://buck2.build/docs/rule_authors/dynamic_dependencies/) you can only have dynamic dependencies within a rule; the build graph itself has to be static. Hadrian on the other hand has access to a fully dynamic build graph, and it does exploit this in certain ways. Still, the shape of the GHC build is essentially static: we know exactly which packages and modules will be built in each stage right from the beginning. Dynamic dependencies is just a convenience in Hadrian, and for Buck2 we will accept a little inconvenience. Details in the sections to follow.

* The build directory (`ghc/buck-out`) is opaque to the user; we have no control over where files are stored. You can build a particular rule and ask Buck2 for the location of the output with `buck2 build <target> --show-output`, but some rules have multiple outputs and you won't necessarily be able to get at them this way. This requires a shift in thinking: instead of writing rules that put files in known places for other rules, Buck2 deals in abstract [artifacts](https://buck2.build/docs/api/build/Artifact/), which are produced by rules and consumed by other rules. An artifact can itself be a directory containing other files, though.

## Workflow

The workflow we're aiming for is this:

```
  ./boot                 # only needed when building from git
  ./configure            # repeat from here if any .cabal.in files are modified
  ./generate-buck2.sh    # repeat from here if any .cabal files are modified
  buck2 build :stage2-ghc
  buck2 test //tests/...
```

The `generate-buck2.sh` step runs `cabal buck2`, which depends on a suitable `cabal-buck2` being installed somewhere.

Compared with Hadrian, the workflow differences are:
* Hadrian doesn't have to re-configure if `.cabal.in` files are modified.
* Hadrian doesn't have an explicit step to run if `.cabal` files are modified; on the other hand Buck2 is much faster. It might be possible to put some automation (or at least a build-time check) around this in the future.

## The main pieces

Installed:

* [cabal-buck2](https://github.com/simonmar/cabal-buck2). Implements the `cabal buck2` external command. (TODO: which version / branch, eventually GHC will require a particular version).

In the GHC source tree:

* `.buckconfig` where the top-level config goes. Mostly boilerplate but you can set some build settings in here, such as the optimisation levels for each stage.
* `buck2/` is a clone of [haskell-buck2](https://github.com/simonmar/haskell-buck2), the Buck2 prelude and support code for building Haskell projects. This will need to be a proper submodule in due course.
* `gen/`: code for generating files at build-time, such as `GHC/Internal/PrimopWrappers.hs`, `AutoApply.cmm` and `DerivedConstants.h` (TODO: maybe we should move these next to the packages that use them, e.g. move the RTS-specific stuff into `rts/BUCK`)
* `toolchains/`: support for [stages and toolchains](#stages-and-toolchains)
* `cabal-buck2.bzl`: GHC-specific overrides to apply to the output of `cabal buck2` 
* `cabal.project`, `cabal.project.stage1`: these tell `cabal buck2` what packages we're building, and flags to use.

## Configure

`./configure` produces values that get spliced into files in the tree, e.g. `@ProjectVersionForLib@` is spliced into `libraries/base.cabal.in` to produce `libraries/base.cabal`.

Hadrian does this splicing at build time, so the user doesn't need to re-configure if they change `base.cabal.in`, but for Buck2 we'll need to go back to
doing this in `./configure` because we need to process the `.cabal` files to generate the Buck2 rules.

## Stages and toolchains

To recap: there are three stages, corresponding to which GHC is used at each stage.
* stage 0: the bootstrap (installed) GHC
* stage 1: the GHC built with stage 0, used to build the libraries
* stage 2: the GHC built with stage 1. This is the binary we ship.

Buck2 has a concept of a [toolchain](https://buck2.build/docs/concepts/toolchain/) which is the set of tools (executables) used to implement a rule. 
In Starlark, the toolchain is itself a [rule](https://github.com/simonmar/haskell-buck2/blob/master/toolchains/haskell.bzl) that produces a record (`HaskellToolchainInfo`) containing the tools.

The toolchain is an attribute (input) on every rule that needs it; for example `haskell_library()` is a rule that has the Haskell toolchain as an attribute.

In a normal Cabal build, there's a single toolchain corresponding to the installed GHC. `cabal buck2` sets up a default toolchain consisting of the `ghc` that Cabal would use (that you can set with e.g. `cabal buck2 -w ghc-9.14`). But in the case of GHC we're going to need a different toolchain for each stage.

However, rather than setting up 3 toolchains, we're going to define **one** toolchain that contains different tools depending on the stage. The mechanism brings together a few different Buck2 concepts.

[Configurations and constraints](https://buck2.build/docs/concepts/configurations/). This is how Buck2 handles conditionals in the build: everything that can vary is a *constraint*, and the constraint has a set of allowed *values*. In our case, we're going to define a constraint for the stage, with 3 values (this is from `toolchains/BUCK`):

```
constraint_setting(
    name = "ghc_stage",
    visibility = ["PUBLIC"],
)

STAGES = ["stage0", "stage1", "stage2"]

[
    constraint_value(
        name = stage,
        constraint_setting = ":ghc_stage",
        visibility = ["PUBLIC"],
    )
    for stage in STAGES
]
```

So the constraint corresponding to stage 1 is `//toolchains:stage1`.

To choose something based on a constraint, you can use [`select()`](https://buck2.build/docs/concepts/configurations/#using-configuration-select), like this:

```
   select(
       "//toolchains:stage0": "ghc-9.14",   # the installed ghc
       "//toolchains:stage1": "//ghc:ghc",  # the ghc in the tree
       "//toolchains:stage2": "//ghc:ghc",
       "DEFAULT": "ghc-9.14",
   )
```

But in our case this is not quite enough, because we need to specify that the `//ghc:ghc` used in stage1 is built by stage0, and stage2 is built using stage1. This is done using [Configuration Transitions](https://buck2.build/docs/rule_authors/configuration_transitions/): we wrap `//ghc:ghc` in a rule that says which stage to use for building that target. The details of how to do this are a little bit obscure and use some utilities from the prelude; for the code see `toolchains/BUCK` and `toolchains/stages.bzl`. The good news is once this is set up it's pretty easy to use.

## Configuring the stage 1 libraries

Configuring a Cabal packages requires GHC for a few things:
* It needs to know `ghc --version` and `ghc --info`
* It also needs to know the supported extensions

This is fine for configuring the stage 0 packages because we have the installed GHC, but for the stage 1 packages we haven't built stage 1 yet. Hadrian defers configuring the stage 1 packages until the stage 1 GHC is built: this is a dynamic dependency, so we have to do something different for Buck2.

Yet configuring the stage 1 libraries doesn't need anything we don't already know after `./configure`. It has some tasks in common with `cabal buck2`, but it's a simpler job in various ways:

* There's no need to run a solver, we know exactly which packages we're configuring
* `cabal buck2` already knows how to configure a package and generate the Buck2 rules
* It just needs to know `ghc --version` and `ghc --info`. We can assume stage 1 supports all the known extensions.

So we also use `cabal buck2` to configure the stage 1 libraries, it has a GHC-specific option `--later-stage=stage1:<version>:cabal.project.stage1` that tells it what to configure for stage 1.

## Running the stage 1 compiler

To build the RTS and stage 1 libraries, we need to invoke the stage 1 GHC. Building the binary is easy enough, but to run it we need some files in certain places. GHC is invoked as `ghc -B<libdir>` where `<libdir>` is a directory containing at least

* `settings`
* `targets/default.target`
* `bin/unlit` (to support building `.lhs` files)
* `package.conf.d` (this can be empty)

In Buck2 the build directory is opaque, so we can't just copy files into a known directory and point GHC at it. Instead we have to construct `<libdir>` as an artifact containing the appropriate files using a Buck2 rule. Then, we can pass this artifact to the stage 1 GHC as the argument to `-B`.

The rules to construct libdir are in `toolchains/libdir.bzl`, and the `-B` arguments are passed to the stage 1 and stage 2 GHCs as part of the toolchain definitions.

## Building the RTS

The RTS is more complex than the rest of the libraries, because it needs to be built in more ways. A regular library is built 4 ways: non-profiled dynamic/static and profiled dynamic/static. The RTS adds two more columns to the matrix: threaded and debug, giving a total of 16 ways.

The tricky bit here is the normal ways (dynamic/static and profiled/non-profiled) are handled entirely by the Buck2 prelude, and mostly it bypasses GHC's handling of ways: instead of one package with multiple ways and files with specific names (`libHSfoo_dyn_<version>.so`) Buck2 just builds a bespoke package for the current way and passes it to GHC, using `extra-libraries` instead of `hs-libraries` so that GHC doesn't use its special logic to derive the name of the library from the way. Partly this approach is chosen because Buck2 integrates with C/C++ and other languages and they all work the same way, and in any case it's all transparent to the user.

But we can't use this approach for the RTS, because its ways are "RTS-only": `-threaded` is a flag that just selects a different variant of the RTS at link time, it doesn't change anything else about the build.

So for the RTS, we have a hybrid mechanism: there are 4 versions of the RTS corresponding to each of the normal ways (profiled on/off and dynamic on/off), and each of those internally has 4 variants (threaded on/off and debug on/off). To make this work we have to create libraries with the names that GHC is expecting, so that GHC can link the right libraries.

The way this works is as follows. Let's look at the targets under `rts/`:

```
$ buck2 uquery rts:
root//rts:rts
root//rts:rts-cmm
root//rts:rts-cxx
root//rts:rts-cxx-headers
root//rts:rts-debug
root//rts:rts-debug_dyn
root//rts:rts-dyn
root//rts:rts-extra-source-files
root//rts:rts-include-0
root//rts:rts-include-1
root//rts:rts-stage1
root//rts:rts-thr
root//rts:rts-thr_debug
root//rts:rts-thr_debug_dyn
root//rts:rts-thr_dyn
root//rts:rts-v
```

The important ones:

* `rts:rts`: the RTS library itself, which can be built in one of its 16 ways depending on which constrain(s) are enabled (`toolchains//:threaded`, `toolchains//:debug` etc.).
* `rts:rts-cxx` and `rts:rts-cmm`, sub-libraries for the C and Cmm sources in the RTS respectively, these just build the objects that get gathered into the final `.a` or `.so` by the `rts:rts` rule. The flags used when compiling these files depend on the way constraints: try `buck2 uquery rts:rts-cxx` to see the conditional attributes.
* `rts:rts-debug`, `rts:rts-thr`, ...: each of these wraps `rts:rts` with constraints to tell it which way to build. 

Finally, there is a rule `toolchains//:rts` that gathers together 4 variants of the RTS (+/- threaded, +/- debug) into a single package by renaming the libraries produced by each individual variant to the ones GHC expects to find. This is implemented by code in `toolchains/rts.bzl`. (why only 4 variants? Because the other ways (+/- dynamic and +/- profiling) are handled separately by the prevailing build mode (`-m static` or `-m prof`). To build the final installer we'll need to combine all of these into a single packages, but that's a TODO for later.

## Building the stage 1 libraries

TODO

# Open questions

* Should `cabal-buck2` be a submodule and be built as part of the build, like Hadrian? Or otherwise we have to have a configure check for it.
