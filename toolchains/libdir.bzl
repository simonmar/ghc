# The libdir of a GHC that the build makes (GHC's topdir, which it is given
# with -B): its settings and target description, the tools it runs, and its
# global package database. That has the libraries that the GHC uses as
# installed ones, if any: the build otherwise passes GHC the package
# databases of what a target depends on.

load("@prelude//haskell:library_info.bzl", "HaskellLibraryInfoTSet")
load("@prelude//haskell:link_info.bzl", "HaskellLinkInfo")
load("@prelude//haskell:toolchain.bzl", "HaskellPlatformInfo", "HaskellToolchainInfo")
load("@prelude//haskell:util.bzl", "haskell_library_unit_id")
load("@prelude//linking:link_info.bzl", "LinkStyle")
load(":rts_ways.bzl", "rts_way_combinations", "rts_way_name")

def _hs_string(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'

def _all_ways():
    # Every way of the RTS (rts_ways.bzl). GHC doesn't read them; Cabal asks
    # GHC for them, to know which ways it can build.
    return [rts_way_name(units) for units in rts_way_combinations()]

def _ghc_libdir_impl(ctx: AnalysisContext) -> list[Provider]:
    settings = [
        ("unlit command", "$topdir/bin/unlit"),
        ("Use interpreter", "YES" if ctx.attrs.interpreter else "NO"),
        ("RTS ways", " ".join(_all_ways())),
        ("Relative Global Package DB", "package.conf.d"),
        ("base unit-id", haskell_library_unit_id(ctx.attrs.base.raw_target())),
        ("Project Unit Id", haskell_library_unit_id(ctx.attrs.ghc_library.raw_target())),
        ("ghc-internal Unit Id", "ghc-internal"),
    ]
    settings_file = ctx.actions.write(
        "settings",
        "[" + "\n,".join(["({}, {})".format(_hs_string(k), _hs_string(v)) for k, v in settings]) + "\n]\n",
    )
    libdir = ctx.actions.declare_output("lib", dir = True)
    libs = _packages(ctx)
    script = " && ".join([
        'mkdir -p "$1/targets" "$1/bin"',
        'cp "$2" "$1/settings"',
        'cp "$3" "$1/targets/default.target"',
        'cp "$4" "$1/bin/unlit"',
        "shift 4",
        'python3 "$@"',
    ])
    files = [f for lib in libs for f in lib.import_dirs.values() + lib.libs + lib.include_dirs]
    ctx.actions.run(
        cmd_args(
            "sh",
            "-c",
            script,
            "--",
            libdir.as_output(),
            settings_file,
            ctx.attrs.target,
            ctx.attrs.unlit[RunInfo],
            ctx.attrs._package_db,
            ctx.attrs.ghc_pkg[RunInfo],
            cmd_args(libdir.as_output(), format = "{}/package.conf.d"),
            {lib.db: None for lib in libs}.keys(),
            hidden = files,
        ),
        category = "ghc_libdir",
    )

    # What the package database refers to, for what runs the GHC.
    return [DefaultInfo(default_output = libdir, other_outputs = files)]

def _packages(ctx: AnalysisContext) -> list:
    # The libraries of `packages`, and those they depend on: built shared,
    # when they are, which a GHC that is itself linked shared needs.
    children = []
    for dep in ctx.attrs.packages:
        info = dep.get(HaskellLinkInfo)
        if info:
            children.append(info.info[LinkStyle("shared")] if LinkStyle("shared") in info.info else info.info.values()[0])
    return list(ctx.actions.tset(HaskellLibraryInfoTSet, children = children).traverse())

ghc_libdir = rule(
    impl = _ghc_libdir_impl,
    attrs = {
        # The libraries whose unit ids GHC needs to know: labels, not
        # dependencies, as they are built by the GHC of this libdir.
        "base": attrs.label(),
        "ghc_library": attrs.label(),
        "ghc_pkg": attrs.exec_dep(providers = [RunInfo]),
        "interpreter": attrs.bool(default = False),
        # The libraries of the global package database: the GHC's
        # installed ones, built by the GHC before it (the stage1 libraries,
        # for the stage2 GHC).
        "packages": attrs.list(attrs.dep(), default = []),
        "target": attrs.source(),
        # The programs that GHC runs, which run where it does.
        "unlit": attrs.exec_dep(providers = [RunInfo]),
        "_package_db": attrs.source(default = "toolchains//:package_db.py"),
    },
)

def _with_libdir_impl(ctx: AnalysisContext) -> list[Provider]:
    libdir = ctx.attrs.libdir[DefaultInfo]
    return [
        DefaultInfo(
            default_outputs = ctx.attrs.tool[DefaultInfo].default_outputs,
            other_outputs = libdir.default_outputs + libdir.other_outputs,
        ),
        RunInfo(args = cmd_args(
            ctx.attrs.tool[RunInfo],
            cmd_args(ctx.attrs.libdir[DefaultInfo].default_outputs[0], format = ctx.attrs.flag),
            hidden = ctx.attrs.libdir[DefaultInfo].other_outputs,
        )),
    ]

# A tool of a GHC that the build makes (ghc, ghc-pkg), run with the GHC's
# libdir, which `flag` (a format string) gives it: as the toolchain runs it,
# for exec_with_libdir (the tool is the one the build uses, for the execution
# platform, and the libdir the one for what it builds), or as a product of the
# build, for with_libdir (the tool built as the libdir's libraries are, for
# the target platform).
with_libdir = rule(
    impl = _with_libdir_impl,
    attrs = {
        "flag": attrs.string(),
        "libdir": attrs.dep(),
        "tool": attrs.dep(providers = [RunInfo]),
    },
)

exec_with_libdir = rule(
    impl = _with_libdir_impl,
    attrs = {
        "flag": attrs.string(),
        "libdir": attrs.dep(),
        "tool": attrs.exec_dep(providers = [RunInfo]),
    },
)

def _ghc_toolchain_impl(ctx: AnalysisContext) -> list[Provider]:
    toolchain = ctx.attrs.toolchain
    if not ctx.attrs.libdir:
        return toolchain.providers
    info = toolchain[HaskellToolchainInfo]
    libdir = ctx.attrs.libdir[DefaultInfo]
    files = libdir.other_outputs
    fields = {field: getattr(info, field) for field in dir(info)}
    fields["compiler"] = cmd_args(info.compiler, cmd_args(libdir.default_outputs[0], format = "-B{}"), hidden = files)
    fields["linker"] = cmd_args(info.linker, cmd_args(libdir.default_outputs[0], format = "-B{}"), hidden = files)
    fields["packager"] = cmd_args(info.packager, cmd_args(libdir.default_outputs[0], format = "--global-package-db={}/package.conf.d"), hidden = files)
    return [
        DefaultInfo(),
        HaskellToolchainInfo(**fields),
        toolchain[HaskellPlatformInfo],
    ]

# A Haskell toolchain whose GHC (and ghc-pkg) is run with a libdir (its
# topdir, -B): a GHC that the build makes. The libdir is for the targets the
# GHC builds (its package database has the libraries it links them with), so
# it is configured as they are, unlike the GHC itself, which runs on the
# execution platform.
ghc_toolchain = rule(
    impl = _ghc_toolchain_impl,
    attrs = {
        "libdir": attrs.option(attrs.dep(), default = None),
        "toolchain": attrs.toolchain_dep(providers = [HaskellToolchainInfo]),
    },
    is_toolchain_rule = True,
)
