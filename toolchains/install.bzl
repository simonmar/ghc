# Construct an installed GHC file tree
#
#    bin/                 # scripts to invoke executables, e.g.
#      ghc                # ghc = libexec/ghc -B<libdir>
#      ghc-pkg
#      hsc2hs
#      ...
#    lib/                 # GHC's libdir
#      settings           # GHC's settings
#      package.conf.d/    # package DB
#        rts.conf
#        ...
#      ...
#    libexec/             # Actual executables
#      ghc
#      ghc-pkg
#    include/             # RTS headers
#      [0-9]*/
#        Rts.h
#        HsFFI.h
#        ...

load(":libdir.bzl", "GhcLibdirInfo")

# How to run each program if it needs extra args, otherwise just exec
# the binary.
_WRAPPERS = {
    "ghc": '"$top/libexec/ghc" -B"$top/lib"',
    # The package database's files have whatever modification times the
    # build gives them, so its cache may look older than its package files.
    "ghc-pkg": '"$top/libexec/ghc-pkg" --global-package-db="$top/lib/package.conf.d" --always-use-cache',
    "hsc2hs": '"$top/libexec/hsc2hs" --template="$top/lib/template-hsc.h"',
    "runghc": '"$top/libexec/runghc" -f "$top/bin/ghc"',
}

def _ghc_install_impl(ctx: AnalysisContext) -> list[Provider]:
    libdir = ctx.attrs.libdir[DefaultInfo]
    files = {"lib": libdir.default_outputs[0]}
    hidden = list(libdir.other_outputs)
    includes = []
    for i, dir in enumerate(ctx.attrs.libdir[GhcLibdirInfo].rts_include_dirs):
        files["include/{}".format(i)] = dir
        includes.append('-I"$top/include/{}"'.format(i))
    for name, program in ctx.attrs.programs.items():
        info = program[DefaultInfo]
        files["libexec/" + name] = info.default_outputs[0]
        hidden += info.other_outputs
        command = _WRAPPERS.get(name, '"$top/libexec/{}"'.format(name))
        files["bin/" + name] = ctx.actions.write(
            "bin/" + name,
            [
                "#!/bin/sh",
                'top="$(cd "$(dirname "$0")/.." && pwd)"',
                " ".join(["exec", command, '"$@"'] + (includes if name == "hsc2hs" else [])),
            ],
            is_executable = True,
        )
    install = ctx.actions.symlinked_dir("install", files)
    return [
        DefaultInfo(default_output = install, other_outputs = hidden),
        RunInfo(args = cmd_args(install, format = "{}/bin/ghc", hidden = hidden)),
    ]

ghc_install = rule(
    impl = _ghc_install_impl,
    attrs = {
        "libdir": attrs.dep(providers = [GhcLibdirInfo]),
        # Program name -> the program: the GHC (ghc) and those installed
        # with it.
        "programs": attrs.dict(attrs.string(), attrs.dep()),
    },
    supports_incoming_transition = True,
)
