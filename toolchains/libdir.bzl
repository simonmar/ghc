# The libdir of a GHC that the build makes (GHC's topdir, which it is given
# with -B): its settings and target description, the tools it runs, and its
# global package database, which is empty, as the build passes GHC the
# package databases of what a target depends on.

load("@prelude//haskell:util.bzl", "haskell_library_unit_id")
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
    script = " && ".join([
        'mkdir -p "$1/targets" "$1/bin" "$1/package.conf.d"',
        'cp "$2" "$1/settings"',
        'cp "$3" "$1/targets/default.target"',
        'cp "$4" "$1/bin/unlit"',
    ])
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
        ),
        category = "ghc_libdir",
    )
    return [DefaultInfo(default_output = libdir)]

ghc_libdir = rule(
    impl = _ghc_libdir_impl,
    attrs = {
        # The libraries whose unit ids GHC needs to know: labels, not
        # dependencies, as they are built by the GHC of this libdir.
        "base": attrs.label(),
        "ghc_library": attrs.label(),
        "interpreter": attrs.bool(default = False),
        "target": attrs.source(),
        "unlit": attrs.dep(providers = [RunInfo]),
    },
)

def _with_libdir_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        RunInfo(args = cmd_args(
            ctx.attrs.tool[RunInfo],
            cmd_args(ctx.attrs.libdir[DefaultInfo].default_outputs[0], format = ctx.attrs.flag),
        )),
    ]

# A tool of a GHC that the build makes (ghc, ghc-pkg), run with the GHC's
# libdir, which `flag` (a format string) gives it. Like the tool, the libdir
# and the tools in it are built in the configuration of this target: that of
# the stage that builds the GHC (see stage_alias()).
with_libdir = rule(
    impl = _with_libdir_impl,
    attrs = {
        "flag": attrs.string(),
        "libdir": attrs.dep(),
        "tool": attrs.dep(providers = [RunInfo]),
    },
)
