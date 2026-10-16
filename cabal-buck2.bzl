# The project's overrides of the targets that `cabal buck2` generates for its
# packages: package name -> component name -> attributes (see buck2/cabal.bzl).

load("@toolchains//:rts_ways.bzl", "RTS_VARIANTS", "rts_way", "rts_way_name")
load("@toolchains//:stages.bzl", "stage_modes")
load("@toolchains//:version.bzl", "GHC_VERSION")

# What the RTS is compiled with that depends on its way (as Hadrian does,
# see rtsPackageArgs and wayCcArgs in hadrian/src/Settings).
def _rts_way_defines(units):
    defines = []
    if "thr" in units:
        defines.append("-DTHREADED_RTS")
    if "debug" in units:
        defines.append("-DDEBUG")
    if units == ["debug"]:
        defines.append("-DTICKY_TICKY")
    if "p" in units:
        defines.append("-DPROFILING")
    if "dyn" in units:
        defines.append("-DDYNAMIC")
    return defines

def _rts_c_flags(units):
    flags = _rts_way_defines(units) + ['-DRtsWay="rts_{}"'.format(rts_way_name(units))]
    if "debug" in units:
        flags += ["-fno-omit-frame-pointer", "-g3", "-Og"]
    return flags

def _rts_cmm_flags(units):
    # GHC preprocesses Cmm with the C compiler's preprocessor.
    defines = _rts_way_defines(units)
    return defines + ["-optc" + d for d in defines] + (["-ticky"] if units == ["debug"] else [])

# The library of the RTS, as GHC names it for a way: see unitHsLibs in
# compiler/GHC/Unit/Info.hs. (The prelude adds _p to a profiled archive.)
_RTS_LIBRARY = "HSrts-1.0.3"

def _rts_library_name(units, dynamic = False):
    tag = "_".join([u for u in units if u not in ["p", "dyn"]])
    return _RTS_LIBRARY + ("_" + tag if tag else "") + ("-ghc" + GHC_VERSION if dynamic else "")

_X86 = ["prelude//cpu:x86_32", "prelude//cpu:x86_64"]

def _x86_only(flags):
    arms = {cpu: flags for cpu in _X86}
    arms["DEFAULT"] = []
    return select(arms)

def _c_compiler(clang, gcc):
    return select({"root//buck2/constraints:clang": clang, "DEFAULT": gcc})

# See Note [AutoApply.cmm for vectors] in utils/genapply/Main.hs: only the
# vector widths that need them get the instructions.
_VECTOR_FLAGS = {
    "_V32.cmm": _x86_only(["-mavx2"]),
    "_V64.cmm": _x86_only(["-mavx512f"]),
}

OVERRIDES = {
    # The sources that the packages' build systems would generate: see gen/BUCK.
    "ghc": {
        "ghc": {
            "srcs": {
                "GHC.Platform.Constants": "//gen:platform-constants",
                "GHC.Settings.Config": "//gen:settings-config",
            },
            "deps": ["//gen:primop-includes"],
        },
    },
    "ghc-internal": {
        "ghc-internal": {
            "srcs": {
                "GHC.Internal.PrimopWrappers": "//gen:primop-wrappers",
            },
            # What GHC adds when it links ghc-internal itself: see Note
            # [RTS/ghc-internal interface] in rts/RtsToHsIface.c.
            # TODO: with a leading underscore on platforms whose symbols
            # have one.
            "exported_linker_flags": ["-Wl,-u,init_ghc_hs_iface"],
        },
    },
    "ghc-boot": {
        "ghc-boot": {
            "srcs": {
                "GHC.Platform.Host": "//gen:ghc-boot-platform-host",
                "GHC.Version": "//gen:ghc-boot-version",
            },
        },
    },
    "rts": {
        "rts": {
            "provided_sources": {
                f: "//gen:" + f
                for f in ["AutoApply.cmm", "AutoApply_V16.cmm", "AutoApply_V32.cmm", "AutoApply_V64.cmm"]
            },
            "include_dirs": ["//gen:rts-headers-dir"],
            # GHC links the RTS, choosing the variant for the way it links (with
            # -threaded, say) from those that toolchains//:rts-libraries has,
            # each the rts built in another configuration (`variants`).
            "variants": RTS_VARIANTS,
            "hs_libraries": select({"toolchains//:rts_variant": [], "DEFAULT": [_RTS_LIBRARY]}),
            "library_dir": select({"toolchains//:rts_variant": None, "DEFAULT": "toolchains//:rts-libraries"}),
            "static_library_name": rts_way(_rts_library_name),
            "shared_library_name": rts_way(lambda units: _rts_library_name(units, dynamic = True)),
            "cxx": {
                "compiler_flags": rts_way(_rts_c_flags),
                "exported_deps": ["//gen:rts-headers"],
            },
            "cmm": {
                "flags": rts_way(_rts_cmm_flags) + [
                    # GHC finds the platform constants of the RTS it compiles
                    # in its include path, and ghcversion.h (for the C
                    # preprocessor) in its source: there is no rts package yet.
                    "-I$(location //gen:rts-headers-dir)",
                    "-ghcversion-file=$(location //rts:rts-extra-source-files)/include/ghcversion.h",
                ],
                "deps": ["//gen:rts-headers"],
            },
            "src_flags": dict([
                (f, flags)
                for prefix in ["AutoApply", "Jumps"]
                for suffix, flags in _VECTOR_FLAGS.items()
                for f in [prefix + suffix]
            ] + [
                ("Hash.c", ["-O3"]),
                ("sm/Evac.c", ["-funroll-loops"]),
                # Warns about call-clobbered registers on x86_64.
                ("StgCRun.c", ["-w"]),
                ("sm/Compact.c", _c_compiler([], ["-finline-limit=2500"])),
                ("RetainerProfile.c", _c_compiler(["-Wno-incompatible-pointer-types"], [])),
                ("prim/atomic.c", _c_compiler([], ["-Wno-sync-nand"])),
            ]),
        },
    },
}

# The configuration of the targets of each later stage (`cabal buck2
# --later-stage`): they are built at that stage (the first modifier), with
# its build mode. A package only in the stage is configured for it (with a
# PACKAGE file); one also in another is built for whichever stage what
# depends on it is.
PACKAGE_SET_MODIFIERS = {
    "stage1": ["toolchains//:stage1"] + stage_modes("stage1"),
}

# The project's own package set (cabal.project's packages) is stage0's: its
# targets of the packages that are in a later stage too are <component>.stage0.
PROJECT_PACKAGE_SET = "stage0"

# The transitions to each stage, for the <component>-<stage> aliases of its
# packages' components (//libraries/unix:unix-stage1, say), which are built
# with the stage's build mode.
PACKAGE_SET_TRANSITIONS = {
    "stage0": "toolchains//:to-stage0",
    "stage1": "toolchains//:to-stage1",
}
