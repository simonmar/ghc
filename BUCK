load("@toolchains//:libdir.bzl", "exec_with_libdir", "with_libdir")
load("@toolchains//:stages.bzl", "STAGES")

# Useful aliases for building ghc & ghc-pkg in stage1/stage2:
#
#   buck2 build //:stage1-ghc
#   buck2 build //:stage2-ghc
#   buck2 build //:stage1-ghc-pkg
#   buck2 build //:stage2-ghc-pkg
#
# The stage1 compiler is the one that the build uses to build the stage1
# libraries and the stage2 compiler (for the execution platform). The stage2
# compiler is the product of the build: built at stage1, for the target
# platform, as the stage1 libraries in its libdir are (as Hadrian's is).
#
# Note that building executables using the raw targets
# (e.g. //ghc:ghc) will build GHC again, because the configuration
# defaults to the target platform, not the exec platform.
[
    (exec_with_libdir if stage == "stage1" else with_libdir)(
        name = stage + "-" + tool,
        flag = flag,
        libdir = "toolchains//:" + stage + "-libdir",
        tool = "toolchains//:" + stage + "-" + tool,
        visibility = ["PUBLIC"],
    )
    for stage in STAGES[1:]
    for tool, flag in [
        ("ghc", "-B{}"),
        ("ghc-pkg", "--global-package-db={}/package.conf.d"),
    ]
]
