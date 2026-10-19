load("@toolchains//:libdir.bzl", "exec_with_libdir", "with_libdir")
load("@toolchains//:stages.bzl", "STAGES")

# Useful aliases for building ghc & ghc-pkg in stage1/stage2:
#
#   buck2 build //:stage1-ghc
#   buck2 build //:stage2-ghc
#   buck2 build //:stage1-ghc-pkg
#   buck2 build //:stage2-ghc-pkg
#
# Platforms:
#                 Runs on                Targets
#    ---------------------------------------------------
#    stage1-ghc   execution platform     target platform
#    stage2-ghc   target platform        target platform
#
# stage1-ghc runs on the execution platform and is used to build the
# stage1 libraries and the stage2 compiler, which is built for the
# target platform. In a cross-compilation setting, stage1 is the
# cross-compiler.
#
# Note that even when the target and execution platform are the same,
# Buck2 treats them as different configurations, so if you ask to
# build the stage1 GHC for the target platform it will not be shared
# with the exec platform build. Unfortunately, the target platform is
# the default so if you ask to build e.g. `//ghc:ghc` it will build
# stage1 for the target platform. That's why these aliases exist:
# `buck2 build //:stage1-ghc` builds the exec-platform artifact.

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
