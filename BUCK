load("@toolchains//:install.bzl", "ghc_install")
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

# Files that a GHC's libdir has (see toolchains/libdir.bzl).
[
    export_file(name = name, src = src, visibility = ["PUBLIC"])
    for name, src in {
        "ghc-usage.txt": "driver/ghc-usage.txt",
        "ghci-usage.txt": "driver/ghci-usage.txt",
        "llvm-passes": "llvm-passes",
        "llvm-targets": "llvm-targets",
    }.items()
]

# An inplace install of stage2, with its tools and libraries. The
# libraries are built static, dynamic, and profiled if `[ghc]
# profiled_libraries = true` in `.buckconfig`.
#
# You can run GHC directly from this target and have access to all the
# libraries, for example to run GHCi:
#
#    buck2 run :stage2-install -- --interactive
#
# The root of the install is given by
#
#   buck2 build //:stage2-install --show-output
#
# This target is also used for running the testsuite, where
# TEST_HC is <the output>/bin/ghc.
ghc_install(
    name = "stage2-install",
    libdir = "toolchains//:stage2-install-libdir",
    programs = {
        "ghc": "toolchains//:stage2-ghc",
        "ghc-pkg": "toolchains//:stage2-ghc-pkg",
        "haddock": "root//utils/haddock:haddock-stage1",
        "hp2ps": "root//utils/hp2ps:hp2ps-stage1",
        "hpc": "root//utils/hpc:hpc-stage1",
        "hsc2hs": "root//utils/hsc2hs:hsc2hs-stage1",
        "runghc": "root//utils/runghc:runghc-stage1",
    },
    visibility = ["PUBLIC"],
)
