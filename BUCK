load("@toolchains//:libdir.bzl", "with_libdir")
load("@toolchains//:stages.bzl", "STAGES")

# Useful aliases for building ghc & ghc-pkg in stage1/stage2:
#
#   buck2 build //:stage1-ghc
#   buck2 build //:stage2-ghc
#   buck2 build //:stage1-ghc-pkg
#   buck2 build //:stage2-ghc-pkg
#
# Note that building executables using the raw targets
# (e.g. //ghc:ghc) will build GHC again, because the configuration
# defaults to the target platform, not the exec platform.
[
    with_libdir(
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
