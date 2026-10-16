load("@toolchains//:stages.bzl", "STAGES", "tool_alias")

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
    tool_alias(
        name = stage + "-" + tool,
        actual = "toolchains//:" + stage + "-" + tool,
        visibility = ["PUBLIC"],
    )
    for stage in STAGES[1:]
    for tool in ["ghc", "ghc-pkg"]
]
