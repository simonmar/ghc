load("@prelude//haskell:ghc_api_tool.bzl", "GHC_API_TOOLS", "GHC_API_TOOL_PACKAGES", "ghc_api_tool_srcs")
load("@root//buck2:haskell.bzl", "haskell_binary")
load("@root//cabal-buck2:packages.bzl", "PACKAGES", "PACKAGE_SETS", "PACKAGE_TARGETS")
load(":stages.bzl", "STAGES")

def _library(name, stage):
    """The target of the library `name` in the configuration of `stage`."""

    # The project's own package set is stage0's, which uses the bootstrap
    # GHC's RTS. PACKAGE_TARGETS redirects packages of the later sets only.
    if stage not in PACKAGE_SETS:
        packages = PACKAGES
    elif name in PACKAGE_TARGETS:
        return PACKAGE_TARGETS[name]
    else:
        packages = PACKAGE_SETS[stage]
    if name in packages:
        return "root//{}:{}".format(packages[name], name)
    return "third-party-haskell//:" + name

def per_module_tools():
    """The tools of the per-module compiles for a GHC that the build makes.

    The prelude's own tools are linked with the GHC library of the GHC that
    builds them, which is the bootstrap GHC, so they can't read the files of
    a GHC that the build makes. These tools are linked with the GHC library
    of the stage that builds them instead. Built at stage N-1, that library
    is the one the stage N GHC is made from, which is the GHC that compiles
    the targets of stage N. See _STAGE_TOOLS in BUCK.
    """
    packages = GHC_API_TOOL_PACKAGES + ["rts"]
    deps = {"DEFAULT": [_library(p, STAGES[0]) for p in packages]}
    for stage in STAGES[1:]:
        if stage in PACKAGE_SETS:
            deps["toolchains//:" + stage] = [_library(p, stage) for p in packages]
    for tool in GHC_API_TOOLS:
        haskell_binary(
            name = tool,
            srcs = ghc_api_tool_srcs(tool),
            deps = select(deps),
            # The RTS is one of the deps, as the library of each stage.
            auto_packages = [],
        )
