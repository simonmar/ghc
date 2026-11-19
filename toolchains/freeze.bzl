load(":stages.bzl", "stage_alias", "stage_links_shared")

# Freezing stage1
#
# If you want to work on the compiler and test your changes by just
# rebuilding stage2, then you need to freeze stage1. Otherwise, every
# change to something in compiler/ would first rebuild the stage1
# compiler, which then forces a rebuild of all the stage1 libraries
# and the whole stage2 compiler.
#
# Freezing stage1 means taking a snapshot of the stage1 compiler (and
# related tools) and telling Buck2 to use that as the stage1
# toolchain.
#
# There are two steps to freeze stage1:
#
#   1. Copy the snapshot into toolchains/_frozen/stage1:
#         buck2 run toolchains//:freeze-stage1
#   2. Tell Buck2 to use it:
#        - set `[ghc] freeze1 = true` in .buckconfig
#        - or `buck2 -c ghc.freeze1=true ...`

# The directory of the copies, in the toolchains cell. Ignored by git.
FROZEN_STAGE1 = "_frozen/stage1"

# The stage1 programs that are frozen. The libdir of the stage1 GHC isn't
# frozen. It has no packages, and none of its files come from the
# compiler's sources, so a change to the compiler doesn't change it.
FROZEN_TOOLS = ["ghc", "ghc-pkg", "haddock", "per-module-compile", "per-module-deps", "per-module-worker"]

def stage1_frozen():
    """Whether `[ghc] freeze1` says that stage1 is frozen."""
    config = read_root_config("ghc", "freeze1", "false").strip().lower()
    if config not in ("true", "false"):
        fail("ghc.freeze1 = {}: expected true or false".format(config))
    return config == "true"

def _frozen_tool_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(default_output = ctx.attrs.program),
        RunInfo(args = cmd_args(ctx.attrs.program)),
    ]

_frozen_tool = rule(
    impl = _frozen_tool_impl,
    attrs = {"program": attrs.source()},
)

def stage_tool_alias(name, stage, tool, actual, build_stage, **kwargs):
    """The tool `tool` of the GHC of `stage`, which is `actual` built at
    `build_stage`. A stage1 tool that is frozen is its copy in FROZEN_STAGE1.

    Each stage1 tool that can be frozen also has a target `<name>-built`,
    which is always the tool that the build makes. Freezing copies those.
    """
    if stage != "stage1" or tool not in FROZEN_TOOLS:
        stage_alias(name = name, actual = actual, stage = build_stage, **kwargs)
        return
    stage_alias(name = name + "-built", actual = actual, stage = build_stage, **kwargs)
    if not stage1_frozen():
        stage_alias(name = name, actual = actual, stage = build_stage, **kwargs)
        return
    program = FROZEN_STAGE1 + "/" + tool
    if not glob([program]):
        fail("ghc.freeze1 = true, but stage1 has not been frozen: toolchains/{} is missing. Freeze it first with `buck2 run -c ghc.freeze1=false toolchains//:freeze-stage1`.".format(program))
    _frozen_tool(name = name, program = program, **kwargs)

def _freeze_stage1_impl(ctx: AnalysisContext) -> list[Provider]:
    # The copies have to run from the snapshot directory. A program that is
    # linked dynamically has the paths of its shared libraries in buck-out,
    # so it can't be copied on its own.
    if ctx.attrs.stage0_links_shared:
        fail("stage1 can only be frozen when stage0 links its programs statically (ghc.stage0 = ..., static), because a program that is linked shared refers to its libraries in buck-out")
    args = cmd_args("sh", ctx.attrs.script, FROZEN_STAGE1)
    for tool, dep in ctx.attrs.tools.items():
        args.add(cmd_args(tool, dep[DefaultInfo].default_outputs[0], delimiter = "="))
    return [DefaultInfo(), RunInfo(args = args)]

_freeze_stage1 = rule(
    impl = _freeze_stage1_impl,
    attrs = {
        "script": attrs.source(),
        "stage0_links_shared": attrs.bool(),
        # The programs, by their names in FROZEN_STAGE1. They are exec deps,
        # so they are configured as the toolchain's tools are.
        "tools": attrs.dict(attrs.string(), attrs.exec_dep(providers = [RunInfo])),
    },
)

def freeze_stage1(**kwargs):
    """A target that `buck2 run` runs to copy the stage1 programs that the
    build makes into FROZEN_STAGE1, replacing what was there."""
    _freeze_stage1(stage0_links_shared = stage_links_shared("stage0"), **kwargs)
