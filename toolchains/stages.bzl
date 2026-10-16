# Bootstrapping stages. A target is built by the GHC of its stage, which
# is the `ghc_stage` constraint of its configuration (see BUCK).

# stage0 is the bootstrap GHC: the one `cabal buck2` was run with. The GHC
# of stage N (N > 0) is the GHC built at stage N-1.
STAGES = ["stage0", "stage1", "stage2"]

def _stage_alias_impl(ctx: AnalysisContext) -> list[Provider]:
    return ctx.attrs.actual.providers

_stage_alias = rule(
    impl = _stage_alias_impl,
    attrs = {"actual": attrs.dep()},
    supports_incoming_transition = True,
)

def stage_alias(name, actual, stage, **kwargs):
    """`actual`, built at `stage`."""
    if stage not in STAGES:
        fail("stage_alias {}: unknown stage {}, expected one of {}".format(name, repr(stage), STAGES))
    _stage_alias(
        name = name,
        actual = actual,
        incoming_transition = "toolchains//:to-" + stage,
        **kwargs
    )

def stage_tool(tool, stage0):
    """Selects `tool` of the configuration's stage: `stage0` at stage0, else
    toolchains//:<stage>-<tool>."""
    arms = {"toolchains//:" + stage: "toolchains//:" + stage + "-" + tool for stage in STAGES[1:]}
    arms["DEFAULT"] = stage0
    return select(arms)

def stage_number(as_string = False):
    """Selects the number of the configuration's stage: 0 for stage0."""
    number = str if as_string else (lambda n: n)
    arms = {"toolchains//:" + stage: number(n) for n, stage in enumerate(STAGES) if n > 0}
    arms["DEFAULT"] = number(0)
    return select(arms)
