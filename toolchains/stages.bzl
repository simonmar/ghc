load("@prelude//transitions:utils.bzl", "transition_utils")

# Bootstrapping stages. A target is built by the GHC of its stage, which
# is the `ghc_stage` constraint of its configuration (see BUCK).

# stage0 is the bootstrap GHC: the one `cabal buck2` was run with. The GHC
# of stage N (N > 0) is the GHC built at stage N-1.
STAGES = ["stage0", "stage1", "stage2"]

# The build mode of a stage's targets, as `[ghc] <stage>` in .buckconfig says
# (or -c ghc.<stage>=...), else as here: a comma-separated list of dev or opt,
# optionally static or shared (else the build mode's default linking), and
# optionally prof. It is that of the targets that are built for the stage by
# a transition to it (a compiler of the next stage, say), and of those of a
# package of the stage alone. What is built at stage0 by being named on the
# command line has the command line's modes (-m).
_DEFAULT_STAGE_MODES = {
    "stage0": "opt, static",
    "stage1": "dev, shared",
    "stage2": "dev, shared",
}

_MODES = {
    "dev": "root//buck2/constraints:dev",
    "opt": "root//buck2/constraints:opt",
    "prof": "root//buck2/constraints:prof",
    "shared": "root//buck2/constraints:shared",
    "static": "root//buck2/constraints:static",
}

def stage_modes(stage):
    """The constraints of the build mode of `stage`'s targets."""
    config = read_root_config("ghc", stage, _DEFAULT_STAGE_MODES[stage])
    modes = [m.strip() for m in config.split(",") if m.strip()]
    unknown = [m for m in modes if m not in _MODES]
    optimisation = [m for m in modes if m in ("dev", "opt")]
    linking = [m for m in modes if m in ("static", "shared")]
    if unknown or len(optimisation) != 1 or len(linking) > 1 or ("prof" in modes and "shared" in modes):
        fail("ghc.{} = {}: expected dev or opt, optionally static or shared, and optionally prof (not with shared)".format(stage, config))
    return [_MODES[m] for m in modes]

def stage_links_shared(stage):
    """Whether `stage`'s build mode links programs shared (as buck2/haskell.bzl
    does): the GHC of the next stage, say."""
    modes = stage_modes(stage)
    if _MODES["static"] in modes or _MODES["prof"] in modes:
        return False
    return _MODES["shared"] in modes or _MODES["dev"] in modes

def stage_transition(name, stage, **kwargs):
    """A transition to `stage`, with its build mode (stage_modes())."""
    modes = stage_modes(stage)
    transition_utils.modify_constraints_transition(
        name = name,
        add = ([] if stage == "stage0" else ["toolchains//:" + stage]) + modes,
        # stage0 is the absence of a stage (see BUCK).
        remove = (["toolchains//:stage0"] if stage == "stage0" else []) + ([] if _MODES["prof"] in modes else [_MODES["prof"]]),
        **kwargs
    )

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
