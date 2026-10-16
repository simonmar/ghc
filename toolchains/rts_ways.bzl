# The ways of the RTS: combinations of the units below, each of which a
# constraint of the configuration selects. The RTS is built in a
# configuration for each of its variants (RTS_VARIANTS), whose libraries GHC
# chooses from (see rts_libraries()) by the way it links a program. The other
# libraries are the same whatever the RTS's way, except for profiling, which
# is the configuration of the whole program.

load("@prelude//transitions:utils.bzl", "transition_utils")
load(":ghc_libraries.bzl", "ghc_libraries")

# Unit name -> its constraint, in the order of the units of a way's name
# (thr_debug_p_dyn, say).
RTS_WAY_UNITS = {
    "thr": "toolchains//:threaded",
    "debug": "toolchains//:debug",
    "p": "root//buck2/constraints:prof",
    "dyn": "toolchains//:dynamic",
}

# The units that distinguish the variants of the RTS of a configuration.
_VARIANT_UNITS = ["thr", "debug", "dyn"]

def rts_way_combinations(units = list(RTS_WAY_UNITS)):
    """Every sub-list of `units` (the vanilla way's first), keeping their
    order."""
    combinations = [[]]
    for unit in units:
        combinations = combinations + [c + [unit] for c in combinations]
    return combinations

def _setting_name(units):
    return "rts-way-" + "_".join(units)

def rts_way_settings():
    """A config_setting for each way but the vanilla one, which is what a
    select() of rts_way() matches the most specific of; and a transition to
    each variant."""
    for units in rts_way_combinations()[1:]:
        native.config_setting(
            name = _setting_name(units),
            constraint_values = [RTS_WAY_UNITS[u] for u in units],
            visibility = ["PUBLIC"],
        )
    for units in rts_way_combinations(_VARIANT_UNITS):
        transition_utils.modify_constraints_transition(
            name = _transition_name(units),
            add = ["toolchains//:rts_variant"] + [RTS_WAY_UNITS[u] for u in units],
            remove = [RTS_WAY_UNITS[u] for u in _VARIANT_UNITS if u not in units],
            visibility = ["PUBLIC"],
        )

def rts_way(f):
    """Selects f(units) for the configuration's way: its list of units, empty
    for the vanilla way."""
    arms = {"toolchains//:" + _setting_name(units): f(units) for units in rts_way_combinations()[1:]}
    arms["DEFAULT"] = f([])
    return select(arms)

def rts_way_name(units):
    """The name of a way, as GHC knows it (`RTS way` in +RTS --info)."""
    return "_".join(units) if units else "v"

def _transition_name(units):
    return "to-rts-" + rts_way_name(units)

# The variants of the RTS of a configuration: way name (without profiling,
# which the configuration has or hasn't) -> the transition to it.
RTS_VARIANTS = {
    rts_way_name(units): "toolchains//:" + _transition_name(units)
    for units in rts_way_combinations(_VARIANT_UNITS)
}

def rts_libraries(name, rts):
    """The libraries of each variant of `rts` (a label) for the
    configuration, as GHC names them: those of the ways that the build mode
    links programs (see buck2/haskell.bzl): dynamic ones or static ones, or
    profiled static ones in a profiling configuration."""
    static = [rts_way_name(units) for units in rts_way_combinations(_VARIANT_UNITS) if "dyn" not in units]
    dynamic = [rts_way_name(units) for units in rts_way_combinations(_VARIANT_UNITS) if "dyn" in units]

    def variants(ways):
        return {way: rts + "-" + way for way in ways}

    ghc_libraries(
        name = name,
        profiling = select({RTS_WAY_UNITS["p"]: True, "DEFAULT": False}),
        variants = select({
            RTS_WAY_UNITS["p"]: variants(static),
            "DEFAULT": select({
                "root//buck2/constraints:static": variants(static),
                "root//buck2/constraints:shared": variants(dynamic),
                "DEFAULT": select({
                    "root//buck2/constraints:opt": variants(static),
                    "DEFAULT": variants(dynamic),
                }),
            }),
        }),
        visibility = ["PUBLIC"],
    )
