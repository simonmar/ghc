# The ways of the RTS: combinations of the units below, each of which a
# constraint of the configuration selects. The RTS is built in a
# configuration for each of its variants (RTS_VARIANTS), whose libraries GHC
# chooses from (see rts.bzl) by the way it links a program. The other
# libraries are the same whatever the RTS's way, except for profiling, which
# is the configuration of the whole program.

load("@prelude//transitions:utils.bzl", "transition_utils")
load(":version.bzl", "GHC_VERSION")

# Unit name -> its constraint, in the order of the units of a way's name
# (thr_debug_p_dyn, say).
RTS_WAY_UNITS = {
    "thr": "toolchains//:threaded",
    "debug": "toolchains//:debug",
    "p": "root//buck2/constraints:prof",
    "dyn": "toolchains//:dynamic",
}

def rts_way_combinations():
    """Every combination of the units (the vanilla way's first), in their
    order."""
    combinations = [[]]
    for unit in RTS_WAY_UNITS:
        combinations = combinations + [c + [unit] for c in combinations]
    return combinations

def _variant_combinations():
    # The ways of the RTS's variants: every combination of units, except
    # profiled dynamic ones, which aren't built.
    return [units for units in rts_way_combinations() if not ("p" in units and "dyn" in units)]

def _setting_name(units):
    return "rts-way-" + "_".join(units)

def rts_way_settings():
    """
    A config_setting (alias for a list of constraints) for each way, e.g.

    config_setting(
        name = "rts-way-thr_debug",
        constraint_values = [
            "toolchains//:threaded",
            "toolchains//:debug",
        ]
    )
    """
    for units in rts_way_combinations()[1:]:
        native.config_setting(
            name = _setting_name(units),
            constraint_values = [RTS_WAY_UNITS[u] for u in units],
            visibility = ["PUBLIC"],
        )

def _transition_name(units):
    return "to-rts-" + rts_way_name(units)

def rts_way_transitions():
    """
    A configuration transition for each RTS variant, e.g.

       toolchains//:to-rts-v
       toolchains//:to-rts-thr
       toolchains//:to-rts-debug
       toolchains//:to-rts-dyn
       ...
    """
    for units in _variant_combinations():
        # A variant is linked as its way is (its library built so).
        linking = "root//buck2/constraints:" + ("shared" if "dyn" in units else "static")
        transition_utils.modify_constraints_transition(
            name = _transition_name(units),
            add = [RTS_WAY_UNITS[u] for u in units] + [linking],
            remove = [RTS_WAY_UNITS[u] for u in RTS_WAY_UNITS if u not in units],
            visibility = ["PUBLIC"],
        )

def rts_way(f):
    """
    Build a select() applying f to each RTS way, e.g.

    select(
       "toolchains//:rts-way-dyn": f(["dyn"]),
       "toolchains//:rts-way-thr_dyn": f(["thr","dyn"]),
       ...
    )
    """
    arms = {"toolchains//:" + _setting_name(units): f(units) for units in rts_way_combinations()[1:]}
    arms["DEFAULT"] = f([])
    return select(arms)

def rts_way_name(units):
    """The name of a way, as GHC knows it (`RTS way` in +RTS --info)."""
    return "_".join(units) if units else "v"

# The variants of the RTS: way name -> the transition to it (a profiled one
# is built in a profiling configuration).
RTS_VARIANTS = {
    rts_way_name(units): "toolchains//:" + _transition_name(units)
    for units in _variant_combinations()
}

# The library of the RTS, as GHC names it: see unitHsLibs in
# compiler/GHC/Unit/Info.hs.
RTS_LIBRARY = "HSrts-1.0.3"

def rts_library_file(units):
    """The file of the RTS's library for a way, as GHC names it (its soname,
    for a dynamic way)."""
    tag = "_".join([u for u in units if u != "dyn"])
    stem = "lib" + RTS_LIBRARY + ("_" + tag if tag else "")
    return stem + ("-ghc" + GHC_VERSION + ".so" if "dyn" in units else ".a")
