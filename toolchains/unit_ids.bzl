load("@root//buck2:cabal.bzl", "cabal_unit_id")
load("@root//compiler:BUCK.cabal.bzl", _GHC_SPEC = "local_build_spec")
load("@root//libraries/base:BUCK.cabal.bzl", _BASE_SPEC = "local_build_spec")
load(":stages.bzl", "STAGES")

# The unit ids of libraries that GHC knows by them: base's, and that of the
# ghc library as the configuration builds it (the project's at stage0, and
# stage1's at the later stages).
BASE_UNIT_ID = cabal_unit_id(_BASE_SPEC, "base", "stage1")

GHC_UNIT_ID = select(dict(
    {"toolchains//:" + stage: cabal_unit_id(_GHC_SPEC, "ghc", "stage1") for stage in STAGES[1:]},
    DEFAULT = cabal_unit_id(_GHC_SPEC, "ghc"),
))
