# The testsuite, run against a GHC installation that the build makes: make
# test in the source tree's testsuite/tests, with what the build gives it
# (the GHC, its ghc-config output, and the testsuite's programs), as
# Hadrian's test rule does.

def _testsuite_impl(ctx: AnalysisContext) -> list[Provider]:
    install = ctx.attrs.install[DefaultInfo]
    ghc = cmd_args(install.default_outputs[0], format = "{}/bin/ghc")
    ghc_config = ctx.actions.declare_output("ghcconfig.mk")
    ctx.actions.run(
        cmd_args(
            "sh",
            "-c",
            '"$1" "$2" > "$3"',
            "--",
            ctx.attrs.ghc_config[RunInfo],
            ghc,
            ghc_config.as_output(),
            hidden = install.other_outputs,
        ),
        category = "ghc_config",
    )
    script = ctx.actions.write(
        "test.sh",
        [
            "#!/bin/sh",
            'tests="$1" ghc="$2" ghc_config="$3" timeout="$4" check_ppr="$5"',
            "shift 5",
            'exec make -C "$tests" test TEST_HC="$ghc" ghc_config_mk="$ghc_config" TIMEOUT_PROGRAM="$timeout" CHECK_PPR="$check_ppr" "$@"',
        ],
        is_executable = True,
    )
    return [
        DefaultInfo(default_output = ghc_config),
        RunInfo(args = cmd_args(
            script,
            cmd_args(ctx.attrs.tests_makefile, parent = 1),
            ghc,
            ghc_config,
            ctx.attrs.timeout[RunInfo],
            ctx.attrs.check_ppr[RunInfo],
            hidden = [install.default_outputs, install.other_outputs],
        )),
    ]

# `buck2 run` it, with make's arguments (TEST=..., THREADS=...) after --.
testsuite = rule(
    impl = _testsuite_impl,
    attrs = {
        "check_ppr": attrs.dep(providers = [RunInfo]),
        "ghc_config": attrs.dep(providers = [RunInfo]),
        "install": attrs.dep(),
        # testsuite/tests/Makefile, by which make finds the source tree's
        # tests (the testsuite runs them there, as it always does).
        "tests_makefile": attrs.source(),
        "timeout": attrs.dep(providers = [RunInfo]),
    },
    supports_incoming_transition = True,
)
