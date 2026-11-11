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
    skip = " ".join(["--skip=" + test for test in ctx.attrs.skip])
    script = ctx.actions.write(
        "test.sh",
        [
            "#!/bin/sh",
            'tests="$1" ghc="$2" ghc_config="$3" timeout="$4"',
            "shift 4",
            # The tests to skip are passed to the driver in
            # EXTRA_RUNTEST_OPTS. If the arguments set EXTRA_RUNTEST_OPTS
            # too, its value is added to them, because make would otherwise
            # use only the last value.
            "opts='{}'".format(skip),
            'for arg; do case "$arg" in EXTRA_RUNTEST_OPTS=*) opts="$opts ${arg#EXTRA_RUNTEST_OPTS=}";; esac; done',
            # IN_TREE_COMPILER=YES tells the driver two things, as Hadrian
            # does. The tests can use files in the source tree, where they
            # run. And the programs that tests ask Hadrian to build are
            # available: this script's further arguments set their make
            # variables, before the arguments given to `buck2 run`.
            'exec make -C "$tests" test TEST_HC="$ghc" ghc_config_mk="$ghc_config" TIMEOUT_PROGRAM="$timeout" IN_TREE_COMPILER=YES "$@" EXTRA_RUNTEST_OPTS="$opts"',
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
            [cmd_args(program[RunInfo], format = var + "={}") for var, program in ctx.attrs.programs.items()],
            hidden = [install.default_outputs, install.other_outputs],
        )),
    ]

# `buck2 run` it, with make's arguments (TEST=..., THREADS=...) after --.
testsuite = rule(
    impl = _testsuite_impl,
    attrs = {
        "ghc_config": attrs.dep(providers = [RunInfo]),
        "install": attrs.dep(),
        # The testsuite's programs (check-ppr, the linters, ...), by the make
        # variables that the tests find them in.
        "programs": attrs.dict(attrs.string(), attrs.dep(providers = [RunInfo]), default = {}),
        # Tests to skip, by name: those that the build can't run yet.
        "skip": attrs.list(attrs.string(), default = []),
        # testsuite/tests/Makefile, by which make finds the source tree's
        # tests (the testsuite runs them there, as it always does).
        "tests_makefile": attrs.source(),
        "timeout": attrs.dep(providers = [RunInfo]),
    },
    supports_incoming_transition = True,
)
