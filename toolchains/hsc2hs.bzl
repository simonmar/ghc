# A rule to run the in-tree hsc2hs together with its template
def _hsc2hs_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        RunInfo(args = cmd_args(
            ctx.attrs.hsc2hs[RunInfo],
            "--template=template-hsc.h",
            cmd_args(ctx.attrs.data, format = "--cflag=-I{}"),
        )),
    ]

hsc2hs = rule(
    impl = _hsc2hs_impl,
    attrs = {
        "data": attrs.source(allow_directory = True),
        "hsc2hs": attrs.dep(providers = [RunInfo]),
    },
)
