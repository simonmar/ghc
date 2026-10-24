def _settings_config_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output("GHC/Settings/Config.hs")
    ctx.actions.run(
        cmd_args(
            "python3",
            ctx.attrs.generate,
            "settings-config",
            ctx.attrs.system_config,
            str(ctx.attrs.stage),
            ctx.attrs.unit_id,
            out.as_output(),
        ),
        category = "generate",
    )
    return [DefaultInfo(default_output = out)]

# GHC.Settings.Config of the ghc library, which says (among other things) what
# the library's unit id is.
settings_config = rule(
    impl = _settings_config_impl,
    attrs = {
        "generate": attrs.source(),
        "stage": attrs.int(),
        "system_config": attrs.source(),
        "unit_id": attrs.string(),
    },
)
