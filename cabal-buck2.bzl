# The project's overrides of the targets that `cabal buck2` generates for its
# packages: package name -> component name -> attributes (see buck2/cabal.bzl).

OVERRIDES = {
    # What the Setup.hs of each would generate: see gen/BUCK.
    "ghc": {
        "ghc": {
            "srcs": {
                "GHC.Platform.Constants": "//gen:platform-constants",
                "GHC.Settings.Config": "//gen:settings-config",
            },
            "deps": ["//gen:primop-includes"],
        },
    },
    "ghc-boot": {
        "ghc-boot": {
            "srcs": {
                "GHC.Platform.Host": "//gen:ghc-boot-platform-host",
                "GHC.Version": "//gen:ghc-boot-version",
            },
        },
    },
}
