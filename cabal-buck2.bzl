# The project's overrides of the targets that `cabal buck2` generates for its
# packages: package name -> component name -> attributes (see buck2/cabal.bzl).

OVERRIDES = {
    "ghc-boot": {
        "ghc-boot": {
            "srcs": {
                "GHC.Platform.Host": "//gen:ghc-boot-platform-host",
                "GHC.Version": "//gen:ghc-boot-version",
            },
        },
    },
}
