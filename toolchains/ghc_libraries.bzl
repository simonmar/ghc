# The libraries of a Haskell library for every way of linking it, in one
# directory, named as GHC names them: the library_dir of a library that GHC
# links itself (see hs_libraries of haskell_library()), the RTS.

load("@prelude//haskell:library_info.bzl", "HaskellLibraryProvider")
load("@prelude//linking:link_info.bzl", "LinkStyle")
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraries",
    "SharedLibraryInfo",
    "merge_shared_libraries",
    "traverse_shared_library_info",
)

def _ghc_libraries_impl(ctx: AnalysisContext) -> list[Provider]:
    files = {}
    shared_libraries = []
    for way, variant in ctx.attrs.variants.items():
        provider = variant[HaskellLibraryProvider]
        if "dyn" in way.split("_"):
            lib = provider.lib[LinkStyle("shared")].libs[0]

            # The variant's own shared library, not its dependencies'.
            shared_libraries += [
                shared
                for shared in traverse_shared_library_info(variant[SharedLibraryInfo], transformation_provider = None)
                if shared.lib.output == lib
            ]
        else:
            lib = (provider.prof_lib if ctx.attrs.profiling else provider.lib)[LinkStyle("static")].libs[0]
        files[lib.basename] = lib
    return [
        DefaultInfo(default_output = ctx.actions.symlinked_dir(ctx.label.name, files)),
        merge_shared_libraries(ctx.actions, SharedLibraries(libraries = shared_libraries)),
    ]

ghc_libraries = rule(
    impl = _ghc_libraries_impl,
    attrs = {
        "profiling": attrs.bool(default = False),
        # Way name (dyn among its units for a dynamic one) -> the library as
        # it is built for that way.
        "variants": attrs.dict(attrs.string(), attrs.dep()),
    },
)
