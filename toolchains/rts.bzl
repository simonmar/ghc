# The RTS's package: a Haskell library that GHC links itself, choosing the
# variant of the RTS for the way it links a program (-threaded, -debug,
# -dynamic, -prof). The variants are the rts library built in a
# configuration for each way (see rts_ways.bzl); this gathers their
# libraries under the names that GHC gives them, and registers a package
# whose hs-libraries GHC finds there. Packages that depend on the rts depend
# on this (PACKAGE_TARGETS in cabal-buck2.bzl).

load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load("@prelude//cxx:preprocessor.bzl", "CPreprocessorInfo")
load("@prelude//decls/toolchains_common.bzl", "toolchains_common")
load("@prelude//haskell:library_info.bzl", "HaskellLibraryInfo", "HaskellLibraryInfoTSet", "HaskellLibraryProvider")
load("@prelude//haskell:link_info.bzl", "HaskellLinkInfo", "HaskellProfLinkInfo")
load(
    "@prelude//linking:link_info.bzl",
    "LibOutputStyle",
    "LinkInfo",
    "LinkInfos",
    "LinkStrategy",
    "LinkStyle",
    "MergedLinkInfo",
    "create_merged_link_info",
    "get_own_link_infos",
)
load("@prelude//linking:linkable_graph.bzl", "create_linkable_graph", "create_linkable_graph_node", "create_linkable_node")
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraries",
    "SharedLibraryInfo",
    "merge_shared_libraries",
    "traverse_shared_library_info",
)
load("@prelude//linking:types.bzl", "Linkage")
load(":rts_ways.bzl", "RTS_LIBRARY", "RTS_VARIANTS", "RTS_WAY_UNITS", "rts_library_file")

def _units(way):
    return [] if way == "v" else way.split("_")

def _rts_package_impl(ctx: AnalysisContext) -> list[Provider]:
    library = ctx.attrs.library
    profiling = ctx.attrs.profiling

    # The variants' libraries, under GHC's names. A dynamic variant's soname
    # is already GHC's (the rts's `soname`), which is what a program that is
    # linked with it looks for.
    files = {}
    shared_libraries = []
    some_variant = None
    for way, variant in ctx.attrs.variants.items():
        provider = variant[HaskellLibraryProvider]
        units = _units(way)
        if "dyn" in units:
            lib = provider.lib[LinkStyle("shared")].libs[0]
            shared_libraries += [
                shared
                for shared in traverse_shared_library_info(variant[SharedLibraryInfo], transformation_provider = None)
                if shared.lib.output == lib
            ]
        else:
            lib = (provider.prof_lib if "p" in units else provider.lib)[LinkStyle("static")].libs[0]
        files[rts_library_file(units)] = lib
        some_variant = some_variant or provider
    libdir = ctx.actions.symlinked_dir("lib", files)

    # The package: one of the variants' (they differ only in their
    # libraries), with GHC's hs-libraries in libdir instead.
    db = ctx.actions.declare_output("db", dir = True)
    variant_info = (some_variant.prof_lib if profiling else some_variant.lib).values()[0]
    # What GHC links the RTS with: what its own C needs (-lm, -lrt, say).
    own = get_own_link_infos(library[MergedLinkInfo], LinkStrategy("static"))
    manifest = ctx.actions.write_json("package.json", [{
        "db": variant_info.db,
        "dynamic_library_dirs": [libdir],
        "hs_library": ctx.attrs.hs_library,
        "ld_options": own.default.pre_flags if own else [],
        "library_dirs": [libdir],
    }], with_inputs = True)
    ctx.actions.run(
        cmd_args("python3", ctx.attrs._package_db, ctx.attrs.ghc_pkg[RunInfo], db.as_output(), manifest),
        category = "rts_package",
    )
    hlib = HaskellLibraryInfo(
        name = variant_info.name,
        db = db,
        id = variant_info.id,
        import_dirs = variant_info.import_dirs,
        stub_dirs = variant_info.stub_dirs,
        include_dirs = variant_info.include_dirs,
        libs = [libdir],
        version = variant_info.version,
        is_prebuilt = False,
        profiling_enabled = profiling,
        linked_by_ghc = True,
    )

    # What the rts depends on is the rts library's of this configuration (its
    # own libraries, not being referred to, aren't built).
    def link_info(infos):
        return {
            link_style: ctx.actions.tset(
                HaskellLibraryInfoTSet,
                value = hlib,
                children = [
                    ctx.actions.tset(HaskellLibraryInfoTSet, value = lib)
                    for lib in tset.traverse()
                    if lib.id != variant_info.id
                ],
            )
            for link_style, tset in infos.items()
        }

    library_link = library[HaskellLinkInfo]
    haskell_link_info = HaskellLinkInfo(info = link_info(library_link.info), prof_info = link_info(library_link.prof_info))

    # GHC links the RTS: what is linked with it is only what its own C needs.
    link_infos = {
        output_style: LinkInfos(default = LinkInfo(name = ctx.label.name, pre_flags = own.default.pre_flags if own else []))
        for output_style in LibOutputStyle
    }
    pic_behavior = get_cxx_toolchain_info(ctx).pic_behavior
    merged = create_merged_link_info(ctx, pic_behavior, link_infos, preferred_linkage = Linkage("any"))

    # The shared libraries of what the rts depends on, and its own variants'.
    library_shared = library[SharedLibraryInfo]
    shared_libraries += [
        shared
        for shared in traverse_shared_library_info(library_shared, transformation_provider = None)
        if shared.label.raw_target() != library.label.raw_target()
    ]

    return [
        DefaultInfo(default_outputs = [libdir, db]),
        HaskellLibraryProvider(
            lib = {link_style: hlib for link_style in library[HaskellLibraryProvider].lib},
            prof_lib = {link_style: hlib for link_style in library[HaskellLibraryProvider].prof_lib},
        ),
        haskell_link_info,
        merged,
        HaskellProfLinkInfo(prof_infos = merged),
        merge_shared_libraries(ctx.actions, SharedLibraries(libraries = shared_libraries)),
        library[CPreprocessorInfo],
        create_linkable_graph(
            ctx,
            node = create_linkable_graph_node(
                ctx,
                linkable_node = create_linkable_node(
                    ctx = ctx,
                    default_soname = None,
                    link_infos = link_infos,
                ),
            ),
        ),
    ]

_rts_package_rule = rule(
    impl = _rts_package_impl,
    attrs = {
        "ghc_pkg": attrs.exec_dep(providers = [RunInfo]),
        # GHC's name of the rts's library (hs-libraries).
        "hs_library": attrs.string(),
        "labels": attrs.list(attrs.string(), default = []),
        # The rts library of this configuration.
        "library": attrs.dep(),
        "profiling": attrs.bool(default = False),
        # Way name -> the rts library built for that way.
        "variants": attrs.dict(attrs.string(), attrs.dep()),
        "_cxx_toolchain": toolchains_common.cxx(),
        "_package_db": attrs.source(default = "toolchains//:package_db.py"),
    },
)

# Whether an installation of the libraries (all_ways) has profiled ones:
# `profiled_libraries` in the ghc section of .buckconfig.
PROFILED_LIBRARIES = read_root_config("ghc", "profiled_libraries", "true") == "true"

_STATIC_WAYS = [way for way in RTS_VARIANTS if "dyn" not in _units(way) and "p" not in _units(way)]
_DYNAMIC_WAYS = [way for way in RTS_VARIANTS if "dyn" in _units(way)]
_PROFILED_WAYS = [way for way in RTS_VARIANTS if "p" in _units(way)]

def _package_ways(f):
    # Selects f(ways) for the ways of the RTS's package in the
    # configuration: those of the configuration's libraries (see
    # buck2/haskell.bzl), which a GHC that has them installed links
    # programs with. They are built shared only when executables are linked
    # shared, and otherwise static and shared, with profiled ones in a
    # profiling configuration or an installation of them (all_ways).
    every = _STATIC_WAYS + _DYNAMIC_WAYS
    return select({
        "root//buck2/constraints:all_ways": f(every + (_PROFILED_WAYS if PROFILED_LIBRARIES else [])),
        "DEFAULT": select({
            RTS_WAY_UNITS["p"]: f(every + _PROFILED_WAYS),
            "DEFAULT": select({
                "root//buck2/constraints:static": f(every),
                "root//buck2/constraints:shared": f(_DYNAMIC_WAYS),
                "DEFAULT": select({
                    "root//buck2/constraints:opt": f(every),
                    "DEFAULT": f(_DYNAMIC_WAYS),
                }),
            }),
        }),
    })

def rts_package_ways():
    """Selects the ways of the RTS's package (rts_package()) in the
    configuration, as GHC names them."""
    return _package_ways(lambda ways: ways)

# Every way of the RTS's package in some configuration.
RTS_PACKAGE_WAYS = _STATIC_WAYS + _DYNAMIC_WAYS + _PROFILED_WAYS

def rts_package(name, library, ghc_pkg):
    """The RTS's package (see rts.bzl), of `library` (the rts library's
    label) built for each of its ways in the configuration (see
    _package_ways())."""
    _rts_package_rule(
        name = name,
        ghc_pkg = ghc_pkg,
        hs_library = RTS_LIBRARY,
        library = library,
        profiling = select({RTS_WAY_UNITS["p"]: True, "DEFAULT": False}),
        variants = _package_ways(lambda ways: {way: library + "-" + way for way in ways}),
        visibility = ["PUBLIC"],
    )
