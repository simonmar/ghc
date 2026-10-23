# The libdir of a GHC that the build makes (GHC's topdir, which it is given
# with -B): its settings and target description, the tools it runs, and its
# global package database. That has the libraries that the GHC uses as
# installed ones, if any: the build otherwise passes GHC the package
# databases of what a target depends on.

load("@prelude//haskell:library_info.bzl", "HaskellLibraryInfoTSet")
load("@prelude//haskell:link_info.bzl", "HaskellLinkInfo")
load("@prelude//haskell:toolchain.bzl", "HaskellPlatformInfo", "HaskellToolchainInfo")
load("@prelude//haskell:util.bzl", "haskell_library_unit_id")
load("@prelude//linking:link_info.bzl", "LinkStrategy", "LinkStyle", "MergedLinkInfo", "get_own_link_infos")
load(":version.bzl", "GHC_VERSION")

# What an installation of the libdir's GHC needs of it: the include dirs of
# its installed rts package (for hsc2hs).
GhcLibdirInfo = provider(fields = {"rts_include_dirs": provider_field(list[Artifact])})

def _hs_string(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'

def _ghc_libdir_impl(ctx: AnalysisContext) -> list[Provider]:
    settings = [
        ("unlit command", "$topdir/bin/unlit"),
        ("Use interpreter", "YES" if ctx.attrs.interpreter else "NO"),
        ("RTS ways", " ".join(ctx.attrs.rts_ways)),
        ("Relative Global Package DB", "package.conf.d"),
        ("base unit-id", haskell_library_unit_id(ctx.attrs.base.raw_target())),
        ("Project Unit Id", haskell_library_unit_id(ctx.attrs.ghc_library.raw_target())),
        ("ghc-internal Unit Id", "ghc-internal"),
    ]
    settings_file = ctx.actions.write(
        "settings",
        "[" + "\n,".join(["({}, {})".format(_hs_string(k), _hs_string(v)) for k, v in settings]) + "\n]\n",
    )
    libdir = ctx.actions.declare_output("lib", dir = True)
    packages, files, rts_include_dirs = _installed_packages(ctx)
    packages += [{"conf": conf} for conf in ctx.attrs.extra_packages]
    manifest = ctx.actions.write_json("packages.json", packages, with_inputs = True)
    hsc2hs_data = [ctx.attrs.hsc2hs_data] if ctx.attrs.hsc2hs_data else []
    script = " && ".join([
        'mkdir -p "$1/targets" "$1/bin"',
        'cp "$2" "$1/settings"',
        'cp "$3" "$1/targets/default.target"',
        'cp "$4" "$1/bin/unlit"',
    ] + ['cp "$5/template-hsc.h" "$1/template-hsc.h"' for _ in hsc2hs_data] + [
        "shift {}".format(4 + len(hsc2hs_data)),
        'python3 "$@"',
    ])
    ctx.actions.run(
        cmd_args(
            "sh",
            "-c",
            script,
            "--",
            libdir.as_output(),
            settings_file,
            ctx.attrs.target,
            ctx.attrs.unlit[RunInfo],
            hsc2hs_data,
            ctx.attrs._package_db,
            ctx.attrs.ghc_pkg[RunInfo],
            cmd_args(libdir.as_output(), format = "{}/package.conf.d"),
            manifest,
            hidden = files,
        ),
        category = "ghc_libdir",
    )

    # What the package database refers to, for what runs the GHC.
    return [
        DefaultInfo(default_output = libdir, other_outputs = files),
        GhcLibdirInfo(rts_include_dirs = rts_include_dirs),
    ]

def _libraries(ctx: AnalysisContext, infos) -> dict:
    # The libraries of `packages`, and those they depend on, by unit id:
    # infos(dep's HaskellLinkInfo) gives each one's tset of them.
    children = filter(None, [infos(dep[HaskellLinkInfo]) for dep in ctx.attrs.packages if dep.get(HaskellLinkInfo)])
    return {lib.id: lib for lib in ctx.actions.tset(HaskellLibraryInfoTSet, children = children).traverse()}

def _installed_packages(ctx: AnalysisContext) -> (list, list, list):
    # The packages installed as GHC's are: each with its library in the ways
    # it was built (static, shared and, if `profiled`, profiled), which GHC
    # chooses from by the way it links (hs-libraries, named as GHC names
    # them), and with what it must be linked with. Returns them
    # (package_db.py's manifest), the files they refer to, and the rts
    # package's include dirs.
    static = _libraries(ctx, lambda info: info.info.get(LinkStyle("static")))
    shared = _libraries(ctx, lambda info: info.info.get(LinkStyle("shared")))
    profiled = _libraries(ctx, lambda info: info.prof_info.get(LinkStyle("static"))) if ctx.attrs.profiled else {}
    linked_with = {}
    for dep in ctx.attrs.packages:
        info = dep.get(HaskellLinkInfo)
        own = get_own_link_infos(dep[MergedLinkInfo], LinkStrategy("static")) if info else None
        if own:
            linked_with[info.info.values()[0].value.id] = own.default.pre_flags
    packages = []
    files = []
    rts_include_dirs = []
    for id in static | shared:
        lib = static.get(id) or shared[id]
        files += lib.include_dirs
        if lib.linked_by_ghc:
            # Installed already (the RTS's package).
            packages.append({"db": lib.db})
            files += lib.libs
            rts_include_dirs = lib.include_dirs
            continue

        # GHC's name for the library, as for a Haskell one (HS...), with the
        # shared library's own name too, its soname. A static build has the
        # shared one's interface files too (-dynamic-too).
        libraries = {}
        import_dirs = [lib.import_dirs[False]]
        if id in static:
            archive = static[id].libs[0]
            name = "HS" + archive.basename.removeprefix("lib").removesuffix(".a")
            libraries["lib{}.a".format(name)] = archive
        if id in shared:
            so = shared[id].libs[0]
            name = "HS" + so.basename.removeprefix("lib").removesuffix(".so")
            libraries[so.basename] = so
            libraries["lib{}-ghc{}.so".format(name, GHC_VERSION)] = so
        if id in profiled:
            libraries["lib{}_p.a".format(name)] = profiled[id].libs[0]
            import_dirs.append(profiled[id].import_dirs[True])
        library_dir = ctx.actions.symlinked_dir("installed/" + id, libraries)
        packages.append({
            "db": lib.db,
            "dynamic_library_dirs": [library_dir],
            "hs_library": name,
            "import_dirs": import_dirs,
            "ld_options": linked_with.get(id, []),
            "library_dirs": [library_dir],
        })
        files += [library_dir] + import_dirs
    return packages, files, rts_include_dirs

ghc_libdir = rule(
    impl = _ghc_libdir_impl,
    attrs = {
        # The libraries whose unit ids GHC needs to know: labels, not
        # dependencies, as they are built by the GHC of this libdir.
        "base": attrs.label(),
        "ghc_library": attrs.label(),
        # The package files of packages that a GHC has installed that the
        # build doesn't make (system-cxx-std-lib).
        "extra_packages": attrs.list(attrs.source(), default = []),
        "ghc_pkg": attrs.exec_dep(providers = [RunInfo]),
        # The data files of the hsc2hs installed with the GHC: its
        # template-hsc.h, which is in the libdir.
        "hsc2hs_data": attrs.option(attrs.source(allow_directory = True), default = None),
        "interpreter": attrs.bool(default = False),
        # The libraries of the global package database: the GHC's
        # installed ones, built by the GHC before it (the stage1 libraries,
        # for the stage2 GHC).
        "packages": attrs.list(attrs.dep(), default = []),
        # Whether the packages are installed with their profiled libraries.
        "profiled": attrs.bool(default = False),
        # The ways of the RTS that what the GHC builds is linked with, which
        # GHC (linking its external interpreter) and Cabal (which ways a
        # compiler supports) read.
        "rts_ways": attrs.list(attrs.string()),
        "target": attrs.source(),
        # The programs that GHC runs, which run where it does.
        "unlit": attrs.exec_dep(providers = [RunInfo]),
        "_package_db": attrs.source(default = "toolchains//:package_db.py"),
    },
)

def _with_libdir_impl(ctx: AnalysisContext) -> list[Provider]:
    libdir = ctx.attrs.libdir[DefaultInfo]
    return [
        DefaultInfo(
            default_outputs = ctx.attrs.tool[DefaultInfo].default_outputs,
            other_outputs = libdir.default_outputs + libdir.other_outputs,
        ),
        RunInfo(args = cmd_args(
            ctx.attrs.tool[RunInfo],
            cmd_args(ctx.attrs.libdir[DefaultInfo].default_outputs[0], format = ctx.attrs.flag),
            hidden = ctx.attrs.libdir[DefaultInfo].other_outputs,
        )),
    ]

# A tool of a GHC that the build makes (ghc, ghc-pkg), run with the GHC's
# libdir, which `flag` (a format string) gives it: as the toolchain runs it,
# for exec_with_libdir (the tool is the one the build uses, for the execution
# platform, and the libdir the one for what it builds), or as a product of the
# build, for with_libdir (the tool built as the libdir's libraries are, for
# the target platform).
with_libdir = rule(
    impl = _with_libdir_impl,
    attrs = {
        "flag": attrs.string(),
        "libdir": attrs.dep(),
        "tool": attrs.dep(providers = [RunInfo]),
    },
)

exec_with_libdir = rule(
    impl = _with_libdir_impl,
    attrs = {
        "flag": attrs.string(),
        "libdir": attrs.dep(),
        "tool": attrs.exec_dep(providers = [RunInfo]),
    },
)

def _ghc_toolchain_impl(ctx: AnalysisContext) -> list[Provider]:
    toolchain = ctx.attrs.toolchain
    if not ctx.attrs.libdir:
        return toolchain.providers
    info = toolchain[HaskellToolchainInfo]
    libdir = ctx.attrs.libdir[DefaultInfo]
    files = libdir.other_outputs
    fields = {field: getattr(info, field) for field in dir(info)}
    fields["compiler"] = cmd_args(info.compiler, cmd_args(libdir.default_outputs[0], format = "-B{}"), hidden = files)
    fields["linker"] = cmd_args(info.linker, cmd_args(libdir.default_outputs[0], format = "-B{}"), hidden = files)
    fields["packager"] = cmd_args(info.packager, cmd_args(libdir.default_outputs[0], format = "--global-package-db={}/package.conf.d"), hidden = files)
    return [
        DefaultInfo(),
        HaskellToolchainInfo(**fields),
        toolchain[HaskellPlatformInfo],
    ]

# A Haskell toolchain whose GHC (and ghc-pkg) is run with a libdir (its
# topdir, -B): a GHC that the build makes. The libdir is for the targets the
# GHC builds (its package database has the libraries it links them with), so
# it is configured as they are, unlike the GHC itself, which runs on the
# execution platform.
ghc_toolchain = rule(
    impl = _ghc_toolchain_impl,
    attrs = {
        "libdir": attrs.option(attrs.dep(), default = None),
        "toolchain": attrs.toolchain_dep(providers = [HaskellToolchainInfo]),
    },
    is_toolchain_rule = True,
)
