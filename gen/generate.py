#!/usr/bin/env python3
"""Generates sources of GHC's packages from the outputs of configure, as
Hadrian does (hadrian/src/Rules/Generate.hs).

    generate.py version SYSTEM_CONFIG OUT
    generate.py platform-host TARGET OUT
    generate.py settings-config SYSTEM_CONFIG STAGE UNIT_ID OUT
    generate.py primops-txt TARGET PRIMOPS_TXT_PP OUT
    generate.py derived-constants TARGET SYSTEM_CONFIG STAGE DERIVE_CONSTANTS RTS GENERATED_INCLUDE OUT

SYSTEM_CONFIG is hadrian/cfg/system.config, TARGET a ghc-toolchain target
file (hadrian/cfg/default{.host,}.target). STAGE is the stage that builds the
compiler, 0 for the bootstrap compiler; UNIT_ID is the unit id of the ghc
library. RTS is the rts's source directory, GENERATED_INCLUDE where its
configure script put the headers it generated.
"""

import ast
import inspect
import re
import os
import subprocess
import sys
import tempfile


def fail(message):
    sys.exit(f"generate.py: {message}")


def hs_string(s):
    if not all(" " <= c <= "~" for c in s):
        fail(f"unexpected character in {s!r}")
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


def read_config(path):
    settings = {}
    with open(path) as f:
        for line in f:
            m = re.match(r"^([\w-]+)\s*=\s*(.*?)\s*$", line)
            if m:
                settings[m.group(1)] = m.group(2)
    return settings


def setting(settings, key, path):
    if key not in settings:
        fail(f"no {key} in {path}")
    return settings[key]


def module(lines):
    return "\n".join(lines) + "\n"


def target_field(target, pattern):
    with open(target) as f:
        m = re.search(pattern, f.read(), re.DOTALL)
    if not m:
        fail(f"{pattern} not found in {target}")
    return m.groups()


# GHC.Version of ghc-boot (generateVersionHs).
def version(system_config):
    settings = read_config(system_config)
    lines = ["module GHC.Version where", "", "import Prelude -- See Note [Why do we import Prelude here?]"]
    for name, key in [
        ("cProjectGitCommitId", "project-git-commit-id"),
        ("cProjectVersion", "project-version"),
        ("cProjectVersionInt", "project-version-int"),
        ("cProjectPatchLevel", "project-patch-level"),
        ("cProjectPatchLevel1", "project-patch-level1"),
        ("cProjectPatchLevel2", "project-patch-level2"),
    ]:
        lines += ["", f"{name:<21} :: String", f"{name:<21} = {hs_string(setting(settings, key, system_config))}"]
    return module(lines)


# GHC.Platform.Host of ghc-boot (generatePlatformHostHs).
def platform_host(target):
    arch, os = target_field(target, r"tgtArchOs\s*=\s*ArchOS\s*\{\s*archOS_arch\s*=\s*(.*?),\s*archOS_OS\s*=\s*(\w+)\s*\}")
    return module([
        "module GHC.Platform.Host where",
        "",
        "import GHC.Platform.ArchOS",
        "",
        "hostPlatformArch :: Arch",
        f"hostPlatformArch = {arch}",
        "",
        "hostPlatformOS   :: OS",
        f"hostPlatformOS   = {os}",
        "",
        "hostPlatformArchOS :: ArchOS",
        "hostPlatformArchOS = ArchOS hostPlatformArch hostPlatformOS",
    ])


# GHC.Settings.Config of ghc (generateConfigHs). The compiler built at stage0
# is built on the build platform and runs on the host; one built at a later
# stage is built on the host and runs on the target.
def settings_config(system_config, stage, unit_id):
    settings = read_config(system_config)
    stage = int(stage)
    build_key, host_key = ("build-platform-full", "host-platform-full") if stage == 0 else ("host-platform-full", "target-platform-full")
    def value(key):
        return hs_string(setting(settings, key, system_config))
    return module([
        "module GHC.Settings.Config",
        "  ( module GHC.Version",
        "  , cBuildPlatformString",
        "  , cHostPlatformString",
        "  , cProjectName",
        "  , cBooterVersion",
        "  , cStage",
        "  , cProjectUnitId",
        "  ) where",
        "",
        "import GHC.Prelude.Basic",
        "",
        "import GHC.Version",
        "",
        "cBuildPlatformString :: String",
        f"cBuildPlatformString = {value(build_key)}",
        "",
        "cHostPlatformString :: String",
        f"cHostPlatformString = {value(host_key)}",
        "",
        "cProjectName          :: String",
        f"cProjectName          = {value('project-name')}",
        "",
        "cBooterVersion        :: String",
        f"cBooterVersion        = {value('ghc-version')}",
        "",
        "cStage                :: String",
        f"cStage                = show ({stage + 1} :: Int)",
        "",
        "cProjectUnitId :: String",
        f"cProjectUnitId = {hs_string(unit_id)}",
    ])


# primops.txt, from compiler/GHC/Builtin/primops.txt.pp, preprocessed by the
# target's Haskell C preprocessor (see hsCppBuilderArgs).
def primops_txt(target, primops_txt_pp):
    program, flags = target_field(target, r"tgtHsCPreprocessor\s*=\s*HsCpp\s*\{\s*hsCppProgram\s*=\s*Program\s*\{\s*prgPath\s*=\s*(\".*?\"),\s*prgFlags\s*=\s*(\[.*?\])\s*\}")
    command = [ast.literal_eval(program)] + ast.literal_eval(flags) + ["-P", "-x", "c", primops_txt_pp]
    result = subprocess.run(command, capture_output=True, text=True)
    if result.returncode != 0:
        fail(f"{' '.join(command)} failed:\n{result.stderr}")
    return result.stdout


# DerivedConstants.h of the rts, by deriveConstants, which compiles C with the
# target's C compiler against the rts's headers (see
# deriveConstantsBuilderArgs).
def derived_constants(target, system_config, stage, derive_constants, rts, generated_include):
    cc, cc_flags = target_field(target, r"tgtCCompiler\s*=\s*Cc\s*\{\s*ccProgram\s*=\s*Program\s*\{\s*prgPath\s*=\s*(\".*?\"),\s*prgFlags\s*=\s*(\[.*?\])\s*\}")
    (nm,) = target_field(target, r"tgtNm\s*=\s*Nm\s*\{\s*nmProgram\s*=\s*Program\s*\{\s*prgPath\s*=\s*(\".*?\")")
    # The OS is the last part of the platform's triple (arch-vendor-os).
    platform = setting(read_config(system_config), "host-platform-full" if int(stage) == 0 else "target-platform-full", system_config)
    flags = ast.literal_eval(cc_flags) + ["-I" + rts, "-I" + os.path.join(rts, "include"), "-I" + generated_include, "-fcommon"]
    with tempfile.TemporaryDirectory() as tmp:
        out = os.path.join(tmp, "DerivedConstants.h")
        command = [derive_constants, "--gen-header", "-o", out, "--tmpdir", tmp, "--gcc-program", ast.literal_eval(cc)]
        command += [arg for flag in flags for arg in ["--gcc-flag", flag]]
        command += ["--nm-program", ast.literal_eval(nm), "--target-os", platform.split("-")[-1]]
        result = subprocess.run(command, capture_output=True, text=True)
        if result.returncode != 0:
            fail(f"{' '.join(command)} failed:\n{result.stderr}")
        with open(out) as f:
            return f.read()


GENERATORS = {
    "version": version,
    "platform-host": platform_host,
    "settings-config": settings_config,
    "primops-txt": primops_txt,
    "derived-constants": derived_constants,
}


def main():
    generator = GENERATORS.get(sys.argv[1]) if len(sys.argv) > 1 else None
    if generator is None or len(sys.argv) != len(inspect.signature(generator).parameters) + 3:
        sys.exit(__doc__)
    *args, out = sys.argv[2:]
    text = generator(*args)
    with open(out, "w") as f:
        f.write(text)


main()
