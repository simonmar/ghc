#!/usr/bin/env python3
"""Generates the modules of ghc-boot that its Setup.hs would generate, from
the outputs of configure, as Hadrian does (generateVersionHs and
generatePlatformHostHs in hadrian/src/Rules/Generate.hs).

    ghc_boot.py version SYSTEM_CONFIG OUT
    ghc_boot.py platform-host TARGET OUT

SYSTEM_CONFIG is hadrian/cfg/system.config, TARGET a ghc-toolchain target
file (hadrian/cfg/default{.host,}.target).
"""

import re
import sys


def hs_string(s):
    if not all(" " <= c <= "~" for c in s):
        sys.exit(f"ghc_boot.py: unexpected character in {s!r}")
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


def read_config(path):
    settings = {}
    with open(path) as f:
        for line in f:
            m = re.match(r"^([\w-]+)\s*=\s*(.*?)\s*$", line)
            if m:
                settings[m.group(1)] = m.group(2)
    return settings


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
        if key not in settings:
            sys.exit(f"ghc_boot.py: no {key} in {system_config}")
        lines += ["", f"{name:<21} :: String", f"{name:<21} = {hs_string(settings[key])}"]
    return lines


def platform_host(target):
    with open(target) as f:
        m = re.search(r"tgtArchOs\s*=\s*ArchOS\s*\{\s*archOS_arch\s*=\s*(.*?),\s*archOS_OS\s*=\s*(\w+)\s*\}", f.read(), re.DOTALL)
    if not m:
        sys.exit(f"ghc_boot.py: no tgtArchOs in {target}")
    arch, os = m.groups()
    return [
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
    ]


def main():
    generators = {"version": version, "platform-host": platform_host}
    if len(sys.argv) != 4 or sys.argv[1] not in generators:
        sys.exit(__doc__)
    lines = generators[sys.argv[1]](sys.argv[2])
    with open(sys.argv[3], "w") as f:
        f.write("\n".join(lines) + "\n")


main()
