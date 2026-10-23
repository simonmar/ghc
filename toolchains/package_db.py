#!/usr/bin/env python3
"""Make a package database of the given packages.

    package_db.py GHC_PKG OUT MANIFEST

MANIFEST is a JSON list of packages, each with "conf", its .conf file, or
"db", the package database of the package's own .conf file, and optionally:

  - "hs_library": the package's library is one that GHC installs, instead of
    its own: GHC names its files by way (lib<name>.a, lib<name>_p.a,
    lib<name>-ghc<version>.so), in "library_dirs" and
    "dynamic_library_dirs";
  - "import_dirs": where its interface files are, instead of its own;
  - "ld_options": what what links it is linked with.

Each package goes into OUT (a new package database), with its paths relative
to the ${pkgroot} of OUT (its directory) rather than of its own database.
GHC_PKG builds OUT's cache.
"""

import json
import os
import re
import subprocess
import sys

PKGROOT = re.compile(r"\$\{pkgroot\}((?:/[^\s,\"]*)?)")

# The fields of a package's library, which a library that GHC installs has
# instead.
LIBRARY_FIELDS = ["library-dirs", "dynamic-library-dirs", "extra-libraries", "hs-libraries"]


def relocate(text, src_root, dst_root):
    def path(m):
        target = os.path.normpath(os.path.join(src_root, m.group(1).lstrip("/")))
        return "${pkgroot}/" + os.path.relpath(target, dst_root)

    return PKGROOT.sub(path, text)


def without_fields(text, fields):
    """A package file without some of its fields (each a line "name: ..." and
    the indented lines that continue it)."""
    lines, skipping = [], False
    for line in text.splitlines():
        if line and not line[0].isspace():
            skipping = line.split(":", 1)[0] in fields
        if not skipping:
            lines.append(line)
    return "\n".join(lines) + "\n"


def main():
    ghc_pkg, out, manifest = sys.argv[1:4]
    os.makedirs(out)
    dst_root = os.path.dirname(os.path.normpath(out))

    def paths(dirs):
        return " ".join("${pkgroot}/" + os.path.relpath(d, dst_root) for d in dirs)

    with open(manifest) as f:
        packages = json.load(f)
    for package in packages:
        if "conf" in package:
            confs = [package["conf"]]
        else:
            db = package["db"]
            confs = [os.path.join(db, name) for name in sorted(os.listdir(db)) if name.endswith(".conf")]
        for conf in confs:
            name = os.path.basename(conf)
            with open(conf) as f:
                text = relocate(f.read(), os.path.dirname(os.path.dirname(os.path.normpath(conf))), dst_root)
            fields = {}
            if "hs_library" in package:
                fields["hs-libraries"] = package["hs_library"]
                fields["library-dirs"] = paths(package["library_dirs"])
                fields["dynamic-library-dirs"] = paths(package["dynamic_library_dirs"])
                text = without_fields(text, LIBRARY_FIELDS)
            if "import_dirs" in package:
                fields["import-dirs"] = paths(package["import_dirs"])
            if package.get("ld_options"):
                fields["ld-options"] = " ".join(json.dumps(o) for o in package["ld_options"])
            text = without_fields(text, list(fields))
            text += "".join("{}: {}\n".format(k, v) for k, v in fields.items())
            with open(os.path.join(out, name), "w") as f:
                f.write(text)
    subprocess.run([ghc_pkg, "--global-package-db=" + out, "recache", "--global"], check=True)


if __name__ == "__main__":
    main()
