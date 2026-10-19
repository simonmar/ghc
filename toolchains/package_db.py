#!/usr/bin/env python3
"""Make a package database of the packages of others.

    package_db.py merge GHC_PKG OUT DB...

Puts the package of each .conf file in each DB into OUT (a new package
database), with its paths relative to the ${pkgroot} of OUT (its directory)
rather than of the DB it comes from.

    package_db.py linked-by-ghc GHC_PKG OUT DB LIBDIR HS_LIBRARY

Puts the package in DB into OUT likewise, as one whose library GHC links
itself: its hs-libraries are HS_LIBRARY, in LIBDIR (for each way, named as
GHC names them), instead of its own library.

Either way, GHC_PKG builds OUT's cache.
"""

import os
import re
import subprocess
import sys

PKGROOT = re.compile(r"\$\{pkgroot\}((?:/[^\s,\"]*)?)")

# The fields of a package's library, which a package that GHC links has
# instead as hs-libraries in its own library-dirs.
LIBRARY_FIELDS = ["library-dirs", "dynamic-library-dirs", "extra-libraries", "hs-libraries"]


def relocate(text, src_root, dst_root):
    def path(m):
        target = os.path.normpath(os.path.join(src_root, m.group(1).lstrip("/")))
        return "${pkgroot}/" + os.path.relpath(target, dst_root)

    return PKGROOT.sub(path, text)


def confs(db):
    """The package files of a database, and its ${pkgroot}."""
    root = os.path.dirname(os.path.normpath(db))
    for name in sorted(os.listdir(db)):
        if name.endswith(".conf"):
            with open(os.path.join(db, name)) as f:
                yield name, f.read(), root


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
    mode, ghc_pkg, out = sys.argv[1], sys.argv[2], sys.argv[3]
    os.makedirs(out)
    dst_root = os.path.dirname(os.path.normpath(out))
    if mode == "merge":
        for db in sys.argv[4:]:
            for name, text, root in confs(db):
                with open(os.path.join(out, name), "w") as f:
                    f.write(relocate(text, root, dst_root))
    elif mode == "linked-by-ghc":
        db, libdir, hs_library = sys.argv[4:7]
        libdir = "${pkgroot}/" + os.path.relpath(libdir, dst_root)
        for name, text, root in confs(db):
            text = without_fields(relocate(text, root, dst_root), LIBRARY_FIELDS)
            text += "hs-libraries: {}\nlibrary-dirs: {}\ndynamic-library-dirs: {}\n".format(hs_library, libdir, libdir)
            with open(os.path.join(out, name), "w") as f:
                f.write(text)
    else:
        sys.exit("package_db.py: unknown mode " + mode)
    subprocess.run([ghc_pkg, "--global-package-db=" + out, "recache", "--global"], check=True)


if __name__ == "__main__":
    main()
