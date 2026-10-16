#!/usr/bin/env python3
"""Make a package database of the packages of others.

    package_db.py GHC_PKG OUT DB...

Puts the package of each .conf file in each DB into OUT (a new package
database), with its paths relative to the ${pkgroot} of OUT (its directory)
rather than of the DB it comes from, and has GHC_PKG build OUT's cache.
"""

import os
import re
import subprocess
import sys

PKGROOT = re.compile(r"\$\{pkgroot\}((?:/[^\s,\"]*)?)")


def relocate(text, src_root, dst_root):
    def path(m):
        target = os.path.normpath(os.path.join(src_root, m.group(1).lstrip("/")))
        return "${pkgroot}/" + os.path.relpath(target, dst_root)

    return PKGROOT.sub(path, text)


def main():
    ghc_pkg, out, dbs = sys.argv[1], sys.argv[2], sys.argv[3:]
    os.makedirs(out)
    dst_root = os.path.dirname(os.path.normpath(out))
    for db in dbs:
        src_root = os.path.dirname(os.path.normpath(db))
        for name in sorted(os.listdir(db)):
            if not name.endswith(".conf"):
                continue
            with open(os.path.join(db, name)) as f:
                text = relocate(f.read(), src_root, dst_root)
            with open(os.path.join(out, name), "w") as f:
                f.write(text)
    subprocess.run([ghc_pkg, "--global-package-db=" + out, "recache", "--global"], check=True)


if __name__ == "__main__":
    main()
