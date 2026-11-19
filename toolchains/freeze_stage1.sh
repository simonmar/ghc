#!/bin/sh
# Usage: freeze_stage1.sh DIR TOOL=PROGRAM...
#
# Copies each PROGRAM to DIR/TOOL, where DIR is relative to the directory of
# this script. The copies replace the whole of DIR at once, so that a copy
# that fails part way leaves the previous snapshot as it was.
set -eu

dir="$(dirname "$0")/$1"
shift
new="$dir.new"
old="$dir.old"

rm -rf "$new" "$old"
mkdir -p "$new"
for arg in "$@"; do
    cp "${arg#*=}" "$new/${arg%%=*}"
done
if [ -e "$dir" ]; then
    mv "$dir" "$old"
fi
mv "$new" "$dir"
rm -rf "$old"

echo "Froze stage1 in $dir."
echo "Set \`freeze1 = true\` in the [ghc] section of .buckconfig.local to use it."
