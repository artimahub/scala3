#!/bin/bash

# Push the cleanup branches that syncem.sh rebased onto current main.
# Uses --force-with-lease so the push fails safely if origin moved
# unexpectedly under us.

F="
scaladoc-tags-sys-concurrent-runtime-cleanup
scaladoc-tags-collection-mutable-cleanup
scaladoc-tags-collection-immutable-cleanup
scaladoc-tags-collection-core-cleanup
scaladoc-tags-math-cleanup
scaladoc-tags-quoted-compiletime-cleanup
scaladoc-tags-jdk-cleanup
scaladoc-tags-util-io-ref-cleanup
scaladoc-tags-root-files-cleanup
"

for i in $F
do
  echo
  echo "==== Pushing branch $i ===="
  echo
  git push --force-with-lease origin "$i"
done
