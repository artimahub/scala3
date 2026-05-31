#!/bin/bash

F_OLD="
scaladoc-tags-sys-concurrent-runtime
scaladoc-tags-collection-mutable
scaladoc-tags-collection-immutable
scaladoc-tags-collection-core
scaladoc-tags-numeric-types
scaladoc-tags-array-predef
scaladoc-tags-math
scaladoc-tags-quoted-compiletime
scaladoc-tags-jdk
scaladoc-tags-util-io-ref
scaladoc-tags-annotation-reflect-misc
scaladoc-tags-function-tuple-product
"

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
  echo "==== Working on branch $i ===="
  echo
  "$(dirname "$0")/sync-branch.sh" $i
  git switch delme-soon-branch
done

