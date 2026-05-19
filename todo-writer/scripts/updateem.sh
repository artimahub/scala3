E="
scaladoc-tags-sys-concurrent-runtime
"
F="
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

for i in $F
do
  git switch $i
  git merge origin/$i
  git switch delme-soon-branch
done
