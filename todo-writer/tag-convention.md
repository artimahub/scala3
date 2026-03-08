# Scaladoc Tag Ordering Convention

This document specifies how `todo-writer` handles the ordering of `@` tags in Scaladoc comments.

## Background

The Scala standard library and official Scaladoc documentation follow a conventional tag ordering where parameter and return documentation appears together, followed by notes and cross-references. See:
- https://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html
- https://docs.scala-lang.org/style/scaladoc.html

## Specification

### Tags Subject to Reordering

The following tags are **signature tags** and will be reordered relative to each other to maintain a consistent order:

1. `@tparam` - type parameter documentation
2. `@param` - value parameter documentation
3. `@return` - return value documentation
4. `@throws` - exception documentation

When multiple signature tags are present, they will be sorted into the order shown above:
- All `@tparam` tags first (preserving their relative order)
- All `@param` tags second (preserving their relative order)
- All `@return` tags third (there should only be one)
- All `@throws` tags last (preserving their relative order)

### Tags NOT Subject to Reordering

All other tags remain in their original position relative to non-signature content. This includes but is not limited to:

- `@note`
- `@see`
- `@example`
- `@author`
- `@version`
- `@since`
- `@group`
- `@define`

These tags will not be moved. If a user writes `@see` before `@param`, the `@see` stays before `@param`. If they write `@note` after `@return`, it stays after `@return`.

### Insertion of Missing Tags

When `todo-writer` inserts missing signature tags (with `TODO FILL IN` placeholders), they are inserted:

1. Adjacent to existing signature tags if any exist, maintaining the canonical order
2. If no signature tags exist, inserted at the end of the Scaladoc comment (before the closing `*/`)

### Examples

#### Example 1: Reordering signature tags

Input:
```scala
/** Does something.
 *  @return the result
 *  @param x the input
 *  @tparam A the type
 */
```

Output:
```scala
/** Does something.
 *  @tparam A the type
 *  @param x the input
 *  @return the result
 */
```

#### Example 2: Non-signature tags stay in place

Input:
```scala
/** Does something.
 *  @note This is important.
 *  @param x the input
 *  @see [[Other]]
 *  @return the result
 */
```

Output (only @param/@return order is enforced):
```scala
/** Does something.
 *  @note This is important.
 *  @param x the input
 *  @see [[Other]]
 *  @return the result
 */
```

The `@note` stays at the top and `@see` stays between `@param` and `@return` because non-signature tags are not moved.

#### Example 3: Inserting missing tags

Input:
```scala
/** Does something.
 *  @note Be careful.
 */
def foo[A](x: Int): String
```

Output (missing tags inserted at end):
```scala
/** Does something.
 *  @note Be careful.
 *  @tparam A TODO FILL IN
 *  @param x TODO FILL IN
 *  @return TODO FILL IN
 */
def foo[A](x: Int): String
```

#### Example 4: Inserting near existing signature tags

Input:
```scala
/** Does something.
 *  @note Be careful.
 *  @param x the input
 *  @see [[Other]]
 */
def foo[A](x: Int): String
```

Output (missing @tparam inserted before @param, missing @return inserted after @param):
```scala
/** Does something.
 *  @note Be careful.
 *  @tparam A TODO FILL IN
 *  @param x the input
 *  @return TODO FILL IN
 *  @see [[Other]]
 */
def foo[A](x: Int): String
```

## Rationale

This approach balances consistency with minimal disruption:

1. **Signature tags are grouped and ordered** because they form a logical unit documenting the method's type signature, and consistent ordering improves readability.

2. **Other tags are left in place** because authors may have intentional reasons for their placement (e.g., a `@note` about a parameter placed right after that `@param`, or an `@example` placed early for prominence).

3. **Follows standard library convention** for the relative ordering of `@tparam`, `@param`, `@return`, and `@throws`.
