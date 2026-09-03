# Scaladoc drops documentation for `protected` members based on whether a companion object exists

## Compiler version

`main` @ 804dd160a1 (2026-08-24). Present since the visibility handling was
introduced; not specific to a recent change.

## Summary

Scaladoc omits `protected` members from generated documentation in two cases
where the member is in fact reachable by a subclass author:

1. **Bare `protected` in a class that happens to have a companion object.**
   Whether the doc is emitted depends on the existence of an unrelated
   companion object, which has no bearing on visibility. Two otherwise
   identical classes are documented differently.

2. **`protected[X]` where `X` is a package or object.** The qualifier on
   `protected` *widens* access rather than narrowing it, so these members
   remain reachable by every subclass, including subclasses outside `X`.

The first is an implementation bug: the code contradicts the documented intent
of its own data type. The second is a policy question, but the current answer
disagrees with the specification.

## Minimal reproduction

```scala
package demo

class WithCompanion:
  /** This documentation is dropped. */
  protected def hook(): Unit = ()

object WithCompanion   // merely existing changes the outcome

class WithoutCompanion:
  /** This documentation is rendered. */
  protected def hook(): Unit = ()

class Widget:
  /** This documentation is dropped. */
  protected[demo] def configure(): Unit = ()
```

### Expectation

All three members are documented. Each is reachable from a subclass defined
anywhere:

```scala
package elsewhere
import demo.*

class MyWidget extends Widget:
  def go(): Unit = configure()          // legal: protected access is retained
```

### Actual

`WithCompanion.hook` and `Widget.configure` are absent from the generated
documentation. `WithoutCompanion.hook` is present. Deleting `object
WithCompanion` makes the first one appear.

(Derived by reading the sources cited below rather than by running scaladoc;
worth confirming with an actual run before acting.)

## Where it comes from

`scaladoc/src/dotty/tools/scaladoc/tasty/SymOps.scala:126`

```scala
def isHiddenByVisibility(using dctx: DocContext): Boolean =
  import VisibilityScope._

  !summon[DocContext].args.includePrivateAPI && sym.getVisibility().match
    case Visibility.Private(_) => true
    case Visibility.Protected(ThisScope | ImplicitModuleScope | _: ExplicitModuleScope) => true
    case _ => false
```

The scopes come from `getVisibility` at `SymOps.scala:75`:

```scala
def explicitScope(ownerType: TypeRepr): VisibilityScope =
  val moduleSym = ownerType.typeSymbol.companionModule
  if moduleSym.isNoSymbol
    then ExplicitTypeScope(ownerType.typeSymbol.name)
    else ExplicitModuleScope(moduleSym.name)

def implicitScope(ownerSym: Symbol): VisibilityScope =
  val moduleSym = ownerSym.companionModule
  if moduleSym.isNoSymbol
    then ImplicitTypeScope
    else ImplicitModuleScope
```

Compare against how `VisibilityScope` documents itself, in
`scaladoc/src/dotty/tools/scaladoc/api.scala:23`:

```scala
enum VisibilityScope:
  case ImplicitTypeScope // private/protected inside a class or a trait
  case ImplicitModuleScope // private/protected inside a package or an object
  case ExplicitTypeScope(typeName: String) // private[X]/protected[X] inside a class or a trait
  case ExplicitModuleScope(moduleName: String) // private[X]/protected[X] inside a package or an object
```

The intent is a question about **what kind of entity the scope is** - a class
or trait, versus a package or object. The implementation instead asks whether
that entity **has a companion module**. Those coincide only by accident. A
`class` with a companion object is not "inside a package or an object", yet
`implicitScope` labels its members `ImplicitModuleScope`, and
`isHiddenByVisibility` then drops them.

## Why hiding `protected[X]` is also wrong

Independently of the misclassification, `Protected(_: ExplicitModuleScope)`
should not be hidden. From SLS 5.2.2 (`docs/_spec/05-classes-and-objects.md`):

> A `protected` modifier can be qualified with an identifier `C` (e.g.
> `protected[C]`) that must denote a class or package enclosing the
> definition. Members labeled with such a modifier are **also** accessible
> respectively from all code inside the package `C` or from all code inside
> the class `C` and its companion module.

"Also" is load-bearing. The qualifier runs in opposite directions for the two
modifiers: it narrows `private`, but widens `protected`. `protected[C]` is
ordinary protected access - available to every subclass, anywhere - plus
access from within `C`. So a `protected[somePackage]` member of a public class
is part of the API a subclass author programs against, and hiding it removes
documentation that a user needs in order to override or call it.

The same holds for `protected[this]`, which the spec describes as adding a
restriction on the prefix while "the restrictions for unqualified `protected`
apply" - a subclass still sees its own inherited member.

## Impact

This affects the standard library's own documentation. Members currently
undocumented for this reason include, among others:

| declaration | file |
|---|---|
| `protected[collection] var array` | `collection/mutable/ArrayBuffer.scala` |
| `protected[collection] final def nwords`, `word`, `fromBitMaskNoCopy` | `collection/mutable/BitSet.scala` |
| `protected[collection] def occCounts` in `trait SeqOps` | `collection/Seq.scala` |
| `protected[collection] def filterImpl` in `trait StrictOptimizedIterableOps` | `collection/StrictOptimizedIterableOps.scala` |
| `protected[matching] val matcher` in `class Match` | `util/matching/Regex.scala` |
| `protected[Regex] val matcher` in `class MatchIterator` | `util/matching/Regex.scala` |

Each is declared in a public, non-final, non-sealed type with a public
constructor, so an external subclass can reach it.

## Suggested fix

Two separable changes:

1. **Classify the scope by what it is.** In `implicitScope`, test whether
   `ownerSym` is itself a package or module (e.g. via `Flags.Module` /
   `isPackageDef`) rather than whether it has a companion; likewise for the
   qualifier in `explicitScope`. This alone makes the outcome independent of
   companion objects and matches the enum's documented meaning.

2. **Stop hiding qualified `protected`.** Narrow the second case of
   `isHiddenByVisibility` to `Visibility.Private(_)` plus, at most,
   `Protected(ThisScope)`, so that `protected` in all its forms is documented
   by default. If retaining a distinction is desired, a separate flag would be
   more appropriate than folding it into `includePrivateAPI`, since these
   members are not private.

Change 1 is the clear defect and is safe on its own. Change 2 alters what
appears in published documentation and is worth a maintainer's judgement.
