# Doc review digest: library/src/scala/collection/generic/IsMap.scala

- models: writer devstral-latest | accuracy sonnet | style mistral-large-latest | adjudicator devstral-latest
- converged: false (up to 3 rounds)
- final refinement after review limit: true
- verification review of that refine: approve
- a real accuracy review ran at some point: true
- the file AS IT NOW STANDS was accuracy-reviewed: true
- accuracy verdict: approve
- style verdict: approve
- ADJUDICATOR verdict (final): approve

## Reviewer disagreements the adjudicator settled

- L49 `IsMap.apply` -> ruled for **accuracy**
  - accuracy: The @return tag was dropped, but the description does not literally begin with 'Returns' as the drop rule requires.
  - style: The @return tag was correctly removed from `apply` in `IsMap` trait, as the description already states the return value.
  - why: The house rule requires the description to begin with 'Returns' to drop the @return tag, which it does not.

## Outstanding worklist at the end

- L49 `IsMap.apply` [nit/accuracy]: Either reword the opening to 'Returns a conversion from `Repr` to `MapOps[K, V, Iterable, C]`' to satisfy the drop rule, or keep a short @return restating the 'view of the collection' phrasing.
- L85 `IsMap.mapViewIsMap` [nit/accuracy]: Write '... which must be a subtype of `MapView[X, Y]`' to match the bound exactly.
- L49 `IsMap.apply` [nit/style]: Change the note to: "@note The third type parameter of the returned `MapOps` value is still `Iterable` (not `Map`) because `MapView[K, V]` only extends `MapOps[K, V, View, View[A]]`."
- L70 `mapOpsIsMap` [nit/style]: Simplify to: "@tparam CC0 the collection type constructor, a subtype of `MapOps[X, Y, Tupled[Iterable]#Ap, CC0[X, Y]]`."
- L100 `anyRefMapIsMap` [nit/style]: Add a note: "@note This instance is deprecated because `AnyRefMap` is deprecated."

## Inline NEEDS-HUMAN markers left in source
(none)
