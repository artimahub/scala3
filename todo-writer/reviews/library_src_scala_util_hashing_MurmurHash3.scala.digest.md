# Doc review digest: library/src/scala/util/hashing/MurmurHash3.scala

- models: writer devstral-2512 | accuracy mistral-medium-2508 | style mistral-large-2512 | adjudicator devstral-2512
- converged: false (up to 2 rounds)
- final refinement after review limit: true (not re-reviewed)
- accuracy verdict: approve
- style verdict: revise
- ADJUDICATOR verdict (final): revise

## Reviewer disagreements the adjudicator settled

- L88 `productHash` -> ruled for **style**
  - accuracy: 
  - style: The documentation does not accurately describe the behavior of the method. The method's implementation mixes in the product prefix only if `ignorePrefix` is false, but the documentation states 'derived from the product prefix (if not ignored)' without clarifying that the prefix is mixed into the seed, not just conditionally included. Additionally, the method's behavior for case classes compiled before 2.13.17 (where `ignorePrefix` is false) is not mentioned, which is a material detail for callers.
  - why: The style reviewer's concern about the accuracy of the description is valid and material to the API.
- L430 `arrayHash` -> ruled for **style**
  - accuracy: 
  - style: The documentation does not mention that the method delegates to `arrayHash(a, arraySeed)`, which is a material detail for callers who might expect a different seed or behavior. The `@return` tag is also redundant with the description.
  - why: The delegation detail is material and the `@return` tag is redundant.
- L435 `bytesHash` -> ruled for **style**
  - accuracy: 
  - style: The documentation does not mention that the method delegates to `bytesHash(data, arraySeed)`, which is a material detail for callers. The `@return` tag is redundant with the description.
  - why: The delegation detail is material and the `@return` tag is redundant.
- L450 `unorderedHash` -> ruled for **style**
  - accuracy: 
  - style: The description uses 'order-independent' but does not clarify that this means the same elements in any order produce the same hash. This is a material detail for callers.
  - why: The clarification of 'order-independent' is material for callers.
- L455 `rangeHash` -> ruled for **style**
  - accuracy: 
  - style: The documentation does not mention that the method delegates to `rangeHash(start, step, last, seqSeed)`, which is a material detail for callers. The `@return` tag is also redundant with the description.
  - why: The delegation detail is material and the `@return` tag is redundant.
- L465 `productHash (deprecated)` -> ruled for **style**
  - accuracy: 
  - style: The documentation does not mention that this method is deprecated in favor of `caseClassHash` or that it delegates to `caseClassHash(x, productSeed, null)`. This is a material detail for callers who might not notice the `@deprecated` annotation.
  - why: The deprecation and delegation details are material for callers.
- L545 `apply (in accum)` -> ruled for **style**
  - accuracy: 
  - style: The documentation does not describe the side effects of the method (updating `a`, `b`, `n`, and `c`), which are material to its behavior. The description is also vague ('Processes a map entry').
  - why: The side effects are material to the method's behavior.
- L565 `setHash` -> ruled for **style**
  - accuracy: 
  - style: The documentation does not mention that the method delegates to `unorderedHash(xs, setSeed)`, which is a material detail for callers. The `@return` tag is redundant with the description.
  - why: The delegation detail is material and the `@return` tag is redundant.
- L575 `ArrayHashing.hash` -> ruled for **style**
  - accuracy: 
  - style: The documentation does not mention that the method delegates to `arrayHash(a)`, which is a material detail for callers. The description is also too terse ('Computes the hash of an array').
  - why: The delegation detail is material for callers.
- L530 `mapHash` -> ruled for **style**
  - accuracy: 
  - style: The documentation does not describe the edge case for empty maps (delegating to `emptyMapHash`) or the accumulation logic for non-empty maps. This is a material omission for callers.
  - why: The edge case and accumulation logic are material for callers.

## Outstanding worklist at the end

- L88 `productHash` [blocker/style]: Revise the documentation to clarify that the product prefix is mixed into the seed when `ignorePrefix` is false, and explicitly mention the behavior for case classes compiled before 2.13.17.
- L430 `arrayHash` [blocker/style]: Revise the documentation to mention that the method delegates to `arrayHash(a, arraySeed)` and remove the redundant `@return` tag.
- L435 `bytesHash` [blocker/style]: Revise the documentation to mention that the method delegates to `bytesHash(data, arraySeed)` and remove the redundant `@return` tag.
- L450 `unorderedHash` [blocker/style]: Revise the description to clarify that the same elements in any order produce the same hash.
- L455 `rangeHash` [blocker/style]: Revise the documentation to mention that the method delegates to `rangeHash(start, step, last, seqSeed)` and remove the redundant `@return` tag.
- L465 `productHash (deprecated)` [blocker/style]: Revise the documentation to mention that this method is deprecated in favor of `caseClassHash` and that it delegates to `caseClassHash(x, productSeed, null)`.
- L545 `apply (in accum)` [blocker/style]: Revise the documentation to describe the side effects of the method (updating `a`, `b`, `n`, and `c`).
- L565 `setHash` [blocker/style]: Revise the documentation to mention that the method delegates to `unorderedHash(xs, setSeed)` and remove the redundant `@return` tag.
- L575 `ArrayHashing.hash` [blocker/style]: Revise the documentation to mention that the method delegates to `arrayHash(a)`.
- L530 `mapHash` [blocker/style]: Revise the documentation to describe the edge case for empty maps (delegating to `emptyMapHash`) and the accumulation logic for non-empty maps.

## Inline NEEDS-HUMAN markers left in source
(none)
