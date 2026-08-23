# Doc review digest: library/src/scala/collection/concurrent/Map.scala

- models: writer devstral-latest | accuracy sonnet | style mistral-large-latest | adjudicator devstral-latest
- converged: false (up to 3 rounds)
- final refinement after review limit: true
- verification review of that refine: revise
- a real accuracy review ran at some point: true
- the file AS IT NOW STANDS was accuracy-reviewed: true
- accuracy verdict: revise
- style verdict: approve
- ADJUDICATOR verdict (final): revise

> **NOT REVIEWED.** Do not put this file in a PR on the strength of this
> digest. Read the diff yourself, or re-run the file once the reviewer is
> healthy. See reviews/NOT-REVIEWED.txt.

## Reviewer disagreements the adjudicator settled

- L165 `Map.updateWith` -> ruled for **accuracy**
  - accuracy: The @throws tag is malformed and must be rewritten to include the exception class name.
  - style: The @throws tag is correct but could be more specific about the exception being rethrown as-is.
  - why: The @throws tag is malformed and will not render correctly in Scaladoc without the exception class name.

## Outstanding worklist at the end

- L165 `Map.updateWith` [blocker/both]: Rewrite the @throws tag to start with the exception class name, e.g., `@throws Exception if `remappingFunction` throws`.
- L109 `Map.getOrElseUpdate` [nit/accuracy]: Drop the @return tag as it restates the description.
- L106 `Map.getOrElseUpdate` [nit/accuracy]: Remove the extra blank line between the description and the @param tags.
- L111 `getOrElseUpdate` [nit/style]: Clarify that defaultValue is evaluated lazily by changing the @param tag to: `@param defaultValue the value to be computed (lazily) and stored if the key is absent (unless another thread inserts a value first)`.

## Inline NEEDS-HUMAN markers left in source
(none)
