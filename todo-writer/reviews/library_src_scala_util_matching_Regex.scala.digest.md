# Doc review digest: library/src/scala/util/matching/Regex.scala

- models: writer devstral-latest | accuracy mistral-medium-latest | style mistral-large-latest | adjudicator devstral-latest
- converged: false (up to 2 rounds)
- final refinement after review limit: true (not re-reviewed)
- accuracy verdict: revise
- style verdict: revise
- ADJUDICATOR verdict (final): revise

## Reviewer disagreements the adjudicator settled

- L792 `MatchData.groupNames (in MatchData)` -> ruled for **merged**
  - accuracy: The description omits that the returned sequence does not include inline group names, which is the core reason for deprecation.
  - style: The documentation restates the `@deprecated` annotation's message without adding meaningful context or explaining the purpose of `groupNames`.
  - why: Both reviewers agree on the need to clarify the exclusion of inline group names and improve the description.
- L896 `MatchData.groupNames (in MatchIterator)` -> ruled for **merged**
  - accuracy: Same factual inaccuracy as the first `groupNames`: the description omits that inline group names are excluded, which is the reason for deprecation.
  - style: The documentation for `groupNames` in `MatchIterator` suffers from the same issues as in `Match`: redundancy with the `@deprecated` annotation and lack of clarity about the purpose and limitations of the field.
  - why: Both reviewers agree on the need to clarify the exclusion of inline group names and improve the description.

## Outstanding worklist at the end

- L792 `MatchData.groupNames (in MatchData)` [blocker/both]: Revise to: "Returns the names of the named capturing groups (excluding inline groups) used to look up groups by name."
- L896 `MatchData.groupNames (in MatchIterator)` [blocker/both]: Revise to: "Returns the names of the named capturing groups (excluding inline groups) used to look up groups by name."
- L792 `MatchData.groupNames (in MatchData)` [nit/accuracy]: Remove the redundant `@deprecated` tag from the Scaladoc body.
- L896 `MatchData.groupNames (in MatchIterator)` [nit/accuracy]: Remove the redundant `@deprecated` tag from the Scaladoc body.

## Inline NEEDS-HUMAN markers left in source
(none)
