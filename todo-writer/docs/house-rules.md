# House rules (learned from reviewer feedback)

Append one rule per bullet as PR reviewers raise conventions the prompts don't
already cover. `fill-doc-todos-free.sh` injects this file into the Writer, both
reviewers, the adjudicator and the refine step, so every rule here shapes the
next partition's output.

Keep each rule short, imperative, and grounded in a real review comment. Delete or
revise a rule if a later review contradicts it. When this file changes, note the
short SHA in the PR description so you know which ruleset produced which PR.

## Rules

Seeded after week 4 (util + concurrent, PR #75), from the human reviewer's 51
comments and two adversarial reviews of the same branch. Each rule below is a
mistake that shipped.

### Say what the code does, not what the name suggests

- Document the member as IMPLEMENTED, not as its name implies. If the body is
  `= this`, the summary is "Returns this future, since ...", never "Creates a
  new future by applying a function": no function is applied and nothing is
  created. This was the single largest cluster in week 4.
- Never describe behaviour that cannot occur for the receiver you are
  documenting. On a future that is never completed, "will be executed when this
  future completes" describes an event that never happens.
- A method returning `Nothing`, or whose body ends in `throw`, never returns
  normally. Do not give it an `@return`; say that it always throws.
- If a parameter is never used by the body, say so on its tag: "(never called)",
  "(never used)". If it IS used, do not write "(ignored)". `Success.fold` shipped
  with `@param fa the function to apply if this were a Failure (ignored)` while
  the body called `fa` on a non-fatal exception from `fb`.
- Check each `@param` name against the signature. `DurationConversions` shipped
  `@param c the classifier instance` on 20 methods where `ev` is the classifier
  and `c` is the value being converted.

### Exceptions

- Use `@throws` tags for what a method throws, rather than prose in the
  description. Give one tag per exception type with the condition that raises it.

### Do not write a second doc comment

- Before writing, check whether the declaration already has a doc comment,
  including one separated from it by `//` comments or an annotation. Two adjacent
  `/** ... */` comments leave the first orphaned in the generated API, and week 4
  shipped one on `NonFatal.apply` and another pair in `reflect`.
- One doc comment per declaration in a parameter list, immediately before the
  parameter it describes. Two comments stacked before the first parameter leave
  the second parameter undocumented (`BatchingExecutor.AbstractBatch`).
- A doc comment must come ABOVE any annotation on the declaration. Below it, the
  parser drops the comment.

### Wording

- Drop hedging that carries no information: "is to be considered non-fatal"
  becomes "is non-fatal".
- Do not open with "This method ...", "This class ...". Describe the thing.
- Do not restate the `@deprecated` annotation in prose; Scaladoc already renders
  it.
- A one-line doc that only spells out the identifier in words ("`runLimit`: the
  maximum number of tasks processed in a single batch") adds nothing. Either say
  something a reader could not infer from the name (the unit, the reason for the
  value, what happens at the limit) or leave it to the human.

### Tags

- Drop `@return` only when the description begins with "Returns" and already
  states the whole return value. Keep it when it carries an edge case, a
  sentinel, a unit or an exception guarantee. Week 4 shipped 26 `@return` tags
  that restated their own description verbatim.
- A tag that contradicts the description above it is always a blocker. When they
  disagree, the description is usually right and the tag is boilerplate.
