# Doc review digest: library/src/scala/collection/concurrent/TrieMap.scala

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

- L135 `INode.copyToGen` -> ruled for **accuracy**
  - accuracy: Replace the description with: 'Creates a copy of this INode in the specified generation, sharing its current main node; used together with `renewed`/`copyToGen` recursion for snapshot operations.'
  - style: 
  - why: The accuracy reviewer correctly identified that the documentation falsely claims a deep copy, which contradicts the implementation.
- L598 `TNode.copyTombed` -> ruled for **accuracy**
  - accuracy: Replace the description with: 'Creates a copy of this TNode, which is already tombed.'
  - style: 
  - why: The accuracy reviewer correctly identified that the documentation falsely claims identity return, which contradicts the implementation.
- L540 `FailedNode.string` -> ruled for **accuracy**
  - accuracy: Change the @param tag to: '@param lev the current level in the trie (never used)'
  - style: 
  - why: The accuracy reviewer correctly identified that the parameter is unused, which is a factual inaccuracy.
- L727 `CNode.knownSize` -> ruled for **accuracy**
  - accuracy: Add '@return the known size, or -1 if not yet computed' to clarify the sentinel value.
  - style: 
  - why: The accuracy reviewer correctly identified that the sentinel value should be documented.
- L60 `INode.gcasRead / INode.GCAS_READ` -> ruled for **accuracy**
  - accuracy: Verify with someone familiar with the GCAS protocol invariants whether `mainnode` can be null when `gcasRead`/`GCAS_READ` is called externally; if not, drop the null case from the doc, or if so, note the discrepancy with the direct `m.prev` dereference.
  - style: 
  - why: The accuracy reviewer correctly identified a potential discrepancy that requires human verification.
- L1178 `TrieMap.lookup` -> ruled for **accuracy**
  - accuracy: Drop the leading 'Deprecated.' and start directly with 'Looks up the value associated with the key, returning null if not found.'
  - style: Change @return to: 'the value associated with the key, or null if not found (for compatibility with deprecated usage)'
  - why: The accuracy reviewer correctly identified that the description restates the @deprecated annotation, which is a house rule violation.
- L1397 `TrieMap.knownSize` -> ruled for **accuracy**
  - accuracy: Replace the description with: 'Returns the known size of this TrieMap, or -1 if this TrieMap is mutable (since it may be concurrently modified) or its size has not yet been cached.'
  - style: 
  - why: The accuracy reviewer correctly identified that the description is misleading about the conditions under which -1 is returned.
- L1285 `TrieMap.getOrElseUpdate` -> ruled for **accuracy**
  - accuracy: Drop the second clause from the first sentence and let the existing Note paragraph carry that detail, e.g., 'Returns the value associated with the key, or computes and inserts it using `defaultValue` if absent.'
  - style: Change @return to: 'the value associated with the key, which may be the result of `defaultValue` or a concurrent update'
  - why: The accuracy reviewer correctly identified that the description is redundant with the existing Note paragraph.
- L1576 `TrieMapIterator.dupTo` -> ruled for **accuracy**
  - accuracy: Mention the receiver-side mutation, e.g., '...; when this iterator has an active sub-iterator, both this iterator's and the new one's sub-iterators are replaced with materialized copies.'
  - style: 
  - why: The accuracy reviewer correctly identified that the description omits a significant side effect.

## Outstanding worklist at the end

- L60 `INode.gcasRead / INode.GCAS_READ` [nit/accuracy/NEEDS-HUMAN]: Verify with someone familiar with the GCAS protocol invariants whether `mainnode` can be null when `gcasRead`/`GCAS_READ` is called externally; if not, drop the null case from the doc, or if so, note the discrepancy with the direct `m.prev` dereference.
- L135 `INode.copyToGen` [blocker/accuracy]: Replace the description with: 'Creates a copy of this INode in the specified generation, sharing its current main node; used together with `renewed`/`copyToGen` recursion for snapshot operations.'
- L598 `TNode.copyTombed` [blocker/accuracy]: Replace the description with: 'Creates a copy of this TNode, which is already tombed.'
- L540 `FailedNode.string` [blocker/accuracy]: Change the @param tag to: '@param lev the current level in the trie (never used)'
- L727 `CNode.knownSize` [nit/accuracy]: Add '@return the known size, or -1 if not yet computed' to clarify the sentinel value.
- L1178 `TrieMap.lookup` [nit/accuracy]: Drop the leading 'Deprecated.' and start directly with 'Looks up the value associated with the key, returning null if not found.'
- L1397 `TrieMap.knownSize` [nit/accuracy]: Replace the description with: 'Returns the known size of this TrieMap, or -1 if this TrieMap is mutable (since it may be concurrently modified) or its size has not yet been cached.'
- L1285 `TrieMap.getOrElseUpdate` [nit/accuracy]: Drop the second clause from the first sentence and let the existing Note paragraph carry that detail, e.g., 'Returns the value associated with the key, or computes and inserts it using `defaultValue` if absent.'
- L1576 `TrieMapIterator.dupTo` [nit/accuracy]: Mention the receiver-side mutation, e.g., '...; when this iterator has an active sub-iterator, both this iterator's and the new one's sub-iterators are replaced with materialized copies.'
- L46 `INode#WRITE` [nit/style]: Change to: 'Atomically writes the specified main node value to this INode.'
- L114 `INode#GCAS` [nit/style]: Change to: 'Atomically updates the main node from the expected old value to the new value using the GCAS protocol.'
- L215 `INode#rec_insertif` [nit/style]: Change @return to: 'null if a retry is needed, `Option[V]` otherwise (indicating the previous value bound to the key)'
- L316 `INode#rec_lookup` [nit/style]: Change @return to: '`INodeBase.NO_SUCH_ELEMENT_SENTINEL` if no value was found, `INodeBase.RESTART` if a retry is needed, or the value otherwise'
- L371 `INode#rec_remove` [nit/style]: Change @return to: 'null if a retry is needed, an `Option[V]` indicating the previous value otherwise'
- L574 `SNode#copyTombed` [nit/style]: Change to: 'Creates a tombed copy of this SNode, marking it as removed in the trie.'
- L577 `SNode#copyUntombed` [nit/style]: Change to: 'Creates an untombed copy of this SNode.'
- L651 `LNode#inserted` [nit/style]: Change @return to: 'the new LNode with the pair inserted, replacing any existing pair with an equivalent key'
- L671 `LNode#removed` [nit/style]: Change @return to: 'the new main node after removal, which may be a TNode if only one pair remains'
- L715 `CNode#cachedSize` [nit/style]: Change to: 'Returns the cached size of this CNode, computing it if necessary.'
- L833 `CNode#toContracted` [nit/style]: Change to: 'Returns a contracted version of this CNode, reducing branching if possible.'
- L850 `CNode#toCompressed` [nit/style]: Change to: 'Returns a compressed version of this CNode, removing null i-nodes.'
- L1178 `TrieMap#lookup` [nit/style]: Change @return to: 'the value associated with the key, or null if not found (for compatibility with deprecated usage)'
- L1215 `TrieMap#put` [nit/style]: Change @return to: 'the previous value associated with the key, or None if not found (including if the previous value was null)'
- L1285 `TrieMap#getOrElseUpdate` [nit/style]: Change @return to: 'the value associated with the key, which may be the result of `defaultValue` or a concurrent update'
- L1304 `TrieMap#remove(k: K, v: V)` [nit/style]: Change @return to: 'true if the key-value pair was removed, false if the key was not present or the value did not match'
- L1321 `TrieMap#replace(k: K, oldvalue: V, newvalue: V)` [nit/style]: Change @return to: 'true if the replacement was successful, false if the key was not present or the old value did not match'
- L1410 `TrieMap#lastOption` [nit/style]: Change @return to: 'the last key-value pair in this TrieMap, or `None` if empty or if the last element cannot be retrieved due to concurrent modifications'
- L562 `group: kvPair` [nit/style]: applies to all 3 occurrences of this block (lines 562, 579, 602). Change to: 'Returns the key-value pair currently stored in this node.'

## Inline NEEDS-HUMAN markers left in source
(none)
