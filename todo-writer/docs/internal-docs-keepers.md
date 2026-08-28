# Keepers: non-user-facing Scaladoc worth leaving in place

A companion to `internal-docs-inventory.md`. Of the 2310 documented
declarations hidden from the published Scaladoc, these **319** carry
information a reader cannot recover from the signature, and should stay on
the user-facing PR branches rather than moving to `<branch>-internal-docs`.

Consumed by `split-internal-docs.py split --keep todo-writer/reviews/internal-docs-keepers.json`.

## The three rules

1. **Container docs** (60). Every hidden `class`/`trait`/`object`. A type
   with no documentation at all is opaque; one sentence saying what it *is*
   is the cheapest orientation there is, and there are only sixty of them.
2. **High-information members** (94). Docs stating an invariant, a protocol,
   a cross-platform difference, or a non-obvious constraint.
3. **Protocol files** (236). Files whose whole content is a machine whose
   rules live nowhere else. Everything hidden in them is kept.

The rules overlap; the union is 319. The remaining **1991** hidden
declarations still move out - overwhelmingly one-line accessors and
mechanically repeated family members.

## What is deliberately *not* kept

The clearest case is `semiclone`, which appears 45 times across the stepper
implementations. The two base-class declarations state the contract - split
off `[i0, half)`, then `trySplit` advances this stepper past it. The other
forty are per-element-type restatements ("Creates a new `IntArrayStepper`
over the range from `i0` to `half`") that a reader gets from the signature.
Base kept, repeats dropped. The same principle drops the
`cachedJavaKeySetHashCode` overrides in `HashMap`/`HashSet` while keeping
the declaring one in `ChampCommon`, which is where the real explanation is.

## A note on the declaration that prompted this

The PR review quoted `LazyListBase.set` in `library-js` as an example of
documenting an implementation detail, showing the summary line alone:

    /** Sets the `_tail` field of `ll` to `value`.

The sentence below the fold is the reason the doc exists:

    *  Unlike the JVM version, the read and the write are not atomic, which is
    *  sufficient in the single-threaded Scala.js runtime.

That is a cross-platform contract, not a restatement of the assignment, so
this declaration is a keeper. The reviewer's general point stands for the
other 1991 - it is this particular example that is the exception.

## The list

### `library/src/scala/collection/concurrent/TrieMap.scala`

The lock-free GCAS/INode protocol. Nothing here is derivable from the signatures.

60 declaration(s).

- L40    `def this` (private[collection]) - protocol file
- L47    `def WRITE` (private[collection]) - protocol file
- L56    `def CAS` (private[collection]) - protocol file
- L64    `def gcasRead` (private[collection]) - protocol file
- L73    `def GCAS_READ` (private[collection]) - protocol file
- L123   `def GCAS` (private[collection]) - protocol file
- L147   `def copyToGen` (private[collection]) - protocol file
- L480   `def isNullInode` (private[collection]) - protocol file
- L487   `def cachedSize` (private[collection]) - protocol file
- L495   `def knownSize` (private[collection]) - protocol file
- L503   `def string` (private[collection]) - protocol file
- L524   `val KEY_PRESENT` (private[concurrent]) - protocol file
- L527   `val KEY_ABSENT` (private[concurrent]) - protocol file
- L530   `val KEY_PRESENT_OR_ABSENT` (private[concurrent]) - protocol file
- L538   `def newRootNode` (private[concurrent]) - protocol file
- L554   `def string` (private[concurrent]) - protocol file
- L561   `def cachedSize` (private[concurrent]) - protocol file
- L567   `def knownSize` (private[concurrent]) - protocol file
- L571   `def toString` (private[concurrent]) - protocol file
- L578   `def kvPair` (private[concurrent]) - protocol file
- L586   `def copy` (private[collection]) - protocol file
- L589   `def copyTombed` (private[collection]) - protocol file
- L592   `def copyUntombed` (private[collection]) - protocol file
- L595   `def kvPair` (private[collection]) - protocol file
- L601   `def string` (private[collection]) - protocol file
- L609   `def copy` (private[collection]) - protocol file
- L612   `def copyTombed` (private[collection]) - protocol file
- L615   `def copyUntombed` (private[collection]) - protocol file
- L618   `def kvPair` (private[collection]) - protocol file
- L623   `def cachedSize` (private[collection]) - protocol file
- L626   `def knownSize` (private[collection]) - protocol file
- L632   `def string` (private[collection]) - protocol file
- L645   `def this` (private[collection]) - protocol file
- L655   `def this` (private[collection]) - protocol file
- L666   `def inserted` (private[collection]) - protocol file
- L686   `def removed` (private[collection]) - protocol file
- L700   `def get` (private[collection]) - protocol file
- L706   `def cachedSize` (private[collection]) - protocol file
- L710   `def knownSize` (private[collection]) - protocol file
- L717   `def string` (private[collection]) - protocol file
- L729   `def cachedSize` (private[collection]) - protocol file
- L741   `def knownSize` (private[collection]) - protocol file
- L775   `def updatedAt` (private[collection]) - protocol file
- L789   `def removedAt` (private[collection]) - protocol file
- L807   `def insertedAt` (private[collection]) - protocol file
- L849   `def toContracted` (private[collection]) - protocol file
- L868   `def toCompressed` (private[collection]) - protocol file
- L895   `def string` (private[collection]) - protocol file
- L899   `def toString` (private[collection]) - protocol file
- L924   `def dual` (private[concurrent]) - protocol file
- L946   `var committed` (private[concurrent]) - protocol file
- L1485  `val Always` (private[concurrent]) - protocol file
- L1488  `val FullEquals` (private[concurrent]) - protocol file
- L1491  `val ReferenceEq` (private[concurrent]) - protocol file
- L1502  `def shouldRemove` (private[concurrent]) - protocol file
- L1524  `def hasNext` (private[collection]) - protocol file
- L1530  `def next` (private[collection]) - protocol file
- L1573  `def advance` (private[collection]) - protocol file
- L1595  `def newIterator` (private[collection]) - protocol file
- L1604  `def dupTo` (private[collection]) - protocol file

### `library/src/scala/collection/immutable/RedBlackTree.scala`

Red-black invariants, the mutable-during-construction discipline, and what `validate` does and does not check.

54 declaration(s).

- L46    `def validate` (private[collection]) - high-information, protocol file
- L66    `def isEmpty` (private[collection]) - protocol file
- L74    `def contains` (private[collection]) - protocol file
- L83    `def get` (private[collection]) - protocol file
- L100   `def lookup` (private[collection]) - protocol file
- L119   `def beforePublish` (private[collection]) - protocol file
- L245   `def mutableUpd` (private[collection]) - high-information, protocol file
- L274   `def mutableUpd` (private[collection]) - high-information, protocol file
- L293   `def count` (private[collection]) - protocol file
- L311   `def update` (private[collection]) - protocol file
- L323   `def delete` (private[collection]) - protocol file
- L336   `def rangeImpl` (private[collection]) - protocol file
- L351   `def range` (private[collection]) - protocol file
- L360   `def from` (private[collection]) - protocol file
- L369   `def to` (private[collection]) - protocol file
- L378   `def until` (private[collection]) - protocol file
- L392   `def drop` (private[collection]) - protocol file
- L405   `def take` (private[collection]) - protocol file
- L418   `def slice` (private[collection]) - protocol file
- L428   `def smallest` (private[collection]) - protocol file
- L442   `def greatest` (private[collection]) - protocol file
- L460   `def tail` (private[collection]) - protocol file
- L483   `def init` (private[collection]) - protocol file
- L537   `def foreach` (private[collection]) - protocol file
- L553   `def keysEqual` (private[collection]) - high-information, protocol file
- L573   `def valuesEqual` (private[collection]) - protocol file
- L593   `def entriesEqual` (private[collection]) - high-information, protocol file
- L613   `def foreachKey` (private[collection]) - protocol file
- L629   `def foreachEntry` (private[collection]) - protocol file
- L644   `def iterator` (private[collection]) - protocol file
- L651   `def keysIterator` (private[collection]) - protocol file
- L659   `def valuesIterator` (private[collection]) - protocol file
- L674   `def nth` (private[collection]) - protocol file
- L685   `def isBlack` (private[collection]) - protocol file
- L959   `def toString` (private[collection]) - protocol file
- L1201  `def nextResult` (private) - protocol file
- L1204  `def hasNext` (private) - protocol file
- L1216  `def next` (private) - protocol file
- L1230  `def findLeftMostOrPopOnEmpty` (private) - protocol file
- L1250  `val stackOfNexts` (private) - high-information, protocol file
- L1269  `var lookahead` (private) - protocol file
- L1302  `def nextResult` (private) - protocol file
- L1315  `def sameKeys` (private) - high-information, protocol file
- L1340  `def sameValues` (private) - protocol file
- L1365  `def sameEntries` (private) - protocol file
- L1386  `def nextResult` (private) - protocol file
- L1394  `def nextResult` (private) - protocol file
- L1402  `def nextResult` (private) - protocol file
- L1465  `def transform` (private[collection]) - protocol file
- L1493  `def filterEntries` (private[collection]) - protocol file
- L1523  `def partitionEntries` (private[collection]) - protocol file
- L1657  `def union` (private[collection]) - protocol file
- L1672  `def intersect` (private[collection]) - protocol file
- L1686  `def difference` (private[collection]) - protocol file

### `library/src/scala/collection/mutable/RedBlackTree.scala`

Same, for the mutable tree: null-as-leaf representation and the parent-pointer invariants.

42 declaration(s).

- L46    `class Tree` (private[collection]) - container, high-information, protocol file
- L48    `def treeCopy` (private[collection]) - protocol file
- L63    `class Node` (private[collection]) - container, high-information, protocol file
- L65    `var key` (private[collection]) - protocol file
- L68    `var left` (private[collection]) - protocol file
- L71    `var right` (private[collection]) - protocol file
- L74    `var parent` (private[collection]) - protocol file
- L77    `def toString` (private[collection]) - protocol file
- L86    `def empty` (private[collection]) - protocol file
- L104   `def unapply` (private[collection]) - protocol file
- L113   `def isRed` (private[collection]) - protocol file
- L118   `def isBlack` (private[collection]) - protocol file
- L127   `def size` (private[collection]) - protocol file
- L132   `def size` (private[collection]) - protocol file
- L137   `def isEmpty` (private[collection]) - protocol file
- L142   `def clear` (private[collection]) - protocol file
- L154   `def get` (private[collection]) - protocol file
- L174   `def contains` (private[collection]) - protocol file
- L183   `def min` (private[collection]) - protocol file
- L194   `def minKey` (private[collection]) - protocol file
- L210   `def minNodeNonNull` (private[collection]) - protocol file
- L220   `def max` (private[collection]) - protocol file
- L231   `def maxKey` (private[collection]) - protocol file
- L247   `def maxNodeNonNull` (private[collection]) - protocol file
- L274   `def minKeyAfter` (private[collection]) - protocol file
- L319   `def maxKeyBefore` (private[collection]) - protocol file
- L355   `def insert` (private[collection]) - protocol file
- L431   `def delete` (private[collection]) - protocol file
- L635   `def foreach` (private[collection]) - protocol file
- L653   `def foreachKey` (private[collection]) - protocol file
- L674   `def foreachEntry` (private[collection]) - protocol file
- L694   `def transform` (private[collection]) - protocol file
- L715   `def iterator` (private[collection]) - protocol file
- L726   `def keysIterator` (private[collection]) - protocol file
- L739   `def valuesIterator` (private[collection]) - protocol file
- L749   `def nextResult` (private) - protocol file
- L752   `def hasNext` (private) - protocol file
- L759   `def next` (private) - protocol file
- L785   `def nextResult` (private) - protocol file
- L795   `def nextResult` (private) - protocol file
- L805   `def nextResult` (private) - protocol file
- L957   `def copyTree` (private[collection]) - protocol file

### `library/src/scala/collection/immutable/ChampCommon.scala`

CHAMP node invariants, including the corrected account of `cachedJavaKeySetHashCode`.

30 declaration(s).

- L27    `val HashCodeLength` (private[collection]) - protocol file
- L34    `val BitPartitionSize` (private[collection]) - protocol file
- L39    `val BitPartitionMask` (private[collection]) - protocol file
- L45    `val MaxDepth` (private[collection]) - protocol file
- L50    `val BranchingFactor` (private[collection]) - protocol file
- L60    `def maskFrom` (private[collection]) - protocol file
- L69    `def bitposFrom` (private[collection]) - protocol file
- L80    `def indexFrom` (private[collection]) - protocol file
- L92    `def indexFrom` (private[collection]) - protocol file
- L99    `def hasNodes` (private[collection]) - protocol file
- L102   `def nodeArity` (private[collection]) - protocol file
- L109   `def getNode` (private[collection]) - protocol file
- L112   `def hasPayload` (private[collection]) - protocol file
- L115   `def payloadArity` (private[collection]) - protocol file
- L122   `def getPayload` (private[collection]) - protocol file
- L130   `def getHash` (private[collection]) - protocol file
- L141   `def cachedJavaKeySetHashCode` (private[collection]) - protocol file
- L154   `def removeElement` (private[collection]) - protocol file
- L171   `def removeAnyElement` (private[collection]) - protocol file
- L189   `def insertElement` (private[collection]) - protocol file
- L208   `def insertAnyElement` (private[collection]) - protocol file
- L239   `var currentValueCursor` (private[immutable]) - protocol file
- L243   `var currentValueLength` (private[immutable]) - protocol file
- L247   `var currentValueNode` (private[immutable]) - protocol file
- L265   `def this` (private[immutable]) - protocol file
- L324   `def hasNext` (private[immutable]) - protocol file
- L341   `var currentValueCursor` (private[immutable]) - protocol file
- L345   `var currentValueNode` (private[immutable]) - protocol file
- L357   `def this` (private[immutable]) - protocol file
- L405   `def hasNext` (private[immutable]) - protocol file

### `library/src/scala/collection/convert/JavaCollectionWrappers.scala`

Wrapper identity and view semantics: which wrappers write through, which unwrap back to the original, and what the Java side does not support.

24 declaration(s).

- L43    `class IteratorWrapper` (private[collection]) - container, high-information
- L85    `class JIteratorWrapper` (private[collection]) - container, high-information
- L115   `class JEnumerationWrapper` (private[collection]) - container, high-information
- L142   `trait IterableWrapperTrait` (private[collection]) - container, high-information
- L164   `class IterableWrapper` (private[collection]) - container, high-information
- L187   `class JIterableWrapper` (private[collection]) - container, high-information
- L221   `class JCollectionWrapper` (private[collection]) - container, high-information
- L261   `class SeqWrapper` (private[collection]) - container
- L282   `class MutableSeqWrapper` (private[collection]) - container, high-information
- L313   `class MutableBufferWrapper` (private[collection]) - container, high-information
- L351   `class JListWrapper` (private[collection]) - container, high-information
- L466   `class SetWrapper` (private[collection]) - container, high-information
- L519   `class MutableSetWrapper` (private[collection]) - container, high-information
- L554   `class JSetWrapper` (private[collection]) - container
- L639   `class MapWrapper` (private[collection]) - container, high-information
- L747   `class MutableMapWrapper` (private[collection]) - container, high-information
- L786   `class AbstractJMapWrapper` (private[collection]) - container, high-information
- L802   `trait JMapWrapperLike` (private[collection]) - container
- L994   `class ConcurrentMapWrapper` (private[collection]) - container, high-information
- L1076  `def getOrElseUpdate` (private[collection]) - high-information
- L1160  `def updateWith` (private[collection]) - high-information
- L1184  `class DictionaryWrapper` (private[collection]) - container, high-information
- L1260  `class JDictionaryWrapper` (private[collection]) - container
- L1338  `class JPropertiesWrapper` (private[collection]) - container

### `library/src/scala/collection/mutable/CheckedIndexedSeqView.scala`

How each checked view detects mutation of the underlying collection.

11 declaration(s).

- L181   `class Id` (private[mutable]) - container
- L193   `class Appended` (private[mutable]) - container
- L205   `class Prepended` (private[mutable]) - container
- L217   `class Concat` (private[mutable]) - container
- L229   `class Take` (private[mutable]) - container
- L241   `class TakeRight` (private[mutable]) - container
- L253   `class Drop` (private[mutable]) - container
- L265   `class DropRight` (private[mutable]) - container
- L279   `class Map` (private[mutable]) - container
- L290   `class Reverse` (private[mutable]) - container, high-information
- L313   `class Slice` (private[mutable]) - container, high-information

### `library/src/scala/collection/immutable/LazyListBase.scala`

JVM tail-publication protocol: the atomic `TailUpdater` and the `InRace` latch.

10 declaration(s).

- L49    `class TailUpdater` (private[immutable]) - container, high-information, protocol file
- L57    `def compareAndSet` (private[immutable]) - protocol file
- L64    `def getAndSet` (private[immutable]) - protocol file
- L78    `def isCurrentThread` (private[immutable]) - high-information, protocol file
- L88    `def InRace` (private[immutable]) - high-information, protocol file
- L98    `class InRace` (private[immutable]) - container, high-information, protocol file
- L106   `def tryAcquireShared` (private) - high-information, protocol file
- L113   `def tryReleaseShared` (private) - high-information, protocol file
- L121   `def await` (private[immutable]) - protocol file
- L123   `def countDown` (private[immutable]) - protocol file

### `library/src/scala/collection/immutable/LazyListIterableBase.scala`

As above, for `LazyListIterable`.

10 declaration(s).

- L53    `class TailUpdater` (private[immutable]) - container, high-information, protocol file
- L61    `def compareAndSet` (private[immutable]) - protocol file
- L69    `def getAndSet` (private[immutable]) - protocol file
- L84    `def isCurrentThread` (private[immutable]) - high-information, protocol file
- L95    `def InRace` (private[immutable]) - high-information, protocol file
- L105   `class InRace` (private[immutable]) - container, high-information, protocol file
- L113   `def tryAcquireShared` (private) - high-information, protocol file
- L120   `def tryReleaseShared` (private) - high-information, protocol file
- L128   `def await` (private[immutable]) - protocol file
- L130   `def countDown` (private[immutable]) - protocol file

### `library-js/src/scala/collection/immutable/LazyListBase.scala`

The Scala.js counterpart, which exists only to say why the JVM's atomicity is unnecessary. The single highest-value internal documentation in the project.

9 declaration(s).

- L37    `class TailUpdater` (private[immutable]) - container, high-information, protocol file
- L48    `def compareAndSet` (private[immutable]) - high-information, protocol file
- L60    `def getAndSet` (private[immutable]) - high-information, protocol file
- L77    `def isCurrentThread` (private[immutable]) - high-information, protocol file
- L87    `def InRace` (private[immutable]) - protocol file
- L96    `class InRace` (private[immutable]) - container, high-information, protocol file
- L103   `def owner` (private[immutable]) - protocol file
- L105   `def await` (private[immutable]) - protocol file
- L107   `def countDown` (private[immutable]) - protocol file

### `library-js/src/scala/collection/immutable/LazyListIterableBase.scala`

As above, for `LazyListIterable`.

9 declaration(s).

- L42    `class TailUpdater` (private[immutable]) - container, high-information, protocol file
- L53    `def compareAndSet` (private[immutable]) - high-information, protocol file
- L64    `def getAndSet` (private[immutable]) - high-information, protocol file
- L81    `def isCurrentThread` (private[immutable]) - high-information, protocol file
- L91    `def InRace` (private[immutable]) - protocol file
- L100   `class InRace` (private[immutable]) - container, high-information, protocol file
- L107   `def owner` (private[immutable]) - protocol file
- L109   `def await` (private[immutable]) - protocol file
- L111   `def countDown` (private[immutable]) - protocol file

### `library/src/scala/collection/immutable/HashMap.scala`

The builder's copy-on-write aliasing handshake, and node-level sharing.

6 declaration(s).

- L1585  `def copyAndSetValue` (private) - high-information
- L1949  `def mergeInto` (private) - high-information
- L2421  `def filterImpl` (private) - high-information
- L2778  `def updated` (private) - high-information
- L2963  `def concat` (private) - high-information
- L3433  `def result` (private[immutable]) - high-information

### `library/src/scala/collection/immutable/HashSet.scala`

As above, for `HashSet`.

6 declaration(s).

- L1492  `def subsetOf` (private) - high-information
- L1737  `def diff` (private) - high-information
- L2360  `def contains` (private) - high-information
- L2553  `def concat` (private) - high-information
- L2867  `def result` (private[collection]) - high-information
- L2930  `def clear` (private[collection]) - high-information

### `library/src/scala/collection/convert/impl/InOrderStepperBase.scala`

The `semiclone`/`trySplit` split protocol, stated once at the base rather than repeated per element type.

5 declaration(s).

- L49    `def semiclone` (private[convert]) - protocol file
- L52    `def hasStep` (private[convert]) - protocol file
- L57    `def characteristics` (private[convert]) - protocol file
- L62    `def estimateSize` (private[convert]) - protocol file
- L70    `def trySplit` (private[convert]) - protocol file

### `library/src/scala/collection/convert/impl/IndexedStepperBase.scala`

As above, for the indexed steppers.

5 declaration(s).

- L37    `def semiclone` (private[convert]) - protocol file
- L40    `def hasStep` (private[convert]) - protocol file
- L45    `def characteristics` (private[convert]) - protocol file
- L48    `def estimateSize` (private[convert]) - protocol file
- L56    `def trySplit` (private[convert]) - protocol file

### `library/src/scala/collection/immutable/TreeSeqMap.scala`

3 declaration(s).

- L624   `class Iterator` (private[immutable]) - container, high-information
- L713   `class Tip` (private[immutable]) - container, high-information
- L744   `class Bin` (private[immutable]) - container, high-information

### `library/src/scala/collection/immutable/Vector.scala`

The `prepend1IfSpace`/`append1IfSpace` fast paths and slice-builder assembly.

3 declaration(s).

- L2335  `def result` (private) - high-information
- L3621  `def prepend1IfSpace` (private) - high-information
- L3660  `def append1IfSpace` (private) - high-information

### `library/src/scala/concurrent/impl/Promise.scala`

3 declaration(s).

- L139   `class DefaultPromise` (private[concurrent]) - container
- L628   `trait Callbacks` (private[concurrent]) - container
- L636   `class ManyCallbacks` (private[concurrent]) - container

### `library/src/scala/StringContext.scala`

2 declaration(s).

- L337   `class InvalidUnicodeEscapeException` (protected[scala]) - container
- L344   `class InvalidUnicodeEscapeException` (protected[scala]) - container

### `library/src/scala/collection/IndexedSeqView.scala`

2 declaration(s).

- L199   `def sliceIterator` (private[collection]) - high-information
- L246   `def sliceIterator` (private[collection]) - high-information

### `library/src/scala/collection/StringParsers.scala`

2 declaration(s).

- L86    `def parseByte` (private[scala]) - high-information
- L116   `def parseShort` (private[scala]) - high-information

### `library/src/scala/collection/generic/BitOperations.scala`

2 declaration(s).

- L27    `trait Int` (private[collection]) - container, high-information
- L89    `trait Long` (private[collection]) - container, high-information

### `library/src/scala/collection/mutable/MutationTracker.scala`

The mutation-count discipline that the checked views rely on.

2 declaration(s).

- L81    `def hasNext` (private) - protocol file
- L88    `def next` (private) - protocol file

### `library/src/scala/concurrent/impl/ExecutionContextImpl.scala`

2 declaration(s).

- L45    `class DefaultThreadFactory` (private[concurrent]) - container, high-information
- L90    `def newThread` (private[concurrent]) - high-information

### `library/src/scala/concurrent/impl/FutureConvertersImpl.scala`

2 declaration(s).

- L31    `class CF` (private[scala]) - container, high-information
- L235   `class P` (private[scala]) - container, high-information

### `library/src/scala/sys/process/BasicIO.scala`

2 declaration(s).

- L68    `def apply` (private[process]) - high-information
- L110   `def apply` (private[process]) - high-information

### `library/src/scala/sys/process/ProcessImpl.scala`

2 declaration(s).

- L33    `def apply` (private[process]) - high-information
- L336   `class DummyProcess` (private[process]) - container, high-information

### `library-js/src/scala/collection/immutable/Range.scala`

1 declaration(s).

- L1076  `def drop` (private) - high-information

### `library/src/scala/collection/ArrayOps.scala`

1 declaration(s).

- L180   `def drop` (private[collection]) - high-information

### `library/src/scala/collection/Iterator.scala`

1 declaration(s).

- L1556  `def concat` (private) - high-information

### `library/src/scala/collection/immutable/BitSet.scala`

1 declaration(s).

- L73    `def fromBitMaskNoCopy` (protected[collection]) - high-information

### `library/src/scala/collection/immutable/IntMap.scala`

1 declaration(s).

- L285   `def next` (private[immutable]) - high-information

### `library/src/scala/collection/immutable/LazyList.scala`

1 declaration(s).

- L1775  `class DeferredState` (private) - container, high-information

### `library/src/scala/collection/immutable/LongMap.scala`

1 declaration(s).

- L282   `def next` (private[immutable]) - high-information

### `library/src/scala/collection/immutable/Range.scala`

1 declaration(s).

- L1142  `def drop` (private) - high-information

### `library/src/scala/collection/mutable/HashTable.scala`

1 declaration(s).

- L485   `trait HashUtils` (private[collection]) - container, high-information

### `library/src/scala/runtime/ClassValueCompat.scala`

1 declaration(s).

- L85    `trait ClassValueInterface` (private[scala]) - container

### `library/src/scala/sys/process/ProcessBuilderImpl.scala`

1 declaration(s).

- L159   `def run` (private[process]) - high-information
