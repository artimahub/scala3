# Human review comments on the Scaladoc pull requests

Every review comment a Scala maintainer left on the two pull requests from this
project, captured from the GitHub API so you do not need network access to read
them. Reviewer is **cheeseng**; replies are from **bvenners**, who opened the PRs.

Read this before writing. It is the only place where someone who maintains Scala
says, in their own words, what is wrong with documentation of exactly this kind.
Where a reply says a comment was declined, the reasoning matters as much as the
comment.


---

# PR #26669 — week 3: Array, Predef, Function/Tuple/Product, sys

<https://github.com/scala/scala3/pull/26669>

8 comments in 8 distinct threads.

## `library/src/scala/Function7.scala`:57

```diff
@@ -54,5 +54,6 @@ trait Function7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R] extends AnyRef {
   @annotation.unspecialized def tupled: ((T1, T2, T3, T4, T5, T6, T7)) => R = {
     case ((x1, x2, x3, x4, x5, x6, x7)) => apply(x1, x2, x3, x4, x5, x6, x7)
   }
+  /** TODO FILL IN */
```

**cheeseng:**

> Not sure how this is missed, but I think this needs to be filled in.


## `library/src/scala/runtime/AbstractFunction14.scala`:17

```diff
@@ -14,6 +14,24 @@ package scala.runtime
 
 import scala.language.`2.13`
 
+/** TODO FILL IN
```

**cheeseng:**

> Not sure how this is missed, but I think these needs to be filled in.


## `library/src/scala/runtime/AbstractFunction0.scala`:17

```diff
@@ -14,6 +14,13 @@ package scala.runtime
 
 import scala.language.`2.13`
 
+/** A base class for zero-parameter function implementations, allowing a function
```

**cheeseng:**

> Shall we include the type parameter R also?


## `library/src/scala/runtime/AbstractFunction1.scala`:17

```diff
@@ -14,6 +14,14 @@ package scala.runtime
 
 import scala.language.`2.13`
 
+/** A base class for one-parameter function implementations, allowing a function
```

**cheeseng:**

> Shall we include the type parameter T1 and R also?


## `library/src/scala/runtime/AbstractFunction2.scala`:17

```diff
@@ -14,6 +14,14 @@ package scala.runtime
 
 import scala.language.`2.13`
 
+/** A base class for two-parameter function implementations, allowing a function
```

**cheeseng:**

> Shall we include type parameter T1, T2 and R?


## `library/src/scala/sys/process/ProcessBuilderImpl.scala`:281

```diff
+     *  @param log the `ProcessLogger` to receive standard error output
+     *  @param capacity the maximum number of lines to buffer before blocking the producer; it must
+     *                  be non-null and positive, since a null or non-positive value fails when the
+     *                  underlying buffer is created
+     *  @return a `LazyList` of the process's standard output lines, whose evaluation raises an
+     *          exception once every line has been yielded if the exit code is non-zero
+     */
     def lazyLines(log: ProcessLogger, capacity: Integer): LazyList[String]   = lazyLines(withInput = false, nonZeroException = true, Some(log), capacity)
+    /** Returns the standard output of the process represented by this builder as a `LazyList`
```

**cheeseng:**

> @@param for `capacity` and @@return is missing.


## `library/src/scala/sys/process/ProcessBuilderImpl.scala`:289

```diff
+    /** Returns the standard output of the process represented by this builder as a `LazyList`
+     *  of lines that blocks until each line becomes available, buffering at most `capacity`
+     *  lines ahead of the consumer and starting that process as a side effect.  The capacity must
+     *  be non-null and positive, since a null or non-positive value fails when the underlying
+     *  buffer is created.  Standard error is sent to the console, and a non-zero exit code raises
+     *  no exception.
+     */
     def lazyLines_!(capacity: Integer) : LazyList[String]                    = lazyLines(withInput = false, nonZeroException = false, None, capacity)
+    /** Returns the standard output of the process represented by this builder as a `LazyList`
```

**cheeseng:**

> @@param for `log` and `capacity` is missing, also no @@return .


## `library/src/scala/Function1.scala`:19

```diff
@@ -16,6 +16,11 @@ import scala.language.`2.13`
 
 object Function1 {
 
+  /** Provides the [[UnliftOps.unlift]] method on functions that return an [[scala.Option]].
```

**cheeseng:**

> shall we need the @@param for the `f`?



---

# PR #26822 — week 4: scala.util and scala.concurrent

<https://github.com/scala/scala3/pull/26822>

51 comments in 18 distinct threads.

## `library/src/scala/concurrent/duration/DurationConversions.scala`:78

```diff
+  /** Returns the duration in days. */
   def days: FiniteDuration         = durationIn(DAYS)
+  /** Returns the duration in days. */
   def day: FiniteDuration          = days
 
+  /** Converts the duration in nanoseconds using the given classifier.
+   *
+   *  @tparam C the type of the classifier
+   *  @param c the classifier instance
```

**cheeseng:**

> I think this is inaccurate, `ev` is the classifier instance, but not `c`.

**bvenners:**

> Fixed.


## `library/src/scala/concurrent/duration/DurationConversions.scala` — **19 identical comments**, lines 86, 94, 102, 111, 119, 127, 135, 144, 152, 160, 168, 177 (+7 more)

```diff
+   *  @param c the classifier instance
+   *  @param ev the implicit classifier instance
+   *  @return the result of converting the duration using the classifier
+   */
   def nanoseconds[C](c: C)(implicit ev: Classifier[C]): ev.R  = ev.convert(nanoseconds)
+  /** Converts the duration in nanoseconds using the given classifier.
+   *
+   *  @tparam C the type of the classifier
+   *  @param c the classifier instance
```

**cheeseng:**

> I think this is inaccurate, ev is the classifier instance, but not c.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.

**bvenners:**

> Fixed.


## `library/src/scala/concurrent/impl/ExecutionContextImpl.scala`:45

```diff
+   *  blocking behavior and exception handling.
+   *
+   *  @param daemonic whether created threads should be daemon threads
+   *  @param maxBlockers the maximum number of threads that can block simultaneously
+   *  @param prefix the prefix to use for thread names
+   *  @param uncaught the handler for uncaught exceptions in created threads
+   */
   final class DefaultThreadFactory(
+    /** Whether created threads should be daemon threads */
```

**cheeseng:**

> I think these are duplicated of what's line 39-42 are.

**bvenners:**

> Fixed


## `library/src/scala/concurrent/BatchingExecutor.scala`:39

```diff
+  /** The maximum depth of nested `Runnable` executions before switching to batching mode. */
   final val syncPreBatchDepth = 16
 
   // Max number of Runnables processed in one go (to prevent starvation of other tasks on the pool)
+  /** The maximum number of `Runnable` tasks processed in a single batch execution. */
   final val runLimit = 1024
 
   object MissingParentBlockContext extends BlockContext {
+    /** Always throws an `IllegalStateException`, since reaching this `BlockContext` at all means
```

**cheeseng:**

> This method implementation is really weird, thus the documentation too, if it is a known bug, shall we fix the bug first before documenting the bug itself?


## `library/src/scala/concurrent/Future.scala`:622

```diff
@@ -617,6 +617,15 @@ object Future {
   /** A Future which is never completed. */
   object never extends Future[Nothing] {
 
+    /** Blocks for at most the specified duration and then always throws, since this future is never completed.
+     *
+     *  Throws a `TimeoutException` once `atMost` has elapsed, an `InterruptedException` if the current
```

**cheeseng:**

> I think this be documented using @@throws ?

**bvenners:**

> Fixed.


## `library/src/scala/concurrent/Future.scala`:737

```diff
+     *  Since this future is never completed, the function is never applied and this method
+     *  returns the future itself.
+     *
+     *  @tparam S the type of the resulting future
+     *  @param f the function to apply to the result of this future
+     *  @param executor the execution context for the transformation
+     */
     override final def transformWith[S](f: Try[Nothing] => Future[S])(implicit executor: ExecutionContext): Future[S] = this
+    /** Creates a new future by applying a function to the successful result of this future.
```

**cheeseng:**

> I think this is inaccurate, no new future will be created, I think we can remove this line.

**bvenners:**

> Fixed.


## `library/src/scala/concurrent/Future.scala` — **15 identical comments**, lines 697, 706, 717, 727, 747, 757, 766, 775, 785, 795, 805, 814 (+3 more)

```diff
+     *  Since this future is never completed, this method always returns `None`.
+     */
     override final def value: Option[Try[Nothing]] = None
+    /** Returns a failed projection of this future.
+     *
+     *  Since this future is never completed, this method returns the future itself.
+     */
     override final def failed: Future[Throwable] = this
+    /** Applies the given function to the result of this future if it is completed successfully.
```

**cheeseng:**

> I think this never gonna happens, probably do not need this line.

**bvenners:**

> Fixed

**bvenners:**

> Fixed.

**bvenners:**

> fixed.

**bvenners:**

> fixed

**bvenners:**

> fixed.

**bvenners:**

> fixed.

**bvenners:**

> fixed.

**bvenners:**

> fixed.

**bvenners:**

> fixed.

**bvenners:**

> fixed

**bvenners:**

> fixed

**bvenners:**

> fixed

**bvenners:**

> fixed

**bvenners:**

> fixed

**bvenners:**

> fixed


## `library/src/scala/util/control/Exception.scala`:485

```diff
@@ -408,7 +478,18 @@ object Exception {
   def failAsValue[T](exceptions: Class[?]*)(value: => T): Catch[T] =
     catching(exceptions*) withApply (_ => value)
 
+  /** A builder for creating Catch objects from exception handlers.
+   *
+   *  @tparam T the input type of the handler function
+   *  @tparam R the result type of the handler function
+   *  @param f the function that creates a Catch from a handler
```

**cheeseng:**

> I think this is inaccurate, the function not creating a Catch.

**bvenners:**

> fixed


## `library/src/scala/util/control/Exception.scala`:488

```diff
 
+  /** A builder for creating Catch objects from exception handlers.
+   *
+   *  @tparam T the input type of the handler function
+   *  @tparam R the result type of the handler function
+   *  @param f the function that creates a Catch from a handler
+   */
   class By[T,R](f: T => R) {
+    /** Applies the handler function to create a Catch.
```

**cheeseng:**

> I don't see how a Catch is being created here.

**bvenners:**

> fixed


## `library/src/scala/util/control/NonFatal.scala`:45

```diff
@@ -42,6 +42,10 @@ import scala.language.`2.13`
  */
 object NonFatal {
   /** Returns true if the provided `Throwable` is to be considered non-fatal, or false if it is to be considered fatal */
+  /** Returns `true` if the provided `Throwable` is to be considered non-fatal, or false if it is to be considered fatal
```

**cheeseng:**

> I think we can consider to drop the 'to be considered'.

**bvenners:**

> fixed


## `library/src/scala/util/Properties.scala`:168

```diff
@@ -94,12 +164,14 @@ private[scala] trait PropertiesTrait {
   /** A verbose alternative to [[versionNumberString]].
    */
   val versionString         = s"version ${scalaPropOrElse("version.number", "(unknown)")}"
+  /** The copyright string for the Scala runtime. */
   val copyrightString       = scalaPropOrElse("copyright.string", "Copyright 2002-2025, LAMP/EPFL and Lightbend, Inc. dba Akka")
```

**cheeseng:**

> Since we are seeing this, may be we should update to 2026.

**bvenners:**

> I decided not to in this PR because off topic. @cheeseng please make a separate PR that bumps the year.


## `library/src/scala/util/Try.scala`:326

```diff
+   *
+   *  @tparam U the type of the value in the inner `Try`
+   *  @param ev evidence that `T` is itself a `Try[U]`
+   */
   override def flatten[U](implicit ev: T <:< Try[U]): Try[U]^{this} = this.asTryOf[U]
+  /** Does nothing since this is a `Failure` with no value to apply `f` to.
+   *
+   *  @tparam U the (discarded) result type of the function `f`
+   *  @param f the function to apply to the value if this were a `Success`
```

**cheeseng:**

> Will be good to mention that the `f` function will not be called.

**bvenners:**

> fixed


## `library/src/scala/util/Try.scala`:341

```diff
+   *  @param f the function to apply to the exception
+   *  @return the `Try` returned by `f` applied to the exception; any non-fatal exception thrown by `f` is caught and returned as a `Failure`
+   */
   override def transform[U](s: T => Try[U]^, f: Throwable => Try[U]^): Try[U]^{f} =
     try f(exception) catch { case NonFatal(e) => Failure(e) }
+  /** Returns this `Failure` unchanged since there is no value to apply `f` to.
+   *
+   *  @tparam U the type of the mapped value
+   *  @param f the function to apply to the value if this were a `Success`
```

**cheeseng:**

> It will be good to mention that `f` function will be ignored.

**bvenners:**

> fixed


## `library/src/scala/util/Try.scala`:347

```diff
+   *
+   *  @tparam U the type of the mapped value
+   *  @param f the function to apply to the value if this were a `Success`
+   */
   override def map[U](f: T => U): Try[U]^{this} = this.asTryOf[U]
+  /** Returns this `Failure` unchanged since there is no value to apply `pf` to.
+   *
+   *  @tparam U the type of the value returned by the partial function
+   *  @param pf the partial function to apply to the value if this were a `Success`
```

**cheeseng:**

> It will be good to mention `pf` will be ignored.

**bvenners:**

> fixed


## `library/src/scala/util/Try.scala`:352

```diff
+  /** Returns this `Failure` unchanged since there is no value to apply `pf` to.
+   *
+   *  @tparam U the type of the value returned by the partial function
+   *  @param pf the partial function to apply to the value if this were a `Success`
+   */
   override def collect[U](pf: PartialFunction[T, U]^): Try[U]^{this} = this.asTryOf[U]
+  /** Returns this `Failure` unchanged since there is no value to test against the predicate.
+   *
+   *  @param p the predicate to test the value against
```

**cheeseng:**

> It will be good to mention `p` will be ignored.

**bvenners:**

> fixed.


## `library/src/scala/util/Try.scala` — **2 identical comments**, lines 455, 462

```diff
+   *  @param f the function to apply if this were a `Failure` (ignored)
+   *  @return the `Try` returned by `s` applied to the value; any non-fatal exception thrown by `s` is caught and returned as a `Failure`
+   */
   override def transform[U](s: T => Try[U]^, f: Throwable => Try[U]^): Try[U]^{s} = this flatMap s
+  /** Returns a `Success` containing the result of applying the given function to the value.
+   *
+   *  @tparam U the type of the mapped value
+   *  @param f the function to apply to the value
+   *  @return a `Success` containing the result of applying `f` to the value; any non-fatal exception thrown by `f` is caught and returned as a `Failure`
```

**cheeseng:**

> The second part I think won't happen, we should drop the second part I think.


## `library/src/scala/util/Try.scala`:499

```diff
+   */
   override def recoverWith[U >: T](pf: PartialFunction[Throwable, Try[U]^]^): Try[U] = this
+  /** Returns a `Failure` containing an `UnsupportedOperationException` since this is a `Success`. */
   override def failed: Try[Throwable] = Failure(new UnsupportedOperationException("Success.failed"))
+  /** Returns `Some` containing the value from this `Success`. */
   override def toOption: Option[T] = Some(value)
+  /** Returns `Right` containing the value from this `Success`. */
   override def toEither: Either[Throwable, T] = Right(value)
+  /** Applies the given function `fb` to the value contained in this `Success`.
```

**cheeseng:**

> This does not mention about `fa`, but clearly it is being used.

**bvenners:**

> fixed


## `library/src/scala/util/Try.scala`:502

```diff
   override def failed: Try[Throwable] = Failure(new UnsupportedOperationException("Success.failed"))
+  /** Returns `Some` containing the value from this `Success`. */
   override def toOption: Option[T] = Some(value)
+  /** Returns `Right` containing the value from this `Success`. */
   override def toEither: Either[Throwable, T] = Right(value)
+  /** Applies the given function `fb` to the value contained in this `Success`.
+   *
+   *  @tparam U the type of the result
+   *  @param fa the function to apply if this were a `Failure` (ignored)
```

**cheeseng:**

> `fa` is not ignored, it is being used.

**bvenners:**

> fixed


## Conversation comments

**bvenners:**

> @cheeseng I made fixes for the issues you found yesterday. A bit concerning there were this many issues. I will try Mistral again and see if I can't improve the accuracy.

**SethTisue:**

> is this ready for me?

**bvenners:**

> > is this ready for me?
> 
> Hi @SethTisue Not yet. I will undraft it once I'm done, hopefully tonight. I need to adjudicate Chee Seng's comments and some AI I asked fixes. Once I'm happy with those I'll squash to one commit on top of main and updraft. This one had more issues as I was trying to use Mistral. Still working out the kinks.

**bvenners:**

> @SethTisue Still not quite yet. I had harness problems on this batch, so I want to do one last review round. Will hopefully have it undrafted tonight.


---

# Editor's note: comments that were declined, and why

The replies above are mostly "Fixed." Four of cheeseng's comments were **not**
accepted, and the reasoning was worked out off-thread, so it does not appear in
the GitHub record. It belongs here, because it shows that a maintainer's comment
can be wrong and what it takes to say so.

## `Try.scala:455` and `:462` — declined, the reviewer was mistaken

cheeseng objected to the `@return` clauses saying "any non-fatal exception thrown
by `f` is caught and returned as a `Failure`", arguing that part would not
happen. It does happen, in both cases:

- `Success.map` is `Try[U](f(value))`, and `Try.apply` is
  `try { Success(r) } catch { case NonFatal(e) => Failure(e) }`.
- `Success.collect` catches `NonFatal` directly in its own body.

The documentation was accurate and was left alone. The lesson is not that the
reviewer is unreliable; it is that the code decides, and a claim is worth
checking against it before you act on it either way.

## `Properties.scala:168` — declined, out of scope

cheeseng suggested updating the copyright string from 2025 to 2026. The real
value is generated by the build with the current year; the literal in the source
is only a fallback, inherited verbatim from scala/scala. Changing it would
diverge from upstream and is not a documentation change.

## `BatchingExecutor.scala:39` — declined, not ours to fix

cheeseng found the implementation strange and asked whether the bug should be
fixed before documenting it. `try thunk finally throw ...` is a deliberate
defensive assertion inherited from scala/scala, not a bug: reaching that
`blockOn` at all means `Batch` violated its own invariant, which is what the
exception message says. Changing library behaviour is out of scope for a
comment-only PR.

## What to take from these

Three of the four turn on the same principle: the code and its provenance decide,
not the comment thread and not the documentation. If you think a review comment
is wrong, you may say so, but you owe the specific lines that make it wrong.

