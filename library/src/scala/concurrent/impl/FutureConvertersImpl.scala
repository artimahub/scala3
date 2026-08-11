/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.concurrent.impl

import scala.language.`2.13`
import java.util.concurrent.{CompletableFuture, CompletionStage, TimeUnit}
import java.util.function.{BiConsumer, BiFunction, Consumer, Function => JFunction}

import scala.concurrent.Future
import scala.concurrent.impl.Promise.DefaultPromise
import scala.util.{Failure, Success, Try}

private[scala] object FutureConvertersImpl {
  /** TODO FILL IN
   *
   *  @tparam T TODO FILL IN
   *  @param wrapped TODO FILL IN
   */
  final class CF[T](val wrapped: Future[T]) extends CompletableFuture[T] with (Try[T] => Unit) {
    /** TODO FILL IN
     *
     *  @param t TODO FILL IN
     */
    override def apply(t: Try[T]): Unit = t match {
      case Success(v) => complete(v)
      case Failure(e) => completeExceptionally(e)
    }

    // Ensure that completions of this future cannot hold the Scala Future's completer hostage

    /** TODO FILL IN
     *
     *  @tparam U TODO FILL IN
     *  @param fn TODO FILL IN
     *  @return TODO FILL IN
     */
    override def thenApply[U](fn: JFunction[? >: T, ? <: U]): CompletableFuture[U] = thenApplyAsync(fn)

    /** TODO FILL IN
     *
     *  @param fn TODO FILL IN
     *  @return TODO FILL IN
     */
    override def thenAccept(fn: Consumer[? >: T]): CompletableFuture[Void] = thenAcceptAsync(fn)

    /** TODO FILL IN
     *
     *  @param fn TODO FILL IN
     *  @return TODO FILL IN
     */
    override def thenRun(fn: Runnable): CompletableFuture[Void] = thenRunAsync(fn)

    /** TODO FILL IN
     *
     *  @tparam U TODO FILL IN
     *  @tparam V TODO FILL IN
     *  @param cs TODO FILL IN
     *  @param fn TODO FILL IN
     *  @return TODO FILL IN
     */
    override def thenCombine[U, V](cs: CompletionStage[? <: U], fn: BiFunction[? >: T, ? >: U, ? <: V]): CompletableFuture[V] = thenCombineAsync(cs, fn)

    /** TODO FILL IN
     *
     *  @tparam U TODO FILL IN
     *  @param cs TODO FILL IN
     *  @param fn TODO FILL IN
     *  @return TODO FILL IN
     */
    override def thenAcceptBoth[U](cs: CompletionStage[? <: U], fn: BiConsumer[? >: T, ? >: U]): CompletableFuture[Void] = thenAcceptBothAsync(cs, fn)

    /** TODO FILL IN
     *
     *  @param cs TODO FILL IN
     *  @param fn TODO FILL IN
     *  @return TODO FILL IN
     */
    override def runAfterBoth(cs: CompletionStage[?], fn: Runnable): CompletableFuture[Void] = runAfterBothAsync(cs, fn)

    /** TODO FILL IN
     *
     *  @tparam U TODO FILL IN
     *  @param cs TODO FILL IN
     *  @param fn TODO FILL IN
     *  @return TODO FILL IN
     */
    override def applyToEither[U](cs: CompletionStage[? <: T], fn: JFunction[? >: T, U]): CompletableFuture[U] = applyToEitherAsync(cs, fn)

    /** TODO FILL IN
     *
     *  @param cs TODO FILL IN
     *  @param fn TODO FILL IN
     *  @return TODO FILL IN
     */
    override def acceptEither(cs: CompletionStage[? <: T], fn: Consumer[? >: T]): CompletableFuture[Void] = acceptEitherAsync(cs, fn)

    /** TODO FILL IN
     *
     *  @param cs TODO FILL IN
     *  @param fn TODO FILL IN
     *  @return TODO FILL IN
     */
    override def runAfterEither(cs: CompletionStage[?], fn: Runnable): CompletableFuture[Void] = runAfterEitherAsync(cs, fn)

    /** TODO FILL IN
     *
     *  @tparam U TODO FILL IN
     *  @param fn TODO FILL IN
     *  @return TODO FILL IN
     */
    override def thenCompose[U](fn: JFunction[? >: T, ? <: CompletionStage[U]]): CompletableFuture[U] = thenComposeAsync(fn)

    /** TODO FILL IN
     *
     *  @param fn TODO FILL IN
     *  @return TODO FILL IN
     */
    override def whenComplete(fn: BiConsumer[? >: T, ? >: Throwable]): CompletableFuture[T] = whenCompleteAsync(fn)

    /** TODO FILL IN
     *
     *  @tparam U TODO FILL IN
     *  @param fn TODO FILL IN
     *  @return TODO FILL IN
     */
    override def handle[U](fn: BiFunction[? >: T, Throwable, ? <: U]): CompletableFuture[U] = handleAsync(fn)

    /** TODO FILL IN
     *
     *  @param fn TODO FILL IN
     *  @return TODO FILL IN
     */
    override def exceptionally(fn: JFunction[Throwable, ? <: T]): CompletableFuture[T] = {
      val cf = new CompletableFuture[T]
      whenCompleteAsync((t, e) => {
          if (e == null) cf.complete(t)
          else {
            val n: AnyRef =
              try {
                fn(e).asInstanceOf[AnyRef]
              } catch {
                case thr: Throwable =>
                  cf.completeExceptionally(thr)
                  this
              }
            if (n ne this) cf.complete(n.asInstanceOf[T])
          }
        }
      )
      cf
    }

    /**
      * @inheritdoc
      *
      * WARNING: completing the result of this method will not complete the underlying
      *          Scala Future or Promise (ie, the one that that was passed to `toJava`.)
      */
    override def toCompletableFuture: CompletableFuture[T] = this

    /** TODO FILL IN
     *
     *  @param value TODO FILL IN
     */
    override def obtrudeValue(value: T): Unit = throw new UnsupportedOperationException("obtrudeValue may not be used on the result of toJava(scalaFuture)")

    /** TODO FILL IN
     *
     *  @param ex TODO FILL IN
     */
    override def obtrudeException(ex: Throwable): Unit = throw new UnsupportedOperationException("obtrudeException may not be used on the result of toJava(scalaFuture)")

    /** TODO FILL IN */
    override def get(): T = scala.concurrent.blocking(super.get())

    /** TODO FILL IN
     *
     *  @param timeout TODO FILL IN
     *  @param unit TODO FILL IN
     *  @return TODO FILL IN
     */
    override def get(timeout: Long, unit: TimeUnit): T = scala.concurrent.blocking(super.get(timeout, unit))

    /** TODO FILL IN */
    override def toString(): String = super[CompletableFuture].toString
  }

  /** TODO FILL IN
   *
   *  @tparam T TODO FILL IN
   *  @param wrapped TODO FILL IN
   */
  final class P[T](val wrapped: CompletionStage[T]) extends DefaultPromise[T] with BiFunction[T, Throwable, Unit] {
    /** TODO FILL IN
     *
     *  @param v TODO FILL IN
     *  @param e TODO FILL IN
     */
    override def apply(v: T, e: Throwable): Unit = {
      if (e == null) success(v)
      else failure(e)
    }
  }
}

