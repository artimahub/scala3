package scala.util

import language.experimental.captureChecking

import scala.annotation.implicitNotFound
import scala.annotation.experimental

/** Type class relating a `FunctionN[..., R]` with an equivalent tupled function `Function1[TupleN[...], R]`
 *
 *  @tparam F a function type
 *  @tparam G a tupled function type (function of arity 1 receiving a tuple as argument)
 */
@implicitNotFound("${F} cannot be tupled as ${G}")
@experimental
sealed trait TupledFunction[F, G]:
  /** TODO FILL IN
   *
   *  @param f TODO FILL IN
   *  @return TODO FILL IN
   */
  def tupled(f: F): G
  /** TODO FILL IN
   *
   *  @param g TODO FILL IN
   *  @return TODO FILL IN
   */
  def untupled(g: G): F

@experimental
private[scala] object TupledFunction:
  /** TODO FILL IN
   *
   *  @tparam F TODO FILL IN
   *  @tparam G TODO FILL IN
   *  @param tupledImpl TODO FILL IN
   *  @param untupledImpl TODO FILL IN
   *  @return TODO FILL IN
   */
  def apply[F, G](tupledImpl: F => G, untupledImpl: G => F): TupledFunction[F, G]^{tupledImpl, untupledImpl} =
    new TupledFunction[F, G]:
      def tupled(f: F): G = tupledImpl(f)
      def untupled(g: G): F = untupledImpl(g)
