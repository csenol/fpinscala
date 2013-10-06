package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import scala.util.Either

trait Applicative[F[_]] extends Functor[F] {

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(apply(unit(f.curried))(fa))(fb)

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]  = map2(fab, fa)((fab, fa) => fab(fa))

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)


//ex2 map2, apply, unit
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)( x => x)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List[B]())) ((x,acc) => map2(f(x), acc)( _ :: _) )

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((a:A,b:B) => (a,b))
//ex2

//ex7
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = { 
    val self = this
    new  Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =  (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

//ex8
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = { 
    val self = this
    new  Applicative[({type f[x] = F[G[x]]})#f] { 
      def unit[A](a: => A) = self.unit(G.unit(a))
      override def apply[A,B](fs: F[G[ A=> B]])(p: F[G[A]]) = map2(fs, p)((x,y) => x.apply(y))
    }
  }

//ex9
  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = ofa.keys.toList.foldRight(unit(Map[K,V]()))((x,acc) => map2(ofa(x), acc)( (a ,b) => b +  (x -> a)))
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[M[_]] extends Applicative[M] {
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: M[A => B])(ma: M[A]): M[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {

def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
  new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A ,B](eea: Either[E, A])(f: A => Either[E, B]): Either[E,B] = eea match {
      case Right(a) => f(a)
      case Left(b) => Left(b)
    }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[M[_],N[_]](implicit M: Monad[M], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = M[N[x]]})#f] = new Monad[({type f[x] = M[N[x]]})#f] {
      override def unit[A](a: => A): M[N[A]] = M.unit(N.unit(a))
      override def flatMap[A,B](mna: M[N[A]])(f: A => M[N[B]]): M[N[B]] = M.flatMap(mna)(na => M.map(T.sequence(N.map(na) ((a:A) => f(a))))( k=> N.join(k)))
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  def validationApplicative[E] = 
    new Applicative[({type f[x] = Validation[E,x]})#f] {
      override def unit[A](a: => A): Validation[E,A] =  Success(a)
      override def map2[A,B,C](fa: Validation[E,A], fb: Validation[E,B])(f: (A, B) => C): Validation[E,C] = (fa, fb) match {
	case (Success(a), Success(b)) => Success(f(a,b))
	case (Success(a), x@Failure(b, bs)) => x
	case (x@Failure(b, bs), Success(a)) => x
	case (Failure(a, as), Failure(b, bs)) => Failure(a, as ++ Vector(b) ++ bs)
      } 

  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[M[_]:Applicative,A,B](fa: F[A])(f: A => M[B]): M[F[B]] =
    sequence(map(fa)(f))
  def sequence[M[_]:Applicative,A](fma: F[M[A]]): M[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa))((a,s) => (s.head, s.tail))._1

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = mapAccum(fa, z)((a,s) => ((), f(s, a)))._2

  def fuse[M[_],N[_],A,B](fa: F[A])(f: A => M[B], g: A => N[B])
                         (implicit M: Applicative[M], N: Applicative[N]): (M[F[B]], N[F[B]]) = 
  traverse[({type f[x] = (M[x], N[x])})#f, A, B](fa)(a => (f(a), g(a)))(M product N)
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Traverse[({type f[x] = F[G[x]]})#f] {
        override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]): M[F[G[B]]] = self.traverse(fa)(ga => G.traverse(ga)(f))
    }

  }
}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[M[_],A,B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      as.foldLeft(M.unit(List[B]())) ((acc, x) => M.map2(f(x), acc)( _ ::_  ))
  }
  val optionTraverse = new Traverse[Option] {
    override def traverse[M[_],A,B](as: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
      as.fold(M.unit(Option.empty[B]))(x => M.map(f(x))(Option(_)))
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[M[_],A,B](as: Tree[A])(f: A => M[B])(implicit M: Applicative[M]): M[Tree[B]] = 
     M.map2(f(as.head), listTraverse.traverse(as.tail)( ta => traverse(ta)(f)))((a,b) => Tree(a,b))
  }
      
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
