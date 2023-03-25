package u03

import u03.Lists.List
import u03.Lists.List.Cons
import u02.Optionals.*
import u03.Persons.*
import u03.Streams.Stream.{cons, empty}
import u03.Tasks.Task5.Stream.iterateOverTwo

import scala.annotation.tailrec

object Tasks:

  object Task1:

    import List.*

    @tailrec
    def drop[A](l: List[A], n: Integer): List[A] = (l, n) match
      case (Nil(), _) => Nil()
      case (l, 0) => l
      case (Cons(h, t), l) => drop(t, l - 1)

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Nil() => right
      case Cons(h, t) => Cons(h, append(t, right))

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => append(f(h), flatMap(t)(f))

    def map[A, B](l: List[A])(mapper: A => B): List[B] = flatMap(l)((x: A) => Cons(mapper(x), Nil()))

    def filter[A](l1: List[A])(p: A => Boolean): List[A] = flatMap(l1)((x: A) => x match
      case y if p(y) => Cons(y, Nil())
      case _ => Nil()
    )

  object Task2:

    import List.*
    import Option.*

    private def maxInt(a: Int, b:Int): Int = (a, b) match
      case (a, b) if a > b => a
      case _ => b

    def max(l: List[Int]): Option[Int] = l match
      case Nil() => Option.None()
      case Cons(h, t) => (h, max(t)) match
        case (h, Option.None()) => Option.Some(h)
        case (h, Option.Some(x)) => Option.Some(maxInt(h, x))

  object Task3:

    import Persons.Person.*

    def courses(persons: List[Person]): List[String] = List.flatMap(persons)(p => p match
      case Student(_, _) => List.Nil()
      case Teacher(_, course) => List.Cons(course, List.Nil()))

  object Task4:

    import List.*

    def foldLeft[A](l: List[A])(foldOver: A)(f: (A, A) => A): A = l match
      case Nil() => foldOver
      case Cons(h, t) => f(foldLeft(t)(foldOver)(f), h)

    def foldRight[A](l: List[A])(foldOver: A)(f: (A, A) => A): A = l match
      case Nil() => foldOver
      case Cons(h, t) => f(h, foldRight(t)(foldOver)(f))

  object Task5:

    enum Stream[A]:
      private case Empty()
      private case Cons(head: () => A, tail: () => Stream[A])

    object Stream:

      def empty[A](): Stream[A] = Empty()

      def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)

      def toList[A](stream: Stream[A]): List[A] = stream match
        case Cons(h, t) => List.Cons(h(), toList(t()))
        case _ => List.Nil()

      def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
        case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
        case _ => Empty()

      @tailrec
      def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
        case (Cons(_, tail), n) if n > 0 => drop(tail())(n - 1)
        case (Cons(head, tail), 0) => cons(head(), tail())
        case _ => Empty()

      def iterate[A](init: => A)(next: A => A): Stream[A] =
        cons(init, iterate(next(init))(next))

      def iterateOverTwo[A](v1: A)(v2: A)(f: (A, A) => A): Stream[A] = cons(v1, iterateOverTwo(v2)(f(v1, v2))(f))


  object Task6:

    import Task5.Stream

    def constant[A](v: A): Stream[A] = Stream.iterate(v)(v=>v)

  object Task7:

    import Task5.Stream
    import Task5.Stream.cons
    import Task5.Stream.empty

    val fibs: Stream[Int] = iterateOverTwo(0)(1)(_+_)

