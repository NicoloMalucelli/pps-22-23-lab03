package u03

import u02.Optionals.Option

object Lists extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:
    
    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = flatMap(l)((x: A) => Cons(mapper(x), Nil()))

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = flatMap(l1)((x: A) => x match
      case y if pred(y) => Cons(y, Nil())
      case _ => Nil()
    )

    def drop[A](l: List[A], n: Integer): List[A] = (l, n) match
      case (Nil(), _) => Nil()
      case (l, 0) => l
      case (Cons(h, t), l) => drop(t, l-1)

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Nil() => right
      case Cons(h, t) => Cons(h, append(t, right))

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => append(f(h), flatMap(t)(f))

    def max(l: List[Int]): Option[Int] = l match
      case Nil() => Option.None()
      case Cons(h, t) => (h, max(t)) match
        case (h, Option.None()) => Option.Some(h)
        case (h, Option.Some(x)) => (h, x) match
          case (v1, v2) if v1 >= v2 => Option.Some(v1)
          case (_, v2) => Option.Some(v2)

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
