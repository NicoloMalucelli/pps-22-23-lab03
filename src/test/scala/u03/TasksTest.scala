package u03

import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Tasks.*
import u03.Lists.*
import u02.Optionals.*

class TasksTest {

  import List.Cons
  import List.Nil
  import Option.Some
  import Option.None
  import Task1.*
  import Task2.*

  //TASK 1

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 5))

  @Test def testAppend() =
    val tail = Cons(40, Nil())
    assertEquals(tail, append(Nil(), tail))
    assertEquals(Cons(10, Cons(20, Cons(30, tail))), append(l, tail))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  //TASK 2

  @Test def testMax() =
    assertEquals(Option.Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(Option.Some(-1), max(Cons(-3, Cons(-15, Cons(-1, Nil())))))
    assertEquals(Option.None(), max(Nil()))

  //TASK 3

  import Persons.Person.*
  import Task3.*

  @Test def testCourses() =
    assertEquals(Cons("a", Cons("b", Nil())), courses(Cons(Student("Mario", 2000), Cons(Teacher("Gianni", "a"), Cons(Teacher("Mario", "b"), Nil())))))
    assertEquals(Nil(), courses(Cons(Student("Mario", 2000), Cons(Student("Gianni", 2001), Cons(Student("Mario", 1999), Nil())))))

  //TASK 4

  import Task4.*

  @Test def testFoldLeft() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))

  @Test def testFoldRight() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-8, foldRight(lst)(0)(_ - _))


  //TASK 5

  import Task5.*

  var str = Stream.iterate(0)(_ + 1)

  @Test def testDropStream() =
    assertEquals(Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Nil()))))), Stream.toList(Stream.take(Stream.drop(str)(5))(5)))

  //TASK 6

  import Task6.*

  @Test def testCons() =
    assertEquals(Cons(0, Cons(0, Cons(0, Cons(0, Cons(0, Nil()))))), Stream.toList(Stream.take(constant(0))(5)))

  //TASK 7

  import Task7.*

  @Test def testFibs() =
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))), Stream.toList(Stream.take(fibs)(8)))

}
