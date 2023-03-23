package u03

import org.junit.*
import org.junit.Assert.*

class StreamTest extends LambdaTest:
  import Streams.*
  import Lists.List.*

  var str = Stream.iterate(0)(_ + 1)

  @Test def testIterate() =
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(Stream.take(str)(5)))

  @Test def testMap() =
    assertEquals(Cons(0, Cons(2, Cons(4, Cons(6, Cons(8, Nil()))))), Stream.toList(Stream.take(Stream.map(str)(_*2))(5)))

  @Test def testFilter() =
    assertEquals(Cons(1, Cons(3, Cons(5, Cons(7, Cons(9, Nil()))))), Stream.toList(Stream.take(Stream.filter(str)(_%2==1))(5)))

  @Test def testDrop() =
    assertEquals(Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Nil()))))), Stream.toList(Stream.take(Stream.drop(str)(5))(5)))

  @Test def testCons() =
    assertEquals(Cons(0, Cons(0, Cons(0, Cons(0, Cons(0, Nil()))))), Stream.toList(Stream.take(Stream.constant(0))(5)))