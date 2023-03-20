package u03

import org.junit.*
import org.junit.Assert.*
import u03.Lists.List
import u03.Persons.Person

class PersonTest extends LambdaTest:
  import List.*
  import Person.*

  @Test def testCourses() =
    assertEquals(Cons("a", Cons("b", Nil())), courses(Cons(Student("Mario", 2000), Cons(Teacher("Gianni", "a"), Cons(Teacher("Mario", "b"), Nil())))))
    assertEquals(Nil(), courses(Cons(Student("Mario", 2000), Cons(Student("Gianni", 2001), Cons(Student("Mario", 1999), Nil())))))

