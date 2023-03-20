package u03

object Persons {

  import Lists.*

  enum Person: // a sum type defined by enumerating various cases
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

    def courses(persons: List[Person]): List[String] = List.flatMap(persons)(p => p match
      case Student(_, _) => List.Nil()
      case Teacher(_, course) => List.Cons(course, List.Nil()))
}
