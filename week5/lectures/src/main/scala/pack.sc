def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: _ =>
    val (first, rest) = xs.span(y => y == x)
    first :: pack(rest)
}

def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs).map(item => (item.head, item.length))
}

val chars = List("a", "a", "a", "b", "c", "c", "a")
pack(chars)
encode(chars)
