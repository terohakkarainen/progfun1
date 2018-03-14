def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((_, n) => n + 1)

val intList = List(1, 2, 3)
mapFun[Int, Int](intList, x => x * x)
lengthFun(intList)
