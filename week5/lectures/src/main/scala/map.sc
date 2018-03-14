def squareListPatternMatch(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => y * y :: squareListPatternMatch(ys)
}

def squareListMap(xs: List[Int]): List[Int] =
  xs.map(x => x * x)

val intList = List(1, 2, 3)
squareListPatternMatch(intList)
squareListMap(intList)
