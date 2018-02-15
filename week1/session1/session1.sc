import Math.abs

def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def isGoodEnough(guess: Double, x: Double) =
  abs(guess * guess - x) < (x * 0.001)

def improve(guess: Double, x: Double) =
  (guess + x / guess) / 2

def sqrt(x: Double) = sqrtIter(1.0, x)

sqrt(0.001)
sqrt(1.0e-20)

// ---

def factorial(n: Int): Int = {

  def loop(sum: Int, n: Int): Int =
    if (n == 0) sum
    else loop(sum * n, n - 1)

  loop(1, n)
}

factorial(4)
1 * 2 * 3 * 4

// ---
