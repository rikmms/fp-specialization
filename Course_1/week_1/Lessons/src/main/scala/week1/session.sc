import scala.annotation.tailrec

def abs(x:Double) = if (x < 0) -x else x

def sqrt(x: Double) = {

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double) =
    (abs(guess * guess / x) / x) < 0.001

  def improve(guess: Double) = (guess + x / guess) / 2

  sqrtIter(1.0)
}


// sqrt(2)
// sqrt(4)
// sqrt(1e-6)
// sqrt(1e60)
def factorial(n: Int): Int = {
  if (n == 0) 1
  else n* factorial(n-1)
}

factorial(0)
factorial(4)
// factorial(25000) StackOverflow

// with tail recursion
def factorialTL(n: Int): Int = {

  // create auxiliary function which is tail recursive
  // The @tailrec annotation serves to compiler check if the function is
  // an tail recursive function
  @tailrec
  def loop(acc: Int, n: Int):Int =
    if (n == 0) acc
    else
      loop(acc*n, n-1)

  // initializes the loop with 1
  loop(1, n)
}

factorialTL(0)
factorialTL(4)
factorialTL(25000) // use same stack. So, don't have the stack frames limitation