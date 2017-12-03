import scala.annotation.tailrec
// High order functions and Currying

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

sum(x => x)(1, 5)

def product(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc * f(a))
  }
  loop(a, 1)
}

product(x => x)(1, 5)

def factorial(n: Int) = product(x => x)(1, n)
factorial(4)

// A more general function that generalizes both sum and product.
// What we want is a map reduce function, which map
// values in the interval to new values, and the reduce would combine
// them
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if(a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a+ 1, b))
}

def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
def factorial2(n: Int) = product2(x => x)(1, n)

factorial2(4)