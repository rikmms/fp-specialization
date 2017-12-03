class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)

  private val g = gcd(x, y)
  def numer = x / g
  def denom = y / g

  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if(this < that) that else this

  def add(that: Rational) = new Rational(
    numer * that.denom + that.numer * denom,
    denom * that.denom)

  // Prefix operators
  def unary_- : Rational = new Rational(-numer, denom)

  def sub(that: Rational) = add(-that)

  override def toString = numer + "/" + denom
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.sub(y).sub(z)

// With Infix notation
x sub y sub z
// With relaxed Identifiers
x < y

y.add(y)
x.max(y)

val strange = new Rational(1)
strange.add(strange)