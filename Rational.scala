class Rational(x: Int, y: Int){
  require(y != 0, "denom must be positive")

  def numer = x
  def denom = y

  def +(that: Rational) = new Rational(
    numer * that.denom + that.numer * denom,
    denom * that.denom
  )

  override def toString = {
    val g = gcd(numer, denom)
    numer/g + "/" + denom/g
  }

  def neg: Rational = new Rational(-numer, denom)
  def unary_- : Rational = new Rational(-numer, denom)
  def - (x: Rational) = this + -x
  def < (x: Rational) = numer * x.denom < x.numer * denom
  def max(x: Rational) = if(this < x) x else this

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
}

val one = new Rational(10,90)
val two = new Rational(3,5)

two + one
one - two
one.neg
one < two
two max one




