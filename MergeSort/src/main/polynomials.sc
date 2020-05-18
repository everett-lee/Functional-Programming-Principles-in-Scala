object polynomials {

  class Poly(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    val terms = terms0 withDefaultValue 0.0

    def +(other: Poly) = new Poly((other.terms foldLeft terms) (addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double]
    = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }

    override def toString = {
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString (" + ")
    }
  }

}

val p1 = new polynomials.Poly(1 -> 2.0, 5 -> 5.0, 2 -> 3.5)
val p2 = new polynomials.Poly(1 -> 2.2, 3 -> 1.2, 2 -> 1.5)

p1 + p2