object polynomials {

  class Poly(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    val terms = terms0 withDefaultValue 0.0

    def +(other: Poly) = new Poly((other.terms foldLeft terms) (addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }


    override def toString = (for ((exp, coeff) <- terms.toList.sorted.reverse) yield s"${coeff}x^$exp") mkString " + "
  }

  val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
  val p2 = new Poly(0 -> 3.0, 3 -> 7.0)

  p1 + p2

  val letters = "letters"
  val grouped = letters groupBy (c => c)
  val list: List[(Char, Int)] = grouped.toList.map(g => (g._1, g._2.length))


  val longWord = List("Halp", "me", "plz").mkString("")

  val newList: List[(Char, Int)] = (for {
    grouped <- letters groupBy (c => c)
  } yield (grouped._1, grouped._2.length)).toList

  val list2 = List(('a', 2), ('b', 2))

  def combinations(occurrences: List[(Char, Int)]): List[List[(Char, Int)]] =
}