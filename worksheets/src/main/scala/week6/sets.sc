object sets {
  val fruit: Set[String] = Set("apple", "banana", "pear")
  val s: Set[Int] = (1 to 6).toSet
  val s2 = s map (_ + 2)
  val oops = fruit.filter(_.startsWith("app"))
  val stuff = s.nonEmpty
}