object pairs {
  def isPrime(n: Int) = (2 until n) forall (n % _ != 0)

  val n = 7

  val messTest = (1 until n) flatMap (i =>
    (1 until i) map (j => (i, j))) filter (pair => isPrime(pair._1 + pair._2))

  val cleanTest = for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)

  case class Person(name: String, age: Int)

  val persons = (1 until n * 4) map (n => new Person(s"$n", n))

  val oldPeople1 = for (p <- persons if p.age > 20) yield p.name

  val oldPeople2 = persons filter (p => p.age > 20) map (p => p.name)

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (for ((x, y) <- xs zip ys) yield x * y).sum
}