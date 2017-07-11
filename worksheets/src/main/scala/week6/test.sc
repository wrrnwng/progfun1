object test {
  val xs = Array(1, 2, 3, 44)
  xs map (_ * 2)

  val r: Range = 1 until 5
  val s: Range = 1 to 5

  val str = "Hello, world!"

  r foreach println
  s foreach println

  val pairs = List(1, 2, 3) zip List('a', 'n', 'x')

  val z = r flatMap (x => (1 to 15) map (y => (x, y)))

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map { case (x, y) => x * y }.sum
}
