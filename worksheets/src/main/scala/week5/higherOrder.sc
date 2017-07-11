object higherOrder {
  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case head :: tail => head * head :: squareList(tail)
  }

  def squareListMap(xs: List[Int]) = xs map(x => x * x)

  val list = List(1, 2, 3)
  val same = squareList(list) == squareListMap(list)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case List() => List()
    case x :: xs1 => {
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
    }
  }

  val letters = List('a', 'a', 'a', 'b', 'c', 'c', 'a')
  val packedLetters = pack(letters)

  def encode[T](xs: List[T]): List[(T, Int)] = pack(xs) map(x => (x.head, x.length))

  val encoded = encode(letters)

  def concat[T](xs: List[T], ys: List[T]): List[T] = (xs foldRight  ys)(_ :: _)
  val twoLists = concat(letters, letters.reverse)
}