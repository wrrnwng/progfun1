object lists {
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  val list = List(0, 1, 2, 3, 4)
  val initList = init(list)

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  val twoLists = concat(List(1, 2, 3, 4), List('a', 'b', 'c'))

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => List()
    case y :: ys => reverse(ys) ::: List(y)
  }

  val reversed = reverse(twoLists)

  def removeAt[T](xs: List[T], n: Int) = xs.take(n) ::: (xs.drop(n + 1))

  val removed = removeAt(List('a', 'b', 'c', 'd'), 1)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => List()
    case z :: zs => (z match {
      case l: List[Any] => flatten(l)
      case i: Any => List(i)
    }) ::: flatten(zs)
  }

  val flatList = flatten(List(List(1, 1), 2, List(3, List(5, 8)), List()))
}