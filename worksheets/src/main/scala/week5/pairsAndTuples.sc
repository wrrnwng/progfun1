object pairsAndTuples {
  import math.Ordering

  def mergeSort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (List(), _) => ys
        case (_, List()) => xs
        case (xHead :: xTail, yHead :: yTail) =>
          if (ord.lt(xHead, yHead)) xHead :: merge(xTail, ys)
          else yHead :: merge(xs, yTail)
      }
      val (first, second) = xs splitAt(n)
      merge(mergeSort(first), mergeSort(second))
    }
  }

  val sortedList = mergeSort(List(928, 44, 912, 82, 82834, 83, 99284, -82, 70, 73, 1084))

  val fruits = mergeSort(List("apple", "pineapple", "banana", "pen"))
}