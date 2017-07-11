object pairsAndTuples {
  def mergeSort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (List(), _) => ys
        case (_, List()) => xs
        case (xHead :: xTail, yHead :: yTail) =>
          if (lt(xHead,yHead)) xHead :: merge(xTail, ys)
          else yHead :: merge(xs, yTail)
      }
      val (first, second) = xs splitAt(n)
      merge(mergeSort(first)(lt), mergeSort(second)(lt))
    }
  }

  val sortedList = mergeSort(List(928, 44, 912, 82, 82834, 83, 99284, -82, 70, 73, 1084))((x, y) => x < y)

  val fruits = mergeSort(List("apple", "pineapple", "banana", "pen"))((x, y) => (x compareTo y) < 0)
}