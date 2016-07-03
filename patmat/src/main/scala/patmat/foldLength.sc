def foldLength[T](xs: List[T]): Int =
  (xs foldRight 0)((x, acc) => acc + 1)

foldLength(List(9, 9, 9, 9, 9, 9))