import quickcheck.QuickCheckHeap

List[Int]().foldRight(Set[Int]())((next: Int, s: Set[Int]) => s + next)

List(3, 2, 1) == List(1, 2, 3)

List(3, 2, 1) == List(3, 2, 1)

List(3, 2, 1).sorted

Nil.reverse
