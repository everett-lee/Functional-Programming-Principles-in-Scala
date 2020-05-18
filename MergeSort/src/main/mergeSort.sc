import scala.util.Random

val listOfNums =
  for (_ <- 1 to 10) yield
    Random.between(-100, 100)

def merge(xs: List[Int], ys: List[Int]): List[Int] =
  (xs, ys) match {
    case (Nil, ys) => ys
    case(xs, Nil) => xs
    case(x :: xs1, y :: ys1) =>
      if (x < y) x :: merge(xs1, ys)
      else y :: merge(xs, ys1)
}

def mergeSort(xs: List[Int]): List[Int] = {
  val index = xs.length / 2
  if (index == 0) xs
  else {
    val (left, right) = xs.splitAt(index)
    merge(mergeSort(left), mergeSort(right))
  }
}
