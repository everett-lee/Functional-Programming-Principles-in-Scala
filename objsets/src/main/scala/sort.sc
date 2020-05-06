def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys =>
    if (x <= y) x :: xs
    else y :: insert(x, ys)
}

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}


val nums = List(2, 3,100,3,2,-100,5,11,2);

println(isort(nums))