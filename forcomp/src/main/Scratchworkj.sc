
def wordOccurrences(w: String) = {
  val wLower = w.toLowerCase()
  val occurrencesToMap = wLower groupBy (c => wLower.count(_ == c))

  ((for {
    (digit, chars) <- occurrencesToMap
    char <- chars
  } yield char -> digit) toList) sorted
}

val x = wordOccurrences("Robert")

val name = "robert"
