import forcomp.Anagrams.{Occurrences, Word, dictionaryByOccurrences}
import forcomp.Dictionary

val dictionary: List[Word] = Dictionary.loadDictionary

def wordOccurrences(w: String) = {
  val wLower = w.toLowerCase()
  val occurrencesToMap = wLower groupBy (c => wLower.count(_ == c))

  ((for {
    (digit, chars) <- occurrencesToMap
    char <- chars
  } yield char -> digit).toList).sorted
}

def sentenceOccurrences(s: List[String])  = {
  s flatMap wordOccurrences
}

lazy val dictionaryByOccurrences: Map[List[(Char, Int)], List[String]] =
{
  dictionary groupBy wordOccurrences
}

def wordAnagrams(word: String): List[String] =
  dictionaryByOccurrences.get(wordOccurrences(word)) match {
    case Some(anagrams) => anagrams
    case None => List()
  }

def subset(nums: List[Int]): List[List[Int]] = {
  def subsetHelper(nums: List[Int]): List[List[Int]] = {
    if (nums.isEmpty) List(List())
    else {
      for {
        i <- 0 until nums.length
        current = nums(i)
        others <- subsetHelper(nums.take(i-1) ::: nums.drop(i+1))
      } yield current :: others
    }.toList
  }

  val firstRes = subsetHelper(nums)

  val otherNums = for (num <- nums) yield List(num)
  List() :: otherNums ::: firstRes
}

def powerSet(nums: List[Int]): List[List[Int]] = {
  def helper(nums: List[Int], acc:List[List[Int]]): List[List[Int]] = nums match {
    case Nil => acc
    case x :: xs => helper(xs, acc ::: (acc map (x :: _)))
  }
  helper(nums, List(List()))
}

def combinations(occurrences: List[(Char, Int)]): List[List[(Char, Int)]]
= {
  if (occurrences.isEmpty) List(List())
  else {
    val (char, count) = occurrences.head
    for {
      index  <- count to 0 by -1
      remaining <- combinations(occurrences.tail)
    } yield ((char, index) :: remaining) filter (el => el._2 >= 1)
  }.toList
}


val x = combinations(List(('a', 2), ('b', 2), ('c', 1)))

x.foreach(println(_))
