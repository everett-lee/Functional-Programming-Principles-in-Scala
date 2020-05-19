import forcomp.Anagrams.Word
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

val name = List("Hi", "whats", "up")
sentenceOccurrences(name)
println(dictionary)
val x = dictionaryByOccurrences(List(('e', 1), ('i', 1), ('l', 1), ('o', 1), ('v', 1)))

println(x)