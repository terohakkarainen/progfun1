val word = "Hiihoo"
word.toLowerCase().groupBy(c => c).mapValues(s => s.length).toList.sorted

val dict = List("fa", "ff", "af")
dict.groupBy(w => forcomp.Anagrams.wordOccurrences(w))
