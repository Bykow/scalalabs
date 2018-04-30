import Anagrams._

import scala.collection.immutable.List

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  subseqs(fingerPrint(sentence)) map (word => wordAnagrams(word))
}

sentenceAnagrams(List("eat", "tea"))