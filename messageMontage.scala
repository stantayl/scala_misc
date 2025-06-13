import scala.annotation.tailrec

trait Mergeable[A] {
  def empty: A
  def combine(x: A, y: A): A
  def combineHyphenated(x: A, y: A): A
}

implicit object StringMergeable extends Mergeable[String] {
  def empty: String = ""
  def combine(a: String, b: String): String = a + b
  def combineHyphenated(a: String, b: String): String = a + "-" + b
}

@tailrec
def padList[A](list: List[A], requiredLength: Int)(implicit mergeable: Mergeable[A]): List[A] =
  if (list.length >= requiredLength) list else padList(list :+ mergeable.empty, requiredLength)

//padList(List("A", "B", "C"), 5)


def mergeLists[A](list1: List[A], list2: List[A])(implicit mergeable: Mergeable[A]): A = {
  val requiredLength: Int = list1.length.max(list2.length)
  val padded1 = padList(list1, requiredLength)
  val padded2 = padList(list2, requiredLength)

  padded1
    .zip(padded2)
    .map { case(a, b) => mergeable.combine(a, b) }
    .reduce((a, b) => mergeable.combineHyphenated(a, b))
}

//val merged = mergeLists(List("t", "e", "s", "t"), List("1", "2"))
//println(s"MERGED: $merged")

def reduceList[A](list: List[A], chunkSize: Int)(implicit mergeable: Mergeable[A]): List[A] =
  if (list.isEmpty)
    return List.empty
  else
    list
      .take(chunkSize)
      .reduce((a, b) => mergeable.combine(a, b)) :: reduceList(list.drop(chunkSize), chunkSize)

//val reduced = reduceList(List("t", "e", "s", "t"), 2)
//println(s"REDUCED: $reduced")

def hyperMerge[A](list1: List[A], list2: List[A], m: Int, n: Int)(implicit mergeable: Mergeable[A]): A = {
  val reduced1 = reduceList(list1, m)
  val reduced2 = reduceList(list2, n)

  mergeLists(reduced1, reduced2)
}

val hypermerged = hyperMerge(List("t", "e", "s", "t"), List("1", "2", "3", "4"), 2, 1)
println(s"HYPERMERGED=$hypermerged")