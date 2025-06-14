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

def mergeLists[A](list1: List[A], list2: List[A])(implicit mergeable: Mergeable[A]): A = {
  val requiredLength: Int = list1.length.max(list2.length)
  if (requiredLength == 0)
    return mergeable.empty

  val padded1 = padList(list1, requiredLength)
  val padded2 = padList(list2, requiredLength)

  padded1
    .zip(padded2)
    .map { case(a, b) => mergeable.combine(a, b) }
    .reduce((a, b) => mergeable.combineHyphenated(a, b))
}

def reduceList[A](list: List[A], chunkSize: Int)(implicit mergeable: Mergeable[A]): List[A] =
  if (list.isEmpty)
    return List.empty
  else
    list
      .take(chunkSize)
      .reduce((a, b) => mergeable.combine(a, b)) :: reduceList(list.drop(chunkSize), chunkSize)

def hyperMerge[A](list1: List[A], list2: List[A], m: Int, n: Int)(implicit mergeable: Mergeable[A]): A = {
  val reduced1 = reduceList(list1, m)
  val reduced2 = reduceList(list2, n)

  mergeLists(reduced1, reduced2)
}

println("padList() tests")
assert(padList(List.empty, 0) == List.empty)
assert(padList(List.empty, 1) == List(""))
assert(padList(List.empty, 2) == List("", ""))
assert(padList(List("A", "B", "C"), 0) == List("A", "B", "C"))
assert(padList(List("A", "B", "C"), 2) == List("A", "B", "C"))
assert(padList(List("A", "B", "C"), 5) == List("A", "B", "C", "", ""))

println("mergeLists() tests")
assert(mergeLists(List.empty, List.empty) == "")
assert(mergeLists(List("A", "B", "C"), List.empty) == "A-B-C")
assert(mergeLists(List.empty, List("1", "2", "3")) == "1-2-3")
assert(mergeLists(List("A", "B", "C"), List("1", "2", "3")) == "A1-B2-C3")
assert(mergeLists(List("A", "B", "C"), List("1", "2")) == "A1-B2-C")
assert(mergeLists(List("A", "B"), List("1", "2", "3")) == "A1-B2-3")

println("reduceList() tests")
assert(reduceList(List.empty, 2) == List.empty)
assert(reduceList(List("t", "e", "s", "t"), 1) == List("t", "e", "s", "t"))
assert(reduceList(List("t", "e", "s", "t"), 2) == List("te", "st"))
assert(reduceList(List("t", "e", "s", "t"), 3) == List("tes", "t"))
assert(reduceList(List("t", "e", "s", "t"), 4) == List("test"))
assert(reduceList(List("t", "e", "s", "t"), 5) == List("test"))

println("hyperMerge() tests")
assert(hyperMerge(List("t", "e", "s", "t"), List("1", "2", "3", "4"), 1, 1) == "t1-e2-s3-t4")
assert(hyperMerge(List("t", "e", "s", "t"), List("1", "2", "3", "4"), 2, 1) == "te1-st2-3-4")
assert(hyperMerge(List("t", "e", "s", "t"), List("1", "2", "3", "4"), 3, 1) == "tes1-t2-3-4")
assert(hyperMerge(List("t", "e", "s", "t"), List("1", "2", "3", "4"), 4, 1) == "test1-2-3-4")
assert(hyperMerge(List("t", "e", "s", "t"), List("1", "2", "3", "4"), 5, 1) == "test1-2-3-4")
assert(hyperMerge(List("t", "e", "s", "t"), List("1", "2", "3", "4"), 1, 2) == "t12-e34-s-t")
assert(hyperMerge(List("t", "e", "s", "t"), List("1", "2", "3", "4"), 1, 3) == "t123-e4-s-t")
assert(hyperMerge(List("t", "e", "s", "t"), List("1", "2", "3", "4"), 1, 4) == "t1234-e-s-t")
assert(hyperMerge(List("t", "e", "s", "t"), List("1", "2", "3", "4"), 1, 4) == "t1234-e-s-t")
assert(hyperMerge(List("t", "e", "s", "t"), List("1", "2", "3", "4"), 2, 2) == "te12-st34")
assert(hyperMerge(List("t", "e", "s", "t"), List("1", "2", "3", "4"), 3, 2) == "tes12-t34")
assert(hyperMerge(List("t", "e", "s", "t"), List("1", "2", "3", "4"), 2, 3) == "te123-st4")
