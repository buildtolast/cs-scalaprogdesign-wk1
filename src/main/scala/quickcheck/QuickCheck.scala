package quickcheck

// Doc comment
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = Gen.oneOf(
    for {
      f <- choose(1, 100)
    } yield insert(f, empty),
    for {
      e <- choose(101, 200)
      g <- choose(200, 300)
    } yield insert(e, insert(g, empty))
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    println(h)
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert") = forAll {
    a: Int =>
      findMin(insert(a, insert(2, empty))) == 2
  }

}
