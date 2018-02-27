import org.scalacheck.Gen.choose
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.{Arbitrary, Gen}


var test = forAll {
  (a: Int, b: Int) => (a + b) == (b + a)
}

test.check


var testAgain = forAll {
  a: Int => (a > 0 && a < 1000) ==> (Math.sqrt(a * a) == a)
}

testAgain.check


val myGen = for {
  n <- Gen.choose(10, 20)
  m <- Gen.choose(2 * n, 500)
} yield (n, m)


myGen.sample

val evenInteger = Arbitrary.arbitrary[Int] suchThat (_ % 2 == 0)
evenInteger.sample
evenInteger.sample


val squares = for {
  xs <- Arbitrary.arbitrary[List[Int]]
} yield xs.map(x => x * x)
squares.sample

val genInt = for {
  j <- Gen.choose(1, 1000)
} yield j

genInt.sample.get

