package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    def id: Int => Boolean = (x: Int) => x == x
    def isEven: Int => Boolean = (x: Int) => x % 2 == 0
    def isOdd: Int => Boolean = (x: Int) => x % 2 != 0
    def isPositive: Int => Boolean = (x: Int) => x > 0
    def isNegative: Int => Boolean = (x: Int) => x < 0

    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(-2)
    val s6 = singletonSet(0)
    val s7 = singletonSet(-100)
    val s8 = singletonSet(42)

    val s9 = union(union(s1, s2), s3)
    val s10 = union(union(s4, s5), s6)
    val s11 = union(s7, s8)

    val all = union(union(s9, s10), s11)
    val evens = union(union(s2, s4), s5)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")


      assert(!contains(s2, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `union contains a union of unions`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect of s1 and s2 contains neither element`: Unit = {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Union 1")
      assert(!contains(s, 2), "Union 2")
    }
  }

  @Test def `intersect of set and itself contains one element`: Unit = {
    new TestSets {
      val s = intersect(s1, s1)
      assert(contains(s, 1), "Union 1")
      assert(!contains(s, 2), "Union 2")
    }
  }

  @Test def `difference of {1} and {2} is {1}`: Unit = {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(!contains(s, 2), "Union 2")
    }
  }

  @Test def `difference of {1} and {1} does not contain 1`: Unit = {
    new TestSets {
      val s = diff(s1, s1)
      assert(!contains(s, 1), "Union 1")
    }
  }

  @Test def `difference of {1, 2} and {1, 3} contains 2 but not 1 or 3`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      val t = union(s1, s3)
      val z = diff(s, t)

      assert(contains(z, 2))
      assert(!contains(z, 1))
      assert(!contains(z, 3))
    }
  }

  @Test def `filter all for evens gives {2, 4, -2, 0, -100, 42}`: Unit = {
    new TestSets {
      val s = filter(all, x => x % 2 == 0)

      assert(contains(s, 2))
      assert(contains(s, 4))
      assert(contains(s, -2))
      assert(contains(s, 0))
      assert(contains(s, -100))
      assert(contains(s, 42))

      assert(!contains(s, 1))
      assert(!contains(s, -3))
    }
  }

  @Test def `filter of <= 0 is {-2, 0, -100}`: Unit = {
    new TestSets {
      val s = filter(all, x => x <= 0)

      assert(contains(s, -2))
      assert(contains(s, 0))
      assert(contains(s, -100))

      assert(!contains(s, 1))
      assert(!contains(s, -3))
      assert(!contains(s, 42))
    }
  }

  @Test def `passes for id`: Unit = {
    new TestSets {

      val result = forall(all, id)
      assert(result)
    }
  }

  @Test def `true for boundary ints`: Unit = {
    new TestSets {

      val minus1000 = singletonSet(-1000)
      val plus1000 = singletonSet(1000)

      assert(forall(minus1000, id))
      assert(forall(plus1000, id))
    }
  }

  @Test def `forAll + isPositive should be false`: Unit = {
    new TestSets {

      val result = forall(all, isPositive)
      assert(!result)
    }
  }

  @Test def `forAll + isBelow500 should be true`: Unit = {
    new TestSets {
      def isBelow500: Int => Boolean = (x: Int) => x < 500

      val result = forall(all, isBelow500)
      assert(result)
    }
  }

  @Test def `forAll + evens + isEven should be true`: Unit = {
    new TestSets {

      val result = forall(evens, isEven)
      assert(result)
    }
  }

  @Test def `forAll + evens + isOdd should be false`: Unit = {
    new TestSets {

      val result = forall(evens, isOdd)
      assert(!result)
    }
  }

  @Test def `exists + evens + isEven should be true`: Unit = {
    new TestSets {

      val result = exists(evens, isEven)
      assert(result)
    }
  }

  @Test def `exists + evens + isOdd should be false`: Unit = {
    new TestSets {

      val result = exists(evens, isOdd)
      assert(!result)
    }
  }

  @Test def `exists of {3, 1002} + isEven should be false`: Unit = {
    new TestSets {
      val s = union(singletonSet(3), singletonSet(1002))

      val result = exists(s, isEven)
      assert(!result)
    }
  }

  @Test def `exists + all + isNegative should be true`: Unit = {
    new TestSets {

      val result = exists(all, isNegative)
      assert(result)
    }
  }

  @Test def `map + all + toOne should not contain any other numbers`: Unit = {
    new TestSets {

      val s = map(all, (x: Int) => 1)
      assert(!exists(s, (x: Int) => x != 1))
    }
  }

  @Test def `map + all + double should not contain {2, 4, 6, 8, -4, 0, -200, 84} `: Unit = {

    new TestSets {

      val s = map(all, (x: Int) => x * 2)

      assert(contains(s, 2))
      assert(contains(s, 4))
      assert(contains(s, 6))
      assert(contains(s, 8))
      assert(contains(s, -4))
      assert(contains(s, 0))
      assert(contains(s, -200))
      assert(contains(s, 84))
    }
  }

  @Test def `map + all + abs should not contain negatives `: Unit = {

    new TestSets {

      def abs: Int => Int = (x: Int) => if (x < 0) -x else x
      val s = map(all, (x: Int) => abs(x))

      printSet(s)

      assert(!exists(s, x => x < 0))
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
