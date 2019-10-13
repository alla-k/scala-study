package funsets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
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
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s6 = singletonSet(6)
    val s12 = singletonSet(12)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

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
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
      assert(!contains(s, 4), "Union 4")
    }
  }


  test("intersect contains same elements of each set") {
    new TestSets {
      val sUnion = union(s2, s3)
      val sUnion2 = union (s1, s2)
      val sIntersect = intersect(sUnion, sUnion2)
      assert(contains(sIntersect, 2), "Intersect1")
      assert(!contains(sIntersect, 3), "Intersect2")
    }
  }


  test("diff contains different elements of each set") {
    new TestSets {
      val sUnion = union(s2, s3)
      val sUnion2 = union (s1, s2)
      val sDiff = diff(sUnion, sUnion2)
      assert(contains(sDiff, 1), "Diff1")
      assert(contains(sDiff, 3), "Diff2")
      assert(!contains(sDiff, 2), "Diff3")
    }
  }

  test("filter contains mapped elements of each set") {
    new TestSets {
      val sUnion = union(s2, s3)
      val sUnion2 = union (s1, s4)
      val sUnion3 = union (sUnion, sUnion2)
      val sMap = filter(sUnion3, ((p: Int) => (p%2==0)))
      assert(contains(sMap, 2), "Map1")
      assert(contains(sMap, 4), "Map2")
      assert(!contains(sMap, 3), "Map3")
    }
  }


  test("forall returns false if set contains elements that don't satisfy function") {
    new TestSets {
      val sUnion = union(s12, s4)
      val sUnion2 = union (sUnion, s3)
      val sForall = forall(sUnion2, ((p: Int) => (p%2==0)))
      assert(!sForall)
    }
  }

  test("forall returns true if set contains only elements that satisfy function") {
    new TestSets {
      val sUnion = union(s2, s4)
      val sUnion2 = union (sUnion, s6)
      val sForall = forall(sUnion2, ((p: Int) => (p%2==0)))
      assert(sForall)
    }
  }


  test("exists returns true if set contains one element that satisfies function") {
    new TestSets {
      val sUnion = union(s3, s1)
      val sUnion2 = union (sUnion, s4)
      val sExists = exists(sUnion2, ((p: Int) => (p%2==0)))
      assert(sExists)
    }
  }


  test("exists returns false if set doesn't contain any element that satisfies function") {
    new TestSets {
      val sUnion = union(s3, s1)
      val sExists = exists(sUnion, ((p: Int) => (p%2==0)))
      assert(!sExists)
    }
  }

  test("map returns new set with applied function") {
    new TestSets {
      val sUnion = union(s3, s1)
      val sMap = map(sUnion, (p: Int) => (p*2))
      assert(contains(sMap, 6))
      assert(contains(sMap, 2))
    }
  }


}
