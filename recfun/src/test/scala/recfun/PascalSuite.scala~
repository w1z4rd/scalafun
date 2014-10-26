package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
  test("pascal: col=0,row=2") {
    assert(pascal(0, 2) === 1)
  }

  test("pascal: col=1,row=2") {
    assert(pascal(1, 2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1, 3) === 3)
  }

  test("pascal: col=3,row=5") {
    assert(pascal(3, 5) === 10)
  }

  test("pascal: column bigger than row") {
    intercept[NoSuchElementException] {
      pascal(3, 1)
    }
  }

  test("pascal: column negative") {
    intercept[NoSuchElementException] {
      pascal(-1, 1)
    }
  }

  test("pascal: row negative") {
    intercept[NoSuchElementException] {
      pascal(1, -1)
    }
  }

}

