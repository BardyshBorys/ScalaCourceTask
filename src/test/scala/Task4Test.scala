import Task3.Validator._
import Task3._
import org.scalatest.FunSuite

class Task4Test extends FunSuite {

  test("Task3.validator") {
    assert((2 validate (lessThan(10) and positiveInt)).isRight)
    assert((-1 validate (positiveInt and lessThan(10))).isLeft)
    assert((-1 validate (lessThan(10) or positiveInt)).isRight)

    assert(("" validate nonEmpty).isRight)
    assert(("Foo" validate nonEmpty).isLeft)
    assert((Person(name = "John", age = 25) validate isPersonValid).isRight)
    assert((Person(name = "John", age = 125) validate isPersonValid).isLeft)

    assert(234.validate.isRight)
    assert(("asdasd" validate).isRight)
    assert((Person(name = "John", age = 25) validate).isRight)

    assert((2 validate (lessThan(10) and positiveInt and lessThan(3))).isRight)
    assert((-1 validate ((positiveInt and lessThan(10)) or lessThan(1))).isRight)
    assert((-1 validate (lessThan(10) or positiveInt)).isRight)

  }
}
