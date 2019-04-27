package object Task3 {

  case class Person(name: String, age: Int)

  def dir(x: AnyRef): Unit = println(x.getClass.getMethods.map(_.getName).distinct.sorted.mkString(" "))

  trait Validator[T] {
    /**
      * Validates the value.
      *
      * @param value value to be validated.
      * @return Right(value) in case the value is valid, Left(message) on invalid value
      */
    def validate(value: T): Either[String, T]

    /**
      * And combinator.
      *
      * @param other validator to be combined with 'and' with this validator.
      * @return the Right(value) only in case this validator and <code>other</code> validator returns valid value,
      *         otherwise Left with error messages from the validator that failed.
      */
    def and(other: Validator[T]): Validator[T] = new Validator[T] {
      override def validate(value: T): Either[String, T] = {
        Validator.this.validate(value) match {
          case Right(v) => other.validate(v)
          case Left(error) => Left(error)
        }
      }
    }
    /**
      * Or combinator.
      *
      * @param other validator to be combined with 'or' with this validator.
      * @return the Right(value) only in case either this validator or <code>other</code> validator returns valid value,
      *         otherwise Left with error messages from both validators.
      */
    def or(other: Validator[T]): Validator[T] = new Validator[T] {
      override def validate(value: T): Either[String, T] = {
        Validator.this.validate(value) match {
          case Right(v) => Right(v)
          case Left(_) => other.validate(value)
        }
      }
    }
  }

  object Validator {

    implicit class Validated[T](t: T) {
      def validate(validator: Validator[T] = new Validator[T] {
        override def validate(value: T): Either[String, T] = Right(value)
      }): Either[String, T] = {
        validator.validate(t)
      }
      def validate: Either[String, T] = Right(t)
    }

    val positiveInt: Validator[Int] = new Validator[Int] {
      type value[T] = Either[String, T]

      override def validate(t: Int): Either[String, Int] = {
        if (t > 0) Right(t) else Left("Int is negative")
      }
    }

    def lessThan(n: Int): Validator[Int] = new Validator[Int] {
      type value[T] = Either[String, T]

      override def validate(t: Int): Either[String, Int] = {
        if (t < n) Right(t) else Left("Int is negative")
      }
    }

    val nonEmpty: Validator[String] = new Validator[String] {
      type value[T] = Either[String, T]

      override def validate(t: String): Either[String, String] = {
        val message = f"""Value "$t%s" - empty"""
        if (t.length > 0) Left(message) else Right(t)
      }
    }

    val isPersonValid: Validator[Person] = new Validator[Person] {
      type value[T] = Either[String, T]

      override def validate(value: Person): Either[String, Person] = {
        val message = f"""Person "$value%s" - not valid"""
        if (value.name.length > 0 & value.age <= 99 & value.age >= 1)
          Right(value)
        else
          Left(message)
      }
    }
  }

}
