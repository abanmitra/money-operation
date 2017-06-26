package com.money

trait RawMoney[T] {
  def add(paramOne: T, paramTwo: T): T
}

trait Money[T] extends RawMoney[T]

object Money {

  // Create object of that type
  def apply[T](implicit m: Money[T]): Money[T] = m

  object options {
    implicit class MoneyOptions[T: Money](paramOne: T) {
      def add(paramTwo: T) = Money[T].add(paramOne, paramTwo)
    }

  }

  def typeOf[T](addFunction: (T, T) => T): Money[T] = new Money[T] {
    def add(paramOne: T, paramTwo: T) = addFunction(paramOne, paramTwo)
  }

  implicit def addIntValue: Money[Int] = typeOf[Int]((paramOne: Int, paramTwo: Int) => { paramOne + paramTwo })
  implicit def addDoubleValue: Money[Double] = typeOf[Double]((paramOne: Double, paramTwo: Double) => { paramOne + paramTwo })
  implicit def addBigDecimalValue: Money[BigDecimal] = typeOf[BigDecimal]((paramOne: BigDecimal, paramTwo: BigDecimal) => { paramOne + paramTwo })
}

object MoneyTesting {

  def main(args: Array[String]): Unit = {

    import Money.options._

    val intParamOne: Int = 20
    val intParamTwo: Int = 30
    print(s"Integer :: $intParamOne add $intParamTwo = "); println(intParamOne add intParamTwo)

    val doubleParamOne: Double = 2023.2323
    val doubleParamTwo: Double = 3420.3534
    print(s"Double :: $doubleParamOne add $doubleParamTwo = "); println(doubleParamOne add doubleParamTwo)

    val bigDecimalParamOne: BigDecimal = 73643.2873
    val bigDecimalParamTwo: BigDecimal = 74686.39859
    print(s"BigDEcimal :: $bigDecimalParamOne add $bigDecimalParamTwo = "); println(bigDecimalParamOne add bigDecimalParamTwo)

  }
}
