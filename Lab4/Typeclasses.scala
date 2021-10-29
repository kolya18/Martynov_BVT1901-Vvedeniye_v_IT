
object Typeclasses {

  // a) Определите тайп-класс Reversable, который представляет в обратном порядке значения.
  // b) Реализуйте функцию Reverse для String.

  trait Reversable[T] { def reverse(a: T): T }

  object Reversable {
    def reverse[T: Reversable](a: T): T = implicitly[Reversable[T]].reverse(a)
    implicit object ReversableString extends Reversable[String] {
      def reverse(str: String): String = str.reverse
    }
  }

  // примените тайп-класс-решение из пункта (a) здесь
  def testReversableString(str: String): String = Reversable.reverse(str)

  val s = "Martynov"
  testReversableString(s)

  // c) Определите тайп-класс Smash таким образом чтобы в нем была функция smash, которая выполняет операцию со значениями одного типа.
  // d) Реализуйте  функции Smash для типа Int и Double.
  //    Используйте сложение для типа Int у умножение для типа Double.
  // e) Реализуйте функцию Smash для типа String. Необходимо выполнить конкатенацию строк, которые будут получены в качестве параметра.

  trait Smash[T] { def smash(a: T, b: T): T }

  object Smash {
    def smash[T : Smash](a: T, b: T): T = implicitly[Smash[T]].smash(a, b)
    implicit object SmashInt extends Smash[Int] {
      def smash(a: Int, b: Int): Int = a + b
    }
    implicit object SmashDouble extends Smash[Double] {
      def smash(a: Double, b: Double): Double = a * b
    }
    implicit object SmashString extends Smash[String] {
      def smash(a: String, b: String): String = a + b
    }
  }

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashInt(a: Int, b: Int): Int = Smash.smash(a,b)

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashDouble(a: Double, b: Double): Double = Smash.smash(a,b)

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashString(a: String, b: String): String = Smash.smash(a,b)

  testSmashInt(18,22)
  testSmashDouble(5.5,7.125)
  testSmashString("Kolya","Martynov")

}

// Реализуйте тестовые функции с выводом на экран проверки разработанных функций.

