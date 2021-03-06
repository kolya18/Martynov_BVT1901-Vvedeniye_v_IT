
import scala.util.{Try, Failure, Success}

/** Реализуйте следующие функции.
  * 
  * List(1, 2) match {
  *   case head :: tail => ???
  *   case Nil          => ???
  *   case l            => ???
  * }
  * 
  * Option(1) match {
  *   case Some(a) => ???
  *   case None    => ???
  * }
  * 
  * Either.cond(true, 1, "right") match {
  *   case Left(i)  => ???
  *   case Right(s) => ???
  * }
  * 
  * Try(impureExpression()) match {
  *   case Success(a)     => ???
  *   case Failure(error) => ???
  * }
  * 
  * Try(impureExpression()).toEither
  * 
  */
object Adts {

  // a) Дан List[Int], верните элемент с индексом n


  // примените функцию из пункта (a) здесь, не изменяйте сигнатуру 
  def testGetNth(list: List[Int], n: Int): Option[Int] = {
    list match {
      case head :: tail => Some(list(n))
      case Nil => null
      case l => Option(l(n))
    }
  }

  testGetNth(List(1, 7, 5, 7), 2)

  // b) Напишите функцию, увеличивающую число в два раза.

  def two(number: Option[Int]): Option[Int] = number match {
    case Some(n) => Option(n * 2)
    case None => null
  }

  two(Option(18))

  // примените функцию из пункта (b) здесь, не изменяйте сигнатуру
  def testDouble(n: Option[Int]): Option[Int] = two(n)

  // c) Напишите функцию, проверяющую является ли число типа Int четным. Если так, верните Right. В противном случае, верните Left("Нечетное число.").

  def IsEven(n: Int): Either[String, Int] = Either.cond(n % 2 == 0, n, "Нечетное число.") match {
    case Left(i) => Left(i)
    case Right(s) => Right(s)
  }

  IsEven(18)
  IsEven(19)

  // примените функцию из пункта (c) здесь, не изменяйте сигнатуру
  def testIsEven(n: Int): Either[String, Int] = IsEven(n)

  // d) Напишите функцию, реализующую безопасное деление целых чисел. Верните Right с результатом или Left("Вы не можете делить на ноль.").

  import scala.util.{Failure, Success, Try}

  def SafeDivide(a: Int, b: Int): Either[String, Int] = Try(a / b) match {
    case Success(a) => Right(a)
    case Failure(error) => Left("Вы не можете делить на ноль.")
  }

  SafeDivide(18, 9)
  SafeDivide(18, 0)

  // примените функцию из пункта (d) здесь, не изменяйте сигнатуру
  def testSafeDivide(a: Int, b: Int): Either[String, Int] = SafeDivide(a, b)

  // e) Обработайте исключения функции с побочным эффектом вернув 0.

  def GoodOldJava(impure: String => Int, str: String): Try[Int] =  Try(impure(str)).toEither match {
      case Right(k) => Success(k)
      case Left(l) => Success(0)
    }

  val s = "Martynov"
  val f = (s:String) => s.size
  GoodOldJava(f,s)

  // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
  def testGoodOldJava(impure: String => Int, str: String): Try[Int] = GoodOldJava(impure,str)

}
