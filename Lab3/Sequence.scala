
import scala.annotation.tailrec

/** Напишите свои решения в тестовых функциях.
  * 
  * Seq(1, 2) match {
  *   case head +: tail => ???
  *   case Nil          => ???
  *   case s            => ???
  * }
  * 
  * https://www.scala-lang.org/api/2.12.0/scala/collection/Seq.html
  */
// Примечание: напишите функции с хвостовой рекурсией

object Sequence {

  /* a) Найдите последний элемент Seq.
   *    
   */
  def testLastElement[A](seq: Seq[A]): Option[A] = seq match {
    case Seq(x) => Option[A](x)
    case head +: tail => testLastElement(tail)
  }

  val a = Seq[Int](1, 11, 111)
  testLastElement(a)

  /* b) Объедините две Seqs (то есть Seq(1, 2) и Seq(3, 4) образуют Seq((1, 3), (2, 4))) - если Seq длиннее игнорируйте оставшиеся элементы.
   *    
   */
  def testZip[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = {
    @tailrec
    def loop[A](a: Seq[A], b: Seq[A], c: Seq[(A, A)]): Seq[(A, A)] = a match {
      case ahead +: atail => b match {
        case blast +: Nil => c :+ (ahead, blast)
        case bhead +: btail =>loop(atail, btail, c :+ (ahead,bhead))
      }
      case Nil => c
    }
    loop(a, b, Nil)
  }

  testZip(Seq[Int](1, 2), Seq[Int](3, 4))

  /* c) Проверьте, выполняется ли условие для всех элементов в Seq.
   *    
   */
  def testForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = {
    @tailrec
    def loop[A](a: Seq[A])(cond: A => Boolean): Boolean = a match {
      case head +: tail => if (cond(head)) loop(tail)(cond) else false
      case Nil => true
    }
    loop(seq)(cond)
  }

  val cond: Int => Boolean = _ % 2 == 0
  testForAll(Seq(1, 2))(cond)


  /* d) Проверьте, является ли Seq палиндромом
   *    
   */
  def testPalindrom[A](seq: Seq[A]): Boolean = {
    @tailrec
    def loop[A](a: Seq[A], b: Seq[A]): Boolean = {
      a match {
        case head +: tail => loop(tail, b = head +: b)
        case Nil => seq.equals(b)
      }
    }
    loop(seq, Nil)
  }

  testPalindrom(Seq(8, 1, 1, 8))
  testPalindrom(Seq(1, 1, 3, 7))

  /* e) Реализуйте flatMap используя foldLeft.
   *    
   */
  def testFlatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = seq.foldLeft[Seq[B]](Seq())((acc, elem) => acc ++: f(elem) )
  val e = Seq(8,(7, 4, 5), 6,9)
  val f = (value: Any) => Seq(Seq(value))
  testFlatMap(e)(f)

  val e2 = Seq((1,1),8,(3, 3, 3),7,7)
  testFlatMap(e2)(f)
}
