

sealed trait List[A]
case class Cons[A](head: A, tail: List[A]) extends List[A]
case class Nil[A]() extends List[A]

/** Напишите свои решения в виде функций. */
object RecursiveData {

  // a) Реализуйте функцию, определяющую является ли пустым `List[Int]`.

  //val nums: List[Int] = List(1, 2, 3, 4)
  //val nn: List[Int] = List()
  //val nnn = Nil
 // def ListIntEmpty(list: List[Int]): Boolean = list.isEmpty

  def ListIntEmpty(list: List[Int]): Boolean = list match {
    case Nil() => true
    case _ => false
  }

  val l = Nil[Int]()
  val l2 = Cons[Int](2, l)

  val ListIntEmpty1 = ListIntEmpty(l)
  val ListIntEmpty2 = ListIntEmpty(l2)

  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testListIntEmpty(list: List[Int]): Boolean = ListIntEmpty(list)
 // def testListIntEmpty(list: List[Int]): Boolean = ListIntEmpty(list)

  // b) Реализуйте функцию, которая получает head `List[Int]`или возвращает -1 в случае если он пустой.

  //val nums2: List[Int] = List(7, 18, 500)

//  def ListIntHead(list: List[Int]): Int = list.isEmpty match {
//    case false => list.head
//    case true => -1
//  }

  def ListIntHead(list: List[Int]): Int = list match {
    case Cons(head, tail) => head
    case Nil() => -1
  }
  val ListIntHead1 = ListIntHead(l)
  val ListIntHead2 = ListIntHead(l2)

  // используйте функцию из пункта (b) здесь, не изменяйте сигнатуру
  def testListIntHead(list: List[Int]): Int = 0
 // def testListIntHead(list: List[Int]): Int = ListIntHead(list)

  // c) Можно ли изменить `List[A]` так чтобы гарантировать что он не является пустым?

 // Добавить head в класс Nil
//  sealed trait List[A]
//  case class Cons[A](head: A, tail: List[A]) extends List[A]
//  case class Nil[A](head: A) extends List[A]



  /* d) Реализуйте универсальное дерево (Tree) которое хранит значения в виде листьев и состоит из:
   *      node - левое и правое дерево (Tree)
   *      leaf - переменная типа A
   */

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  val tree: Tree[Int] = Node( Node (Leaf(18), Node(Leaf(5), Leaf(6))), Leaf(7))

}
