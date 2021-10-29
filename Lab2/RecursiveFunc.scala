

import scala.annotation.tailrec

/** Реализуйте функции для решения следующих задач.
  * Примечание: Попытайтесь сделать все функции с хвостовой рекурсией, используйте аннотацию для подстверждения.
  * рекурсия будет хвостовой если:
  *   1. рекурсия реализуется в одном направлении
  *   2. вызов рекурсивной функции будет последней операцией перед возвратом
  */
object RecursiveFunctions {

  def length[A](as: List[A]): Int = {
    @tailrec
    def loop(rem: List[A], agg: Int): Int = rem match {
      case Cons(_, tail) => loop(tail, agg + 1)
      case Nil()         => agg
    }

    loop(as, 0)
  }

  /* a) Напишите функцию которая записывает в обратном порядке список:
   *        def reverse[A](list: List[A]): List[A]
   */

 // val n: List[Int] = List(1, 2, 3, 4, 6, 7, 8)
//  val m = List('M', 'a', 'r', 't', 'y', 'n', 'o', 'v')

  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def revT(revlist: List[A], remlist: List[A]): List[A] = remlist match {
      case Nil => revlist
      case head :: tail => revT(head :: revlist, tail)
    }
    revT(Nil, list)
  }



  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
 // def testReverse[A](list: List[A]): List[A] = list
 def testReverse[A](list: List[A]): List[A] = reverse(list)

  /* b) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def map[A, B](list: List[A])(f: A => B): List[B]
   */

//  object Combinators {
//    def map[A, B](f: (A) => B)(list: List[A]): List[B] = list map f
//  }
//val intToSring = (i: Int) => s"N=$i"
//val flist = Combinators.map(intToSring) _
//val list = flist(List(1, 2, 3, 4, 5))


  sealed trait List[A]
  case class Cons[A](head: A, tail: List[A]) extends List[A]
  case class Nil[A]() extends List[A]

  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def loop(rem: List[A], num: List[A]): List[A]
    = rem match {
      case Nil() => num
      case Cons(x, y) => loop(y, Cons(x, num))
    }
    loop(list, Nil())
  }

  def map[A, B](list: List[A])(f: A => B): List[B] = {
    @tailrec
    def loop2(l: List[A], num: List[B]): List[B] = l match {
      case Nil() => reverse(num)
      case Cons(x,y) => loop2(y,Cons(f(x),num))
    }
    loop2(list,Nil())
  }

  val l2 = Cons[Int](5, Cons[Int](10, Nil[Int]()))
  val f2 = (a: Int) => a * 5
  val Map2 = map(l2)(f2)



  // используйте функцию из пункта  (b) здесь, не изменяйте сигнатуру
  def testMap[A, B](list: List[A], f: A => B): List[B] = map(list)(f)
  
  /* c) Напишите функцию, которая присоединяет один список к другому:
   *        def append[A](l: List[A], r: List[A]): List[A]
   */

   //val l1: List[Int] = List(6, 7, 8)
   //val l2: List[Int] = List(14, 17, 20)
//  @tailrec
//  def append[A](l: List[A], r: List[A]): List[A] = l match {
//    case Nil => r
//    case h :: t => append(t, h :: r)
//  }

  def append[A](l: List[A], r: List[A]): List[A] = l match {
      case Nil() => r
      case Cons(h,t) => Cons(h, append(t, r))
    }
  val l31 = Cons[Int](1, Cons[Int](2, Nil[Int]()))
  val l32 = Cons[Int](7, Cons[Int](8, Nil[Int]()))
  val app = append(l31,l32)

  // используйте функцию из пункта  (c) здесь, не изменяйте сигнатуру
  def testAppend[A](l: List[A], r: List[A]): List[A] = append(l, r)

  /* d) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def flatMap[A, B](list: List[A])(f: A => List[B]): List[B]
   * 
   *    она получает функцию, которая создает новый List[B] для каждого элемента типа A в 
   *    списке. Поэтому вы создаете List[List[B]]. 
   */

//  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
//    @tailrec
//    def fMap(as: List[A], acc: List[B])(f: A => List[B]):List[B] = as match {
//      case Nil => acc
//      case a :: aas => fMap(aas, acc ::: f(a)) (f)
//    }
//    fMap(list, Nil)(f)
//  }

//  flatMap(l1)(i => List(i, 2*i, 3*i))
//  flatMap(l2)(i => List(i, 2*i, 3*i))
//flatMap(List(18, 25, 40))(i => List(i, 2*i, 3*i))


  def flatMap[A,B](list: List[A])(f: A=>List[B]): List[B] = {
    @tailrec
    def loop2(l: List[A],num: List[B]): List[B] = l match {
      case Nil() => reverse(num)
      case Cons(x,y) => loop2(y, append(f(x),num))
    }
    loop2(list, Nil())
  }

  val l4 = Cons[Cons[Int]](Cons[Int](1 , Nil[Int]()), Cons[Cons[Int]](Cons[Int](2 , Nil[Int]()), Nil[Cons[Int]]()))
  val func: List[Int] => List[Int] = _ match {
    case Cons(head, tail) => Cons(head,tail)
    case Nil() => Nil()
  }
  val fMap = flatMap(l4)(func)


  // используйте функцию из пункта  (d) здесь, не изменяйте сигнатуру
  //def testFlatMap[A, B](list: List[A], f: A => List[B]): List[B] = flatMap((list),(f))

  /* e) Вопрос: Возможно ли написать функцию с хвостовой рекурсией для `Tree`s? Если нет, почему? */

  sealed class Tree
  case object Leaf extends Tree
  case class Node(val elem: Int, val left: Tree, val right: Tree) extends Tree

  def sumTailRec(bt: Tree) = {
    @tailrec
    def sumAccStack(trees: List[Tree], acc: Int): Int = trees match {
      case Nil => acc
      case Leaf :: rs => sumAccStack(rs, acc)
      case Node(e, l, r) :: rs => sumAccStack(l :: r :: rs, e + acc)
    }

    sumAccStack(List(bt), 0)
  }

  val tree = Node(8, Node(5, Node(5, Leaf, Leaf), Leaf), Leaf)
  sumTailRec(tree)

}
