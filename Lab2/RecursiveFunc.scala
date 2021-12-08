import RecursiveFunctions.{flatMap, testAppend, testMap, testReverse}

import scala.annotation.tailrec

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
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def loop(origin: List[A], reverse: List[A]): List[A] = origin match {
      case Cons(n, t) => loop(t, Cons(n, reverse))
      case Nil() => reverse
    }
    loop(list, new Nil[A])
  }
  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testReverse[A](list: List[A]): List[A] = reverse(list)


  /* b) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def map[A, B](list: List[A])(f: A => B): List[B]
   */
  def map[A, B](list: List[A])(f: A => B): List[B] = {
    @tailrec
    def loop(origin: List[A], changed: List[B]): List[B] = origin match {
      case Cons(n, t) => loop(t, Cons(f(n), changed))
      case Nil() => reverse(changed)
    }
    loop(list, Nil())
  }
  // используйте функцию из пункта  (b) здесь, не изменяйте сигнатуру
  def testMap[A, B](list: List[A], f: A => B): List[B] = map(list)(f)


  /* c) Напишите функцию, которая присоединяет один список к другому:
   *        def append[A](l: List[A], r: List[A]): List[A]
   */
  def append[A](l: List[A], r: List[A]): List[A] = {
    @tailrec
    def loop(left: List[A], right: List[A] ): List[A] = left match {
      case Cons(n, t) => loop(t, Cons(n, right))
      case Nil() => right
    }
    loop(reverse(l), r)
  }

  // используйте функцию из пункта  (c) здесь, не изменяйте сигнатуру
  def testAppend[A](l: List[A], r: List[A]): List[A] = append(l,r)

  /* d) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def flatMap[A, B](list: List[A])(f: A => List[B]): List[B]
   *
   *    она получает функцию, которая создает новый List[B] для каждого элемента типа A в
   *    списке. Поэтому вы создаете List[List[B]].
   */
  def flatMap[A, B](list: List[A])(f: A => B): List[B] = {
    @tailrec
    def loop(origin: List[A], result: List[B]): List[B] = origin match {
      case Cons(n, t) => loop(t, Cons(f(n), result))
      case Nil() => reverse(result)
    }
    loop(list,new Nil[B])
  }
  // используйте функцию из пункта (d) здесь, не изменяйте сигнатуру
  def testFlatMap[A, B](list: List[A], f: A => List[B]): List[B] = Nil()

  /* e) Вопрос: Возможно ли написать функцию с хвостовой рекурсией для `Tree`s? Если нет, почему? */
  /** Если необходимо найти какое-то значение, тогда можно написать функцию с хвостовой рекурсие.
   * Если необходимо проверить оба значения в Node, тогда реализация может стать невыполнимой
   * потому что понадобится запустить 2 функции (для каждого Node)
   * значений
   */


}

object RecursiveFunctionsTest extends App{
  println("1 - Задание")
  println(testReverse(Cons(321, Cons(654, Cons(987, Nil())))))
  println("2 - Задание")
  println(testMap(Cons(115, Cons(300, Cons(250, Nil()))), (x: Int) => x *2))
  println("3 - Задание")
  println(testAppend(Cons[Int](32, Cons(12, Cons(4, Nil()))), Cons[Int](654, Cons(35, Cons(33, Nil())))))
  println("4 - Задание")
  println(flatMap(Cons[Int](23, Cons(12, Cons(54, Nil()))))((x: Int) => Cons(x, Nil())))
}