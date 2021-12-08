import RecursiveData.{Leaf, Node, testListIntEmpty, testListIntHead}

sealed trait List[A]
case class Cons[A](head: A, tail: List[A]) extends List[A]
case class Nil[A]() extends List[A]

/** Напишите свои решения в виде функций. */
object RecursiveData {

  // a) Реализуйте функцию, определяющую является ли пустым `List[Int]`.
  //сравнение с пустым листом nill
  def listIntEmpty(list: List[Int]): Boolean = list != Nil()

  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testListIntEmpty(list: List[Int]): Boolean = listIntEmpty(list)

  // b) Реализуйте функцию, которая получает head `List[Int]`или возвращает -1 в случае если он пустой.
  //сравнение с непустым листом Cons
  def listIntHead(list: List[Int]): Int = list match {
    case list: Cons[Int] => list.head
    case _ => -1
  }

  // используйте функцию из пункта (б) здесь, не изменяйте сигнатуру
  def testListIntHead(list: List[Int]): Int = listIntHead(list)

  // c) Можно ли изменить `List[A]` так чтобы гарантировать что он не является пустым?
  //пустой лист nill заменяем на лист, в котором будет значение
  sealed trait List2[A]

  case class Cons2[A](head: A, tail: List2[A]) extends List2[A]

  case class End[A](head: A) extends List2[A]

  /* d) Реализуйте универсальное дерево (Tree) которое хранит значения в виде листьев и состоит из:
   *      node - левое и правое дерево (Tree)
   *      leaf - переменная типа A
   */
  sealed trait Tree[A]

  case class Node[A](leftBranch: Tree[A], rightBranch: Tree[A]) extends Tree[A]

  case class Leaf[A](head: A) extends Tree[A]


}

object RecDataTest extends App {
  var list1: List[Int] = Cons(34, Cons(12, Nil())) //Пример с полным списком
  var list2: List[Int] = Nil() // Пример с пустым
  println("1 - Задание ")
  println(testListIntEmpty(list1))
  println(testListIntEmpty(list2))
  println("2 - Задание ")
  println(testListIntHead(list1))
  println(testListIntHead(list2))

  println("3 - Задание ")
  println(
    Node(
      Node(Leaf(1),
        Leaf(2)),
      Node(Leaf(3),
        Node(Leaf(44),
          Leaf(55)))))

}
