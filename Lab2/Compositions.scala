import Compositions.{Ex1, Ex2, Ex3, Ex4, Ex5, Ex6, testCompose, testForComprehension, testMapFlatMap}
//в данной программе выполняется одно и то же тремя разными способами.
//Есть три функции и они последовательно выполняются, чтобы получить общий результат
//сначала это делается при помощи compose - можно просто сказать, что это функция, которая обьединяет функции
//потом выполняется с помощью map и flatmap - это функции, которые к элементам коллекции применяют функцию, тогда коллекция модифицируется.
//и еще выполняют с помощью цикла, который выполняется один раз и в нем последовательно выполняются функции, рещультат возвращается с помощью yield

sealed trait Option[A] {

  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
}
case class Some[A](a: A) extends Option[A] {

  def map[B](f: A => B): Option[B] = Some(f(a))
  def flatMap[B](f: A => Option[B]): Option[B] = f(a)
}
case class None[A]()     extends Option[A] {

  def map[B](f: A => B): Option[B] = None()
  def flatMap[B](f: A => Option[B]): Option[B] = None()
}

/** Напишите ваши решения в тестовых функциях.  */
object Compositions {

  // a) Используйте данные функции. Вы можете реализовать свое решение прямо в тестовой функции.
  // Нельзя менять сигнатуры

  def testCompose[A, B, C, D](f: A => B)
                             (g: B => C)
                             (h: C => D): A => D =
    h.compose(g).compose(f)

  /**
   * Происходит объединение функций
   */


  // b) Напишите функции с использованием `map` и `flatMap`. Вы можете реализовать свое решение прямо в тестовой функции.
  // Нельзя менять сигнатуры

  def testMapFlatMap[A, B, C, D](f: A => Option[B])
                                (g: B => Option[C])
                                (h: C => D): Option[A] => Option[D] = _ flatMap f flatMap g map h


  // c) Напишите функцию используя for. Вы можете реализовать свое решение прямо в тестовой функции.
  // Нельзя менять сигнатуры

  def testForComprehension[A, B, C, D](f: A => Option[B])
                                      (g: B => Option[C])
                                      (h: C => D): Option[A] => Option[D] = {
    for {
      a <- _
      b <- f(a)
      c <- g(b)
    } yield h(c)
  }

  def Ex1(x: Char): Int = x+3 // значение 97 + 3
  def Ex2(x: Int): Double = x*2.5
  def Ex3(x: Double): Int = (x/3).toInt
  def Ex4(x: Char): Option[Int] = Some(x+3)
  def Ex5(x: Int): Option[Double] = Some(x*2.5)
  def Ex6(x: Double): Int = (x / 3).toInt


}


object Test extends App {
  println(testCompose(Ex1)(Ex2)(Ex3)('a')) // ascii of [space] is 32
  println(testMapFlatMap(Ex4)(Ex5)(Ex6)(Some('a')))
  println(testForComprehension(Ex4)(Ex5)(Ex6)(Some('a')))
}