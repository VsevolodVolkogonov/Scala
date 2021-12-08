/** Напишите отдельные функции, решающие поставленную задачу. 
 *
 * Синтаксис:
 *   // метод
 *   def myFunction(param0: Int, param1: String): Double = // тело
 *
 *   // значение
 *   val myFunction: (Int, String) => Double (param0, param1) => // тело
 */
import scala.math.Pi

/** Напишите отдельные функции, решающие поставленную задачу.
 *
 * Синтаксис:
 *   // метод
 *   def myFunction(param0: Int, param1: String): Double = // тело
 *
 *   // значение
 *   val myFunction: (Int, String) => Double (param0, param1) => // тело
 */
object Functions {

  def acreage(r: Double): Double = r*r*Pi
  /* a) Напишите функцию, которая рассчитывает площадь окружности
   *    r^2 * Math.PI
   */
  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testCircle(r: Double): Double = acreage(r)

  /* b) Напишите карированную функцию которая рассчитывает площадь прямоугольника a * b.
   */
  def rectangle(a: Double)(b: Double): Double = a * b

  // примените вашу функцию из пукта (b) здесь, не изменяя сигнатуру
  def testRectangleCurried(a: Double, b: Double): Double = rectangle(a)(b)

  // c) Напишите не карированную функцию для расчета площади прямоугольника.
  def rectangleNotCur(a: Double, b: Double): Double = a * b

  // примените вашу функцию из пункта (c) здесь, не изменяя сигнатуру
  def testRectangleUc(a: Double, b: Double): Double = rectangleNotCur(a, b)

}

object Functions_test extends App{

  println(Functions.acreage(4))

  println(Functions.testCircle(4))

  println(Functions.rectangle(4)(2))

  val carrFun = Functions.testRectangleCurried(4, _)
  println(carrFun(5))

  println(Functions.rectangleNotCur(3,4))

  println(Functions.testRectangleUc(4,5))


}
