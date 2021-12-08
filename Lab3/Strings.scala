import Strings.{testComputation, testInterpolations, testTakeTwo, testUppercase}

object Strings {

  /* a) Преобразуйте все символы типа Char в верхний регистр (не используйте заглавные буквы). */
  def testUppercase(str: String): String = str.toUpperCase()

  /* b) Вставьте следующие значения в строку:Hi my name is <name> and I am <age> years old.*/
  def testInterpolations(name: String, age: Int): String = s"Hi my name is $name and I am $age years old"

  /* c) Добавьте два числа в следующую строку:
   *       Hi,
   *       now follows a quite hard calculation. We try to add:
   *         a := <value of a>
   *         b := <value of b>
   *         result is <a + b>*/
  def testComputation(a: Int, b: Int): String =
    s"Hi," +
      s"\nnow follows a quite hard calculation. We try to add:" +
      s"\na := $a\nb := $b\nresult is ${a + b}"

  /* d) Если длина строки равна 2, верните всю строку, иначе верните первые два символа строки.*/
  def testTakeTwo(str: String): String = {
    if(str.length==2) str else str.substring(0,2)
  }
}

object StringsTest extends App{
  {
    print("1 - Задание")
    println(testUppercase("Task 1 completed"))
    print("2 - Задание")
    println(testInterpolations("Vsevolod",20))
    println("3 - Задание ")
    println(testComputation(36,14))
    print("4 - Задание")
    println(testTakeTwo("4+ Задание выполнено"))
  }
}