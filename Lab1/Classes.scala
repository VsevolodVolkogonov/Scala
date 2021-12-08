// a) c)
class AnimalC(val name: String, val species: String, food: String) {
  def eats(food: String): Boolean = ((food == "meat" && name == "cat") || (food == "vegetables" && name == "parrot") || (food == "plants" && name == "goldfish"))
}

/* b) Создайте объект-компаньон для класса Animal и добавьте следующие сущности как поля:
*      - cat, mammal, meat
*      - parrot, bird, vegetables
*      - goldfish, fish, plants
*
*    Синтаксис: object MyClass {
*              // статические поля и методы
*            }
*/

object AnimalsC {
  val cat = new AnimalC("cat", "mammal", "Meat")
  val parrot = new AnimalC("parrot", "bird", "Vegetables")
  val goldfish = new AnimalC("goldfish", "fish", "Plants")
}

/*
* d) Переопределите ваш класс Animal как трейт и создайте объекты класса-образца для Mammals, Birds и Fishes.
*    Вам все еще нужно поле `species`?
*/

trait Animal {
  val name: String
  val food: Food

}

case class Mammals(name: String, food: Food) extends Animal
case class Birds(name: String, food: Food) extends Animal
case class Fishes(name: String, food: Food) extends Animal


/* e) Добавьте следующие функции в объект-компаньон Animal:
*      def knownAnimal(name: String): Boolean  // true если это имя одного из трех животных из (b)
*      def apply(name: String): Option[Animal] // возвращает одно из трех животных в соответствии с именем (Some) или ничего (None), см. ниже
*/

object Animal {
  def knownAnimal(name: String): Boolean = name == "cat" | name == "parrot" | name == "goldfish"

  def apply(name: String): Option[Animal] = name match {
    case "cat" => Some(Mammals("cat", Meat))
    case "parrot" => Some(Birds("parrot", Vegetables))
    case "goldfish" => Some(Fishes("goldfish", Plants))
    case _ => None
  }
}

/* f) Создайте трейт Food со следующими классами-образцами:
*      - Meat
*      - Vegetables
*      - Plants
*   и добавьте это в определение Animal. Так же добавьте объект-компаньон с методом apply():
*      def apply(food: String): Option[Food]
*/

trait Food

case object Meat extends Food
case object Vegetables extends Food
case object Plants extends Food

object Food {
  def apply(food: String): Option[Food] = {
    food match {
      case "meat" => Some(Meat)
      case "vegetables" => Some(Vegetables)
      case "plants" => Some(Plants)
      case _ => None
    }
  }
}

object main extends App {
  println("Кошки едят мясо: ", AnimalsC.cat.eats("Meat"))
  println("Попуги едят мясщ: ", AnimalsC.parrot.eats("Meat"))
  println("Cat входит в список описываемых: ", Animal.knownAnimal("cat"))
  println("Volodya входит в список описываемых: ", Animal.knownAnimal("Volodya"))
  println(Animal.apply("cat"))
  println(Food.apply("vegetables"))
}