import Maps.{User, testGroupUsers, testNumberFrodos, testUnderaged}

object Maps {

  case class User(name: String, age: Int)

  /* a) В данной Seq[User] сгруппируйте пользователей по имени (`groupBy`) и вычислите средний возраст: `name -> averageAge`*/
  def testGroupUsers(users: Seq[User]): Map[String, Int] = {
    def srage(num:Seq[Int])=num.sum/num.length

    users.groupBy(_.name).map{
      myKey=>(myKey._1, srage(myKey._2.map(_.age)))
    }
  }


  /* b) Дана `Map[String, User]` состоящая из имен пользователей `User`, сколько имен пользователей, содержащихся в Map, содержат подстроку "Adam"?*/
  def testNumberFrodos(map: Map[String, User]): Int = {
    map.count(x=>x._2.name=="Adam")
  }


  /* c) Удалите всех пользователей возраст которых менее 35 лет.*/
  def testUnderaged(map: Map[String, User]): Map[String, User] = {
      map.filter(x=>x._2.age>35)
  }

}

object TestMain extends App {
  print("1 - Задание: ")
  println(testGroupUsers(Seq(User("Seva",14),User("Vanya",13),User("Senya",4),User("Vanya",4),User("Seva",18))))
  print("2 - Задание: ")
  var map=Map("firstUser"->User("Adam",12),"secondUser"->User("Adam",15),"NONUser"->User("Vsevolod",2),"thirdUser"->User("Adam",46),"FourthUser"->User("Adam",2),"FithUser"->User("Adam",20))
  println(testNumberFrodos(map))
  print("3 - Задание: ")
  print(testUnderaged(map))

}