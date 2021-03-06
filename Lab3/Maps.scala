

/** Напишите вашу реализацию в тестовые функции.
  * 
  * https://docs.scala-lang.org/overviews/collections/maps.html
  */
object Maps {

  case class User(name: String, age: Int)

  /* a) В данной Seq[User] сгруппируйте пользователей по имени (`groupBy`) и вычислите средний возраст: `name -> averageAge`
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testGroupUsers(users: Seq[User]): Map[String, Int] = {
    var map: Map[String, Int] = Map()
    var name = users.groupBy(_.name)
    var average: Int = 0
    for (e <- users) {
      val age: Int = e.age
      average = average + age
    }
    average = average / name.size
    for (e <- name) {
      map += (e._1 -> average)
    }
    map
  }

  val u = Seq(User("Kolya", 21), User("Nick", 18), User("Kostya", 100))
  testGroupUsers(u)


  /* b) Дана `Map[String, User]` состоящая из имен пользователей `User`, сколько имен пользователей, содержащихся в Map, содержат подстроку "Adam"?
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testNumberFrodos(map: Map[String, User]): Int = {
    var a: Int = 0
    for (num <- map) {
      if (num._2.name.indexOf("Adam") != (-1))
        a += 1
    }
    a
  }

  val u2 = Map("1" -> User("Adam", 21), "2" -> User("Kolya", 19), "3" -> User("Kostya", 100))
  testNumberFrodos(u2)

  /* c) Удалите всех пользователей возраст которых менее 35 лет.
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testUnderaged(map: Map[String, User]): Map[String, User] = {
    map.filter(k => k._2.age >= 35)
  }
  testUnderaged(u2)
}
