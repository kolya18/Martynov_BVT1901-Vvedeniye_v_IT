
sealed trait Option[A] {

  def isEmpty: Boolean
}
case class Some[A](a: A) extends Option[A] {
  val isEmpty = false
}
case class None[A]()     extends Option[A] {
  val isEmpty = true
}

//a) Создать класс Animal, который имеет следующие поля:
 class Animal {
  var name: String = _
  var species: String = _
  var food: String = _
}

//b) Создайте объект-компаньон для класса Animal и добавьте следующие сущности как поля:
object Animal {

  def add(name: String, species: String, food: String): Animal = {
        var a1 = new Animal
        a1.name = name
        a1.species = species
        a1.food = food
        return a1
    }

  val a1 = add("cat","mammal","meat")
  val a2 = add("parrot","bird","vegetables")
  val a3 =  add("goldfish","fish","plants")

//c) Добавьте следующие метод в Animals: который проверяет ест ли животное определенную пищу
    def eats(animal: Animal,food: String): Boolean = {
      if (animal.food == food) {return true}
      else return false
    }

//  d) Переопределите ваш класс Animal как трейт и создайте объекты класса-образца для Mammals, Birds и Fishs.
//   Вам все еще нужно поле `species`?

  //f) Создайте трейт Food со следующими классами-образцами:
  sealed trait Food
  case object Meat extends Food
  case object Vegetables extends Food
  case object Plants extends Food

  sealed trait Animals {
    val name: String
    val food: Food
  }
  case class Mammal(name: String, food: Food) extends Animals
  case class Birds(name: String, food: Food) extends Animals
  case class Fishs(name: String, food: Food) extends Animals

  val a4: Animals = Mammal("cat",Meat)
  val a5: Animals = Birds("parrot",Vegetables)
  val a6: Animals = Fishs("goldfish",Plants)

//e) Добавьте следующие функции в объект-компаньон Animal:

  // true если это имя одного из трех животных из (b)
  def knownAnimal(name: String, a: Animal): Boolean = {
   if  (a.name == name) {return true}
    else return false
  }


// возвращает одно из трех животных в соответствии с именем
def applyAnimals(name: String, a1: Animals, a2: Animals, a3: Animals): Animals = name.toLowerCase match {
  case "cat" => a1.name match {
    case "cat" => a1
    case _ => a2.name match {
      case "cat" => a2
      case _ => a3.name match {
        case "cat" => a3
      }
    }
  }
  case "parrot" => a1.name match {
    case "parrot" => a1
    case _ => a2.name match {
      case "parrot" => a2
      case _ => a3.name match {
        case "parrot" => a3
      }
    }
  }
  case "goldfish" => a1.name match {
    case "goldfish" => a1
    case _ => a2.name match {
      case "goldfish" => a2
      case _ => a3.name match {
        case "goldfish" => a3
      }
    }
  }
}

  // возвращает одно из трех животных в соответствии с едой
  def applyFood(food: String, a1: Animals, a2: Animals, a3: Animals ): Animals = food.toLowerCase match {
    case "meat" => a1.food match {
      case Meat => a1
      case _ => a2.food match {
        case Meat => a2
        case _ => a3.food match {
          case Meat => a3
        }
      }
    }

    case "vegetables" => a1.food match {
      case Vegetables => a1
      case _ => a2.food match {
        case Vegetables => a2
        case _ => a3.food match {
          case Vegetables => a3
        }
      }
    }

    case "plants" => a1.food match {
      case Plants => a1
      case _ => a2.food match {
        case Plants => a2
        case _ => a3.food match {
          case Plants => a3
        }
      }
    }

  }




  }

