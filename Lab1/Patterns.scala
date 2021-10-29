
/** Напишите решение в виде функции. 
  * 
  * Синтаксис:
  *   val a: Int = ???
  * 
  *   a match {
  *     case 0 => true
  *     case _ => false
  *   }
  */

object PatternMatching {

  sealed trait Hand
  case object Rock    extends Hand
  case object Paper   extends Hand
  case object Scissor extends Hand

  sealed trait Result
  case object Win  extends Result
  case object Lose extends Result
  case object Draw extends Result

  sealed trait Food
  case object Meat       extends Food
  case object Vegetables extends Food
  case object Plants     extends Food

  sealed trait Animal {
    val name: String
    val food: Food
  }
  case class Mammal(name: String, food: Food, weight: Int) extends Animal
  case class Fish(name: String, food: Food)                extends Animal
  case class Bird(name: String, food: Food)                extends Animal



  /* a) Напишите функцию, которая ставит в соответствие числу строку слудеющим образом:
   * Если:
   *     1 => "it is one"
   *     2 => "it is two"
   *     3 => "it is three"
   *     иначе => "what's that"
   */


/*  def InttoStr (value: Int): String = {
 if (value == 1) {return "it is one"}
 else if (value == 2) {return "it is two"}
 else if (value == 3) {return "it is three"}
 else { return "what's that"}
} */

  def toStr(value: Int): String = value match {
    case 1 => "it is one"
    case 2 => "it is two"
    case 3 => "it is three"
    case _ => "what's that"
  }

// примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
// def testIntToString(value: Int): String = value.toString
//def testIntToString(value: Int): String = InttoStr(value)
def testIntToString(value: Int): String = toStr(value)

  /* b) Напишите функцию которая возвращает true если переменная `value` принимает значение:
  *     "max" или "Max
  *     "moritz" или "Moritz"
  */

/*def MaxAndMoritz(value: String): Boolean = {
 if (value.toLowerCase == "max" || value.toLowerCase == "moritz") {return true}
 else return false
}*/

  def IsMaxAndMoritz(value: String): Boolean = value.toLowerCase match {
    case "max" => true
    case "moritz" => true
    case _ => false
  }

// примените функции из пункта (b) здесь, не изменяя сигнатуру
//def testIsMaxAndMoritz(value: String): Boolean = false
//def testIsMaxAndMoritz(value: String): Boolean = MaxAndMoritz(value)
def testIsMaxAndMoritz(value: String): Boolean = IsMaxAndMoritz(value)

// c) Напишите функцию проверки является ли `value` четным
  def IsEven(value: Int): Boolean = (value%2) match {
    case 0 => true
    case _ => false
  }


// примените функции из пункта (c) здесь, не изменяя сигнатуру
//def testIsEven(value: Int): Boolean = false
def testIsEven(value: Int): Boolean = IsEven(value)


/* d) Напишите функцию, моделирующую игру в Камень ножницы бумага
*     1. камень побеждает ножницы
*     2. ножницы побеждают бумагу
*     3. бумага побеждает камень
*    Выиграет ли игрок `a`?
*/
  Rock
  Paper
  Scissor

  Win
  Lose
  Draw

  def WinsA(a: Hand, b: Hand): Result = a match {
    case Rock => b match {
      case Scissor => Win
      case Paper => Lose
      case Rock => Draw
    }
    case Paper => b match {
      case Scissor => Lose
      case Paper => Draw
      case Rock => Win
    }
    case Scissor => b match {
      case Scissor => Draw
      case Paper => Win
      case Rock => Lose
    }
  }

// примените вашу функцию из пункта (d) здесь, не изменяя сигнатуру
//def testWinsA(a: Hand, b: Hand): Result = Draw
def testWinsA(a: Hand, b: Hand): Result = WinsA(a, b)


// Примечание: используйте определение Animals

// e) Верните вес (weight: Int) объекта Mammal, иначе верните -1.


  val Mammal1 = new Mammal("Mammal1",Meat,50)
  val Mammal2 = new Mammal("Mammal2",Meat, 80)
  val Test1 = new Fish("Fish1",Plants)

  def ExtractMammalWeight(animal: Animal): Int = animal match {
    case Mammal(name, food, weight) => weight
    case _ => -1
  }

// примените функцию из пункта (e) здесь, не изменяйте сигнатуру
def testExtractMammalWeight(animal: Animal): Int = ExtractMammalWeight(animal)


// f) Измените поле еда объектов классов Fishes и Birds на Plants, класс Mammels оставьте неизмененным.

  val Fish1 = new Fish("Piranha",Meat)
  val Bird1 = new Bird("Kesha",Vegetables)

  def UpdateFood (animal: Animal): Animal = animal match {
    case Fish(name, food) => Fish(name,Plants)
    case Bird(name, food) => Bird(name,Plants)
    case Mammal(name, food, weight) => Mammal(name, food, weight)
  }



// примените функцию из пункта (f) здесь, не изменяйте сигнатуру
//def testUpdateFood(animal: Animal): Animal = animal
def testUpdateFood(animal: Animal): Animal = UpdateFood(animal)
}
