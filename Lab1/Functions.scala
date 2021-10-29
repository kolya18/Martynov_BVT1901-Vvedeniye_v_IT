
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

  /* a) Напишите функцию, которая рассчитывает площадь окружности
   *    r^2 * Math.PI
   */
  def area(r: Double): Double = { return math.Pi*r*r }

  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
 // def testCircle(r: Double): Double = r
  def testCircle(r: Double): Double = area(r)

  /* b) Напишите карированную функцию которая рассчитывает площадь прямоугольника a * b.
   */
  val curriedSpr: Double => Double => Double = a => b => a*b

  // примените вашу функцию из пукта (b) здесь, не изменяя сигнатуру
  def testRectangleCurried(a: Double, b: Double): Double = curriedSpr(a)(b)


  // c) Напишите не карированную функцию для расчета площади прямоугольника.
  val Spr: (Double,Double) => Double = (a,b) => a*b

  // примените вашу функцию из пункта (c) здесь, не изменяя сигнатуру
  def testRectangleUc(a: Double, b: Double): Double = Spr(a,b)
}
