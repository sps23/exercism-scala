import scala.annotation.tailrec
import scala.collection.immutable.{HashSet, Stream}
import scala.collection.mutable
import scala.util.Random

trait RobotName {

  def name: String
}

class Robot extends RobotName {

  private var _name: Option[String] = None

  def name: String = {
    val currentName = _name.getOrElse(Robot.generateName(2, 3))
    if (_name.isEmpty) {
      _name = Option(currentName)
    }
    currentName
  }

  def reset(): Unit = _name = None
}

object Robot {

  private val random = Random
//  private var usedNames: HashSet[String] = new HashSet[String]

  private def randomCapitalLetter: Stream[Char] = {
    def nextAlphaNum: Char = {
      val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      chars.charAt(random.nextInt(chars.length))
    }

    Stream continually nextAlphaNum
  }

//  @tailrec
  def generateName(nOfLetters: Int, nOfDigits: Int): String = {
    val letters: Seq[Char] = randomCapitalLetter.take(nOfLetters)
    val digits: Seq[Int] = (1 to nOfDigits).map(_ => random.nextInt(9))
    val name: String = letters.mkString + digits.mkString
    name
//    println(s"$name\t${usedNames.size}")
//    if(usedNames.contains(name)) {
//      generateName(nOfLetters, nOfDigits)
//    }
//    else {
//      usedNames += name
//      name
//    }
  }
}