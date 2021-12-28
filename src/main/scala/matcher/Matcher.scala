package matcher

import java.util.Scanner
import scala.annotation.tailrec

object Matcher {
  def apply(useRegex: Boolean): Matcher ={
    if (useRegex) new WithRegexMatcher else new WithoutRegexMatcher
  }
}

abstract class Matcher {
  protected final val scanner = new Scanner(System.in)
  var pattern: String = ".*" //публичный доступ к полю только ради тестов

  @tailrec
  final def scan(): Unit = {
    println(s"current pattern is:\"$pattern\". insert row to match")

    scanner.next() match {
      case "np" => { //np - new pattern. зарезервированная последовательность символов для смены паттерна
        println(s"current pattern is:$pattern. insert new pattern")
        val input = scanner.next()
        if (validatePattern(input)) pattern = input
        else println("input is invalid, pattern should be 1 <= length <= 30 and contains only lower case letters and\\or \".\", \"*\" chars.")
      }
      case string => {
        if (validateMatchingRow(string)) println("RESULT: " + processMatching(string))
        else println("input is invalid, matching row should be 1 <= length <= 20 and contains only lower case letters.")
      }
    }
    scan()
  }

  protected def validatePattern(input: String): Boolean

  protected def validateMatchingRow(input: String): Boolean

  def processMatching(string: String): Boolean
}


class WithoutRegexMatcher() extends Matcher {

  override protected def validatePattern(input: String): Boolean = {
    //исключение ** определил исходя из условия задачи, в котором наличие предыдущего символа обязательно
    input.nonEmpty && input.length <= 30 && input.toList.forall(ch => (ch.isLower && ch.isLetter) || ch == '*' || ch == '.') && !input.contains("**")
  }

  override protected def validateMatchingRow(input: String): Boolean = {
    input.nonEmpty && input.length <= 20 && input.toList.forall(ch => ch.isLower && ch.isLetter)
  }

  /**
   *Идея проверки в том, что бы оперировать указателями на индекс элементов паттерна и анализируемой строки одновременно.
   *Строки считаются совпадающими, если указатели прошли паттерн и строку до конца, не считая * в конце паттерна.
   *И не совпадающими, если один из указателей достиг конца строки, но другой еще нет. (Например: а и ааа)
   */
  override def processMatching(string: String): Boolean = {
    if (pattern.startsWith("*")) pattern = s".$pattern" //предположим, что введенный паттерн "*..." допустимо считать как ".*..."

    @tailrec
    def matchByChars(patternCharIdx: Int, stringCharIdx: Int): Boolean = {
      val patternIsEnded = if (pattern.endsWith("*")) pattern.length - 1 <= patternCharIdx else pattern.length <= patternCharIdx
      val stringIsEnded = string.length <= stringCharIdx
      val nextCharIsStar = pattern.length -1 > patternCharIdx && pattern.charAt(patternCharIdx + 1) == '*'
      val matchStep = (patternChar: Char, stringCharIdx: Int) => !stringIsEnded && (patternChar == '.' || string(stringCharIdx) == patternChar)

      if (patternIsEnded && stringIsEnded) true
      else if (patternIsEnded && !stringIsEnded && !pattern.endsWith("*")) false
      else {
        pattern(patternCharIdx) match {
          //если указатель на '*' - берем предыдущий символ для сравнения.
          //задерживаем указатель на * пока символ до * в паттерне совпадает с символами в анализируемой строке
          case '*' => {
            if (matchStep(pattern(patternCharIdx - 1), stringCharIdx)) matchByChars(patternCharIdx, stringCharIdx + 1)
            else if (!patternIsEnded) matchByChars(patternCharIdx + 1, stringCharIdx)
            else false
          }
          case any => {
            if (nextCharIsStar) matchByChars(patternCharIdx + 1, stringCharIdx)
            else if (matchStep(any, stringCharIdx)) matchByChars(patternCharIdx + 1, stringCharIdx + 1)
            else false
          }
        }
      }
    }
    matchByChars(0, 0)
  }
}


class WithRegexMatcher() extends Matcher {
  override def processMatching(string: String): Boolean = string.matches(s"$pattern")

  override protected def validatePattern(input: String): Boolean = input.matches("[a-z\\*\\.]{1,30}")

  override protected def validateMatchingRow(input: String): Boolean = input.matches("[a-z]{1,20}")
}
