import matcher.Matcher
import org.scalatest.flatspec.AnyFlatSpec

class AnyMatchingSpec extends AnyFlatSpec {


  {
    //определим свой матчер для каждого из асинхронного потока тестирования
    val matcher = Matcher(false)
    matcher.pattern = "a"
    "pattern \"a\"" should "match with \"a\" row" in assert(matcher.processMatching("a"))
    it should "not match with \"aa\" row" in assert(!matcher.processMatching("aa"))
    it should "not match with \"b\" row" in assert(!matcher.processMatching("b"))
  }

  {
    val matcher = Matcher(false)
    matcher.pattern = "."
    "pattern \".\"" should "match with \"a\" row" in assert(matcher.processMatching("a"))
    it should "match with \"b\" row" in assert(matcher.processMatching("b"))
    it should "not match with \"aa\" row" in assert(!matcher.processMatching("aa"))
  }

  {
    val matcher = Matcher(false)
    matcher.pattern = "i*"
    "pattern \"i*\"" should "match with \"i\" row" in assert(matcher.processMatching("i"))
    it should "match with \"ii\" row" in assert(matcher.processMatching("ii"))
    it should "not match with \"aa\" row" in assert(!matcher.processMatching("aa"))
  }

  {
    val matcher = Matcher(false)
    matcher.pattern = "i*p"
    "pattern \"i*p\"" should "match with \"p\" row" in assert(matcher.processMatching("p"))
    it should "match with \"ip\" row" in assert(matcher.processMatching("ip"))
    it should "match with \"iip\" row" in assert(matcher.processMatching("iip"))
    it should "not match with \"ii\" row" in assert(!matcher.processMatching("ii"))
  }

  {
    val matcher = Matcher(false)
    matcher.pattern = ".*"
    "pattern \".*\"" should "match with \"p\" row" in assert(matcher.processMatching("p"))
    it should "match with \"ip\" row" in assert(matcher.processMatching("ip"))
    it should "match with \"iip\" row" in assert(matcher.processMatching("iip"))
  }

  {
    val matcher = Matcher(false)
    matcher.pattern = "..pi*"
    "pattern \"..pi*\"" should "match with \"aap\" row" in assert(matcher.processMatching("aap"))
    it should "match with \"iipii\" row" in assert(matcher.processMatching("iipii"))
    it should "match with \"zzpiiii\" row" in assert(matcher.processMatching("zzpiiii"))
    it should "not match with \"ii\" row" in assert(!matcher.processMatching("ii"))
  }

}
