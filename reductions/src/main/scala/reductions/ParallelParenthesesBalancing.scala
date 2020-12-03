package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceRec(currentChars: List[Char], count: Int): Boolean = currentChars match {
      case _ if (count < 0) => false
      case Nil => count == 0
      case char :: rest if(char == '(') => balanceRec(rest, count + 1)
      case char :: rest if(char == ')') => balanceRec(rest, count - 1)
      case char :: rest => balanceRec(rest, count)
    }

    balanceRec(chars.toList, 0) 
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int,Int) = {
      var arg1t = arg1
      var arg2t = arg2
      for(i <- idx until until) {
        chars(i) match {
          case '(' => arg1t += 1
          case ')' => arg2t += 1
          case _ =>
        }
      }
      (arg1,arg2)
    }

    def reduce(from: Int, until: Int): (Int,Int) = {
      if(until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val middle = from + (until-from)/2
        val (left, right) = parallel(reduce(from, middle), reduce(middle, until))
        List(left,right).reduce((a,b) => (a._1 - b._1,a._2 - b._2))
      }
    }

    reduce(0, chars.length) match {
      case (x,y) if (x == -y) => true
      case _ => false
    }
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
