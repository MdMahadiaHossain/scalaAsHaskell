import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.annotation.tailrec
import scala.math.BigInt


def add: Int => Int => Int= a => b => a+b

println(add(10)(20))


def list : Int => ((Int,Int)) => ((Int,(Int,Int))) =  head => tail => (head,tail)


def fac: BigInt => BigInt = a => a match{
  case a if a.intValue==0 => BigInt(1)
  case _ => a*fac(a-1)
}


def tailFac : BigInt => BigInt => BigInt = param => accum => param match {
  case p if p.intValue==0 => accum
  case _ => tailFac(param-1)(accum*param)
}


def factorial(n: BigInt): BigInt = {
  @tailrec
  def iter(x: BigInt, result: BigInt): BigInt =
    if (x.intValue == 0 ) result
    else iter(x - 1, result * x)

  iter(n, 1)
}


