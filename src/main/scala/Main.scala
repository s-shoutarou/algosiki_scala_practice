@main def hello: Unit =
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"

import scala.io.StdIn
import scala.annotation.nowarn

def q1_1 = {
    val num = StdIn.readLine().toInt

    var oneCoinNum = 0
    var fiveCoinNum = 0

    fiveCoinNum = num / 5
    oneCoinNum = num - (fiveCoinNum * 5)

    println(fiveCoinNum + oneCoinNum)
}

def q1_2 = {
  var num = StdIn.readLine().toInt
  var count = 0
  
  while (0 < num) {
    if(num % 2 == 0){
      num = num / 2
    } else {
      num = num - 1
    }
    count = count + 1
  }
  println(count)
}

def q1_3 = {
  var inputLine1 = StdIn.readLine().toInt
  val inputLine2 = StdIn.readLine().toString()

  val useCoinMaxList = inputLine2.split(" ").toList

  val resList = useCoinMaxList.zipWithIndex.map { case (element, index) => {
      val coin =  index match {
        case 0 => 50
        case 1 => 10
        case 2 => 5
        case 3 => 1
      }
      val useCoin =  calcUseCoin(coin, inputLine1, 1, element.toInt)
      inputLine1 -= useCoin * coin
      useCoin
    }
  }
  val res = resList.foldLeft(0){ (amount, num) => amount.toInt + num}
  println(res)
}

@scala.annotation.tailrec
def calcUseCoin(coin:Int, nowTarget: Int, use: Int, max:Int): Int = {
  if (nowTarget - coin * use >= 0 && use <= max) {
    calcUseCoin(coin, nowTarget, use + 1, max)
  } else {
    return use - 1
  }
}



def q1_4 = {
  val inputLine1 = StdIn.readLine().toString().split(" ").toList.map(_.toInt)
  var inputLine2 = StdIn.readLine().toString().split(" ").toList.map(_.toInt)
  var inputLine3 = StdIn.readLine().toString().split(" ").toList.map(_.toInt)

  var resList = List.empty[Int]
  inputLine2.foreach{ i =>
    val tmp = inputLine3.filter(num => {
      i <= num
    })
    if(tmp.lift(0).getOrElse(0) != 0){
      resList = resList :+ tmp(0)
      inputLine3 = inputLine3.filter(_ != tmp(0))
    }
  }
  println(resList.size)
}

def q1_4_functional= {
  val inputLine1 = StdIn.readLine().toString().split(" ").toList.map(_.toInt)
  val N = inputLine1(0)
  val M = inputLine1(1)

  val A = StdIn.readLine().toString().split(" ").toList.map(_.toInt)
  val B = StdIn.readLine().toString().split(" ").toList.map(_.toInt)

  val (_, res) = B.foldLeft((A, 0)){ case ( (remainingBalls, count), box ) =>
    remainingBalls.find(_ <= box) match {
      case Some(ball) => (remainingBalls.filterNot(_ == ball), count + 1)
      case None => (remainingBalls, count)
    }
  }
  println(res)
}

def q1_5 = {
  val inputLine1 = StdIn.readLine().toInt
  val res = calcQ1_4(inputLine1, 1)

  println(res)
}

def calcQ1_4(target: Int ,count: Int): Int = {
  target match
    case x if target % 3 == 0 && target / 3 >= 1 => calcQ1_4(target / 3, count + 1)
    case x if target % 3 != 0 && target - 1 >= 1  => calcQ1_4(target - 1, count + 1)
    case _ => count
}

/* def fail_calcQ1_4(target: Int, nowNum :Int, count: Int): Int = {
  nowNum match
    case x if nowNum * 3 < target => calcQ1_4(target ,nowNum * 3, count + 1)
    case x if nowNum < target => calcQ1_4(target ,nowNum + (target - nowNum), count + (target - nowNum))
    case _ => count
} */