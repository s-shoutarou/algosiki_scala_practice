@main def hello: Unit =
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"

import scala.io.StdIn

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