@main def hello: Unit =
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"

import scala.io.StdIn
import scala.io.Source
import scala.annotation.nowarn
import scala.math._
import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.annotation.targetName

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

def q1_6 = {
  // 複数行の標準入力を読み込む
  val lines = scala.collection.mutable.ListBuffer[String]()
  var input = ""

  // 空行が入力されるまで入力を受け付ける
  while ({ input = scala.io.StdIn.readLine(); input.nonEmpty }) {
    lines += input
  }

  val tmp = lines.tail.map( v => v.split(" ").toList.map(_.toDouble)).filter(v => v.exists(_ > 0)).toList
  println(calcQ1_6(List(0,0), tmp, 0))
}

def calcQ1_6 (nowPoint:List[Double], points:List[List[Double]], nowAmount:Double): Double = {
  if (points.length > 0) {
    val distances = points.zipWithIndex.map { case (v, index) => List(index, calculateDistance(nowPoint(0), nowPoint(1),  v(0), v(1)))}

    val t = distances.sortBy(subList => subList(1).asInstanceOf[Double])
    val f = points.zipWithIndex.filter{ case (v, index) => index != t(0)(0).asInstanceOf[Int]}.map(_.productIterator.toList).map(_(0))
    calcQ1_6(points(t(0)(0).asInstanceOf[Int]), 
            f.asInstanceOf[List[List[Double]]], 
            nowAmount + t(0)(1).asInstanceOf[Double])
  } else if (points.length  == 0) {
    nowAmount + calculateDistance(nowPoint(0), nowPoint(1), 0.0, 0.0)
  }
  else {
    0
  }
}

  def calculateDistance(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    sqrt(pow(x2 - x1, 2) + pow(y2 - y1, 2))
  }




  ///////////////
  object TSPGreedy {

  type Point = (Double, Double)

  def main(args: Array[String]): Unit = {
    // 複数行の標準入力を読み込む
    val lines = Iterator.continually(scala.io.StdIn.readLine()).takeWhile(_.nonEmpty).toList

    // 入力行から座標のリストを生成
    val points = lines.tail.map(v => v.split(" ").toList.map(_.toDouble) match {
      case x :: y :: Nil => (x, y)
      case _ => throw new IllegalArgumentException("Invalid input format")
    }).filter(point => point._1 > 0 || point._2 > 0)

    // 巡回セールスマン問題を解く
    val tour = solveTSP(List((0.0, 0.0), points.head), points.tail)
    val totalDistance = calculateTotalDistance(tour)

    // 結果を表示
    println(s"Optimal Tour: $tour")
    println(s"Total Distance: $totalDistance")
  }

  // 巡回セールスマン問題を解く関数
  def solveTSP(currentTour: List[Point], remainingPoints: List[Point]): List[Point] = {
    if (remainingPoints.isEmpty) {
      currentTour.reverse
    } else {
      // 最も近い未訪問の都市を求め、それを次の都市として追加
      val nearestPoint = findNearestPoint(currentTour.head, remainingPoints)
      solveTSP(nearestPoint :: currentTour, remainingPoints.filterNot(_ == nearestPoint))
    }
  }

  // 最も近い都市を求める関数
  def findNearestPoint(currentPoint: Point, remainingPoints: List[Point]): Point = {
    remainingPoints.minBy(point => calculateDistance(currentPoint, point))
  }

  // 巡回経路の総距離を計算する関数
  def calculateTotalDistance(tour: List[Point]): Double = {
    tour.sliding(2).foldLeft(0.0) { (acc, pair) =>
      acc + calculateDistance(pair.head, pair.last)
    }
  }

  // 2つの座標の距離を計算する関数
  def calculateDistance(point1: Point, point2: Point): Double = {
    val (x1, y1) = point1
    val (x2, y2) = point2
    math.sqrt(math.pow(x2 - x1, 2) + math.pow(y2 - y1, 2))
  }
}


def q1_7 = {
  val events = List((10,20), (12,15), (15,18), (20,22))
  val res = calcSchedule(events, (0, 0))

  println(res)
}

def makePerfectSchedule (events: List[(Int, Int)], target: (Int, Int), nowResult:List[(Int, Int)]) = {
  if(events.nonEmpty){
    val tmp = calcSchedule(events ,target)
    makePerfectSchedule(events.filterNot(v => v._1 == tmp._1 && v._2 == tmp._2), tmp, tmp::nowResult)
  } else {
    nowResult
  }
}

def calcSchedule (events: List[(Int, Int)], target: (Int, Int))= {
  val sortedList = events.sortBy(_._1)
  val minDiffNowEnd = sortedList.map( v => v._1 - target._2).filter(_ >= 0).minBy(v => v)
  val tmpEvents = events.filter(v => v._1 - target._2 <= minDiffNowEnd)
  val minDiff = tmpEvents.map( v => v._2 - v._1).filter(_ >= 0).minBy(v => v)
  println(minDiffNowEnd)
  println(minDiff)

  tmpEvents.find(v => v._2 - v._1 == minDiff)
}
