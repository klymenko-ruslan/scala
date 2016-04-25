package snake

import scala.collection.mutable
import scala.util.Random

/**
 * Created by klymenko.ruslan on 25.04.2015.
 */
class TheSnakeGame(fieldSize : Int, speedParam : Int) {
  private val FIELD_SIZE = fieldSize
  private var fieldData = Array.ofDim[Char](fieldSize,fieldSize)
  private var foodCoordinate : Coordinate = _
  private val snake = new Snake

  start(FIELD_SIZE, speedParam)

  def this() = this(SnakeGameField.DEFAULT_FIELD_SIZE, Speed.MIDDLE)
  def this(fieldSize : Int) = this(fieldSize, Speed.MIDDLE)

  private def setUpField = {
    fieldData = Array.fill[Char](fieldSize, fieldSize)(SnakeGameField.EMPTY_CELL)
    setSnakeToStartPosition
    createAndFillFood
  }

  private def setSnakeToStartPosition = {
    val center = FIELD_SIZE / 2 - 1
    fieldData(center)(center) = SnakeGameField.SNAKE_CELL
    snake addSnakeElement(new Coordinate(center, center))
  }

  private def createAndFillFood = {
    foodCoordinate = createFoodCoordinate
    fillFoodByCoordinate(foodCoordinate)
  }

  private def fillFoodByCoordinate(foodCoordinate : Coordinate) =
    fieldData(foodCoordinate.y)(foodCoordinate.x) = SnakeGameField.FOOD_CELL

  private def createFoodCoordinate = {
    val random = Random
    var foodCoordinate = new Coordinate(random.nextInt(FIELD_SIZE), random.nextInt(FIELD_SIZE))
    while(snake.getSnakeBody.contains(foodCoordinate))
      foodCoordinate = new Coordinate(random.nextInt(FIELD_SIZE), random.nextInt(FIELD_SIZE))
    foodCoordinate
  }

  private def moveSnake(direction : Char) = {
    val newSnakeFace = getSnakeFaceByDirection(direction)
    checkCrash(newSnakeFace)
    snake.addSnakeFace(newSnakeFace)
    fillSnake
    if(foundFood) createAndFillFood else removeSnakeTail
  }

  private def removeSnakeTail = {
    fieldData(snake.getSnakeTail.y)(snake.getSnakeTail.x) = SnakeGameField.EMPTY_CELL
    snake.removeSnakeTail
  }

  private def getSnakeFaceByDirection(direction: Char) = {
    var newSnakeFace = new Coordinate(snake.getSnakeFace.x, snake.getSnakeFace.y)
    direction match {
      case Direction.LEFT => if(newSnakeFace.x - 1 >= 0) newSnakeFace.x = newSnakeFace.x - 1 else newSnakeFace.x = FIELD_SIZE - 1
      case Direction.RIGHT => if(newSnakeFace.x + 1 < FIELD_SIZE) newSnakeFace.x = newSnakeFace.x + 1 else newSnakeFace.x = 0
      case Direction.TOP => if(newSnakeFace.y - 1 >= 0) newSnakeFace.y = newSnakeFace.y - 1 else newSnakeFace.y = FIELD_SIZE - 1
      case Direction.DOWN => if(newSnakeFace.y + 1 < FIELD_SIZE) newSnakeFace.y = newSnakeFace.y + 1 else newSnakeFace.y = 0
      case default =>
    }
    newSnakeFace
  }

  private def fillSnake = {
    fieldData(snake.getSnakeFace.y)(snake.getSnakeFace.x)= SnakeGameField.SNAKE_CELL
    fieldData(snake.getSnakeTail.y)(snake.getSnakeTail.x) = SnakeGameField.SNAKE_CELL
  }

  private def checkCrash(turnCell : Coordinate) = {
    if(snake.getSnakeBody.contains(turnCell)) {
      printResultInfo
      System.exit(1)
    }
    def printResultInfo = {
      println("Game over =( Your snake was " + snake.getSnakeBody.size + " meters in length.")
      println("Your snake conquered " + snake.getSnakeBody.size * 100 / (FIELD_SIZE * FIELD_SIZE)  + "% of the territory.")
    }
  }

  private def foundFood = snake.getSnakeFace.equals(foodCoordinate)

  private def printField = {
    println
    for(fieldRow <- fieldData) println(fieldRow.mkString)
  }

  private def start(fieldSize : Int, speed : Int) = {
    checkSpeed(speed)
    checkFieldSize(fieldSize)
    setUpField
    printInfoMessage

    val scanner = new java.util.Scanner(System.in)
    // Wait for user confirmation
    scanner.nextLine

    val actionThread = new ActionThread(speed, this)
    new Thread(actionThread) start

    listenToUser(scanner, actionThread)
  }

  private def checkSpeed(speed : Int) = if(speed <= 0) throw new IllegalArgumentException("Speed should be greater than 0")
  private def checkFieldSize(fieldSize : Int) = if(fieldSize <= 0) throw new IllegalArgumentException("Field size shoud be greater than 0")

  private def listenToUser(scanner : java.util.Scanner, actionThread : ActionThread) = {
    while(true) {
      val enteredString = scanner next
      val lastEnteredChar = enteredString charAt(enteredString.length - 1)
      val direction = lastEnteredChar
      if(isAllowedDirection(actionThread.currentDirection, direction)) actionThread.currentDirection = direction
    }
  }

  private def isAllowedDirection(currentDirection: Char, direction : Char) = {
    if(isUnknownDirection(direction)) false
    else if(direction == Direction.TOP && currentDirection == Direction.DOWN) false
    else if(direction == Direction.DOWN && currentDirection == Direction.TOP) false
    else if(direction == Direction.LEFT && currentDirection == Direction.RIGHT) false
    else if(direction == Direction.RIGHT && currentDirection == Direction.LEFT) false
    else true
  }

  private def isUnknownDirection(direction : Char) =
    direction != Direction.TOP && direction != Direction.DOWN && direction != Direction.LEFT && direction != Direction.RIGHT

  private def printInfoMessage = {
    println("w : up")
    println("s : down")
    println("a : left")
    println("d : right")
    println("Click ENTER to confirm direction")
    println()
    println("Please click ENTER to start")
  }

  private class Coordinate(xParam : Int, yParam : Int) {
    var x = xParam; var y = yParam
    override def equals(that : Any) = {
      that match {
        case f: Coordinate => f.x == x && f.y == y
        case _ => false
      }
    }
  }

  private class Snake {
    private val snakeBody : mutable.ListBuffer[Coordinate] = mutable.ListBuffer()
    def getSnakeBody = snakeBody
    def addSnakeElement(element : Coordinate) = snakeBody.+=(element)
    def getSnakeTail = snakeBody.last
    def getSnakeFace = snakeBody.head
    def addSnakeFace(snakeFace : Coordinate) = snakeBody.+=:(snakeFace)
    def addSnakeTail(snakeFace : Coordinate) = addSnakeElement(snakeFace)
    def removeSnakeTail() = snakeBody.remove(snakeBody.size - 1)
  }

  private class ActionThread(speed : Int, theSnakeGameP : TheSnakeGame) extends Thread {
    val sleepTime = speed
    val theSnakeGame = theSnakeGameP
    var currentDirection = Direction.RIGHT

    override def run = {
      while(true) {
        theSnakeGame moveSnake(currentDirection)
        theSnakeGame.printField
        Thread.sleep(sleepTime)
      }
    }
  }
}
object Speed {
  val SLOWEST = 900
  val SLOW = 700
  val MIDDLE = 500
  val QUICK = 300
  val QUICKEST = 100
}
object Direction {
  val TOP = 'w'
  val DOWN = 's'
  val LEFT = 'a'
  val RIGHT = 'd'
}
object SnakeGameField {
  val DEFAULT_FIELD_SIZE = 10
  val EMPTY_CELL = '-'
  val SNAKE_CELL = '+'
  val FOOD_CELL = '0'
}