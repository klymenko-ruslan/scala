package scalaSnake

/**
 * Created by klymenko.ruslan on 25.04.2015.
 */
object Client {
  def main(args : Array[String]) = new TheSnakeGame(SnakeGameField.DEFAULT_FIELD_SIZE, Speed.MIDDLE)
}
