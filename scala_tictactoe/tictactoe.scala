/** Represents the state of a cell
  * 
  * @constructor create a new cell with a number
  * @param number number of the cell
  */
import java.util.Random

class Cell {
  private var _value = " "
  
  def value = _value
  def value_= (v: String):Unit = _value = v
   
}

/** A TicTacToe player
 *  
 *  @constructor creates a new player with a name and sign
 *  @param name player's name
 *  @param sign player's sign, X or O
 */
class Player(_name: String, _sign: String) {
  def name = _name  
  def sign = _sign    
}

/** The TicTacToe board
 *  
 */
class Board {
  
  val cells = List.fill(9)(new Cell)
  val winning_paths = List(
      List(0,1,2),
      List(3,4,5),
      List(6,7,8),
      List(0,3,6),
      List(1,4,7),
      List(2,5,8),
      List(0,4,8),
      List(2,4,6)
      )
  
  var last_move: Move = null.asInstanceOf[Move]

  def new_move(number: Int, p: Player): Move = {
    if (number > cells.size)
      throw new IllegalArgumentException("invalid move")
    
    var cell = cells(number)
    if (cell.value != " ") 
      throw new IllegalArgumentException("invalid move by " + p.name + ": " + p.sign + " on cell " + cells.indexOf(cell))
    
    cell.value_=(p.sign)
    println(p.name + " placed " + cell.value + " on cell " + cells.indexOf(cell))
    println(this)
    last_move = new Move(cell, p)
    last_move
  } 

  def won():Boolean = {
    winning_paths.foreach { path =>
      var allsame = ""  
      path.foreach { p =>
        allsame += cells.apply(p).value
      }
      var line = allsame.replace(" ", "").toList
      if (line.size == 3 && line.distinct.size == 1) {
        return(true)
      }
    }
    return(false)
  }
  
  def ended():Boolean = {
    cells.foreach { c =>
      if (c.value == " ")
        return false
    }
    return true
  }
  
  override def toString = {
    var sb = ""
    cells.foreach { c =>
      sb += " " + c.value + "|"
      if (cells.indexOf(c) % 3 == 2)
       sb += "\n"        
  	}
    sb
  }
}

/** Represents a move on the board
 *  
 *  @constructor creates a new move
 *  @param cell used cell by the move
 *  @param p player who places the move
 */
class Move(cell: Cell, p: Player) {
  val player = p
}

/** 
 * 
 * @constructor creates a new game with a new board and two players
 * @param board the game board
 * @param p1 first player
 * @param p2 second player
 */
class Game(board: Board, p1: Player, p2: Player) {

  var current_player = p1
  
  def next_player():Player = (
    if (current_player == p1) {
      current_player = p2      
	  current_player
    }
    else {
      current_player = p1      
	  current_player
    }
  )
  
  def run() = {
    var r = new scala.util.Random
    while (!(board.won() || board.ended())) {
      try {
        board.new_move(r.nextInt(9), current_player)
        next_player()
      } catch {
        case e:IllegalArgumentException =>
      }
    }

    if (board.won())
      println(board.last_move.player.name + " won the game!")
    else if (board.ended())
      println("draw game!")
    
  }
}

var game = new Game(new Board, new Player("Ismail", "x"), new Player("René", "o"))
game.run

