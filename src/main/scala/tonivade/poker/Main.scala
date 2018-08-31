package tonivade.poker

import cats.effect.IO
import cats.data.StateT
import cats.data.StateT._

object Main extends App {
  import GameHand._
  
  val players = List(Player("pepe"), Player("paco"), Player("toni"), Player("curro"), Player("perico"))
  
  val winner = runHandLoop(new Game(players))
  
  println(winner)
}