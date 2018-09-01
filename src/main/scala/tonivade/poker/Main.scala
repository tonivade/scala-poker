package tonivade.poker

import cats.effect.IO
import cats.data.StateT
import cats.data.StateT._

object Main extends App {
  import GameHand._
  
  val players = List(Player("pepe"), Player("paco"), Player("toni"), Player("curro"), Player("perico"))
  
  val result: GameHand = runHandLoop(new Game(players))
  
  println(result.players)
  println(result.winner)
  println(result.winner.map(_._2.bestHand))
}