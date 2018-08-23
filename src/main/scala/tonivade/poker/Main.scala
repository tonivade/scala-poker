package tonivade.poker

import cats.data.State
import cats.data.State._

object Main extends App {
  import Game._
  import GameHand._
  import Console._
  import BetTurn._
  
  val players = List(Player("pepe"), Player("paco"), Player("toni"), Player("curro"), Player("perico"))
  
  def runBetLoop(hand: GameHand): State[Deck, GameHand] = pure(betLoop.runS(hand).value)
  
  def gameLoop(game: Game): State[Deck, (Player, FullHand)] = 
    for {
      _ <- print(game)
      preFlop <- nextGameHand(game)
      _ <- print(s"current pot ${preFlop.pot}")
      preFlopBets <- runBetLoop(preFlop)
      _ <- print(preFlopBets)
      flop <- nextPhase(preFlopBets)
      _ <- print(s"current pot ${flop.pot}")
      flopBets <- runBetLoop(flop)
      _ <- print(flopBets)
      turn <- nextPhase(flopBets)
      _ <- print(s"current pot ${turn.pot}")
      turnBets <- runBetLoop(turn)
      _ <- print(turnBets)
      river <- nextPhase(turnBets)
      _ <- print(s"current pot ${river.pot}")
      riverBets <- runBetLoop(river)
      _ <- print(riverBets)
      showdown <- nextPhase(riverBets)
      _ <- print(showdown)
      player <- winner(showdown)
    } yield player.get
  
  val result = 
    for {
      game <- start(players)
      winner <- gameLoop(game)
    } yield winner
 
  val win = result.runA(Deck.shuffle).value
  
  println(win)
}

object Console {
  import scala.io.StdIn.readLine
  import cats.data.State._
  
  def exit[S]: State[S, Unit] = pure(())

  def print[S](value: Any): State[S, Unit] = pure(println(value))
  
  def read[S]: State[S, String] = pure(readLine())
}