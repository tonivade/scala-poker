package tonivade.poker

import cats.data.State
import cats.data.State._

object Main extends App {
  import Game._
  import GameHand._
  import Console._
  import BetTurn._
  
  val players = List(Player("pepe"), Player("paco"), Player("toni"), Player("curro"), Player("perico"))
  
  def runBetLoop(hand: GameHand): State[Deck, GameHand] = pure(betLoop.runS(BetTurn.from(hand)).value.hand)
  
  def gameLoop(game: Game): State[Deck, (Player, FullHand)] = 
    for {
      _ <- print(game)
      preFlop <- nextGameHand(game)
      flop <- phaseLoop(preFlop)
      turn <- phaseLoop(flop)
      river <- phaseLoop(turn)
      showdown <- phaseLoop(river)
      player <- winner(showdown)
    } yield player.get
    
  def phaseLoop(hand: GameHand): State[Deck, GameHand] = 
    for {
      _ <- print(s"current pot ${hand.pot} in ${hand.phase}")
      bets <- runBetLoop(hand)
      _ <- print(bets)
      nextHand <- nextPhase(bets)
      _ <- print(nextHand)
    } yield nextHand
  
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
  
  def noop[S]: State[S, Unit] = pure(())

  def print[S](value: Any): State[S, Unit] = pure(println(value))
  
  def read[S]: State[S, String] = pure(readLine())
}