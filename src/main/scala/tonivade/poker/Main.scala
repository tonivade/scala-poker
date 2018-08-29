package tonivade.poker

import cats.effect.IO
import cats.data.StateT
import cats.data.StateT._

object Main extends App {
  import Game._
  import GameHand._
  import Console._
  import BetTurn._
  import Deck._
  
  val players = List(Player("pepe"), Player("paco"), Player("toni"), Player("curro"), Player("perico"))
  
  def runBetLoop(hand: GameHand): StateT[IO, Deck, GameHand] = pure(betLoop.runS(BetTurn.from(hand)).unsafeRunSync().hand)
  
  def gameLoop(game: Game): StateT[IO, Deck, (Player, FullHand)] = 
    for {
      _ <- print(game)
      _ <- set[IO, Deck](shuffle)
      preFlop <- nextGameHand(game)
      flop <- phaseLoop(preFlop)
      turn <- phaseLoop(flop)
      river <- phaseLoop(turn)
      showdown <- phaseLoop(river)
      player <- winner(showdown)
    } yield player.get
    
  def phaseLoop(hand: GameHand): StateT[IO, Deck, GameHand] = 
    for {
      _ <- print(s"current pot ${hand.pot} in ${hand.phase}")
      _ <- print(hand)
      bets <- runBetLoop(hand)
      _ <- print(bets)
      nextHand <- nextPhase(bets)
    } yield nextHand
  
  val result = 
    for {
      game <- start(players)
      winner <- gameLoop(game)
    } yield winner
 
  val win = result.runA(ordered).unsafeRunSync()
  
  println(win)
}

object Console {
  import scala.io.StdIn.readLine
  
  def noop[S]: StateT[IO, S, Unit] = pure(())

  def print[S](value: Any): StateT[IO, S, Unit] = pure(println(value))
  
  def read[S]: StateT[IO, S, String] = pure(readLine())
}