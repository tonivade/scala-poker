package tonivade.poker

import cats.data.State

object Main extends App {
  import Game._
  import GameHand._
  import Console._
  
  val players = List(Player("pepe"), Player("paco"), Player("toni"), Player("curro"), Player("perico"))
  
  def gameLoop(game: Game): State[Deck, (Player, FullHand)] = 
    for {
      _ <- print(game)
      preFlop <- nextGameHand(game)
      _ <- print(preFlop)
      flop <- nextPhase(preFlop)
      _ <- print(flop)
      turn <- nextPhase(flop)
      _ <- print(turn)
      river <- nextPhase(turn)
      _ <- print(river)
      showdown <- nextPhase(river)
      _ <- print(showdown)
      winner <- winner(showdown)
      _ <- print(winner)
    } yield winner.get
  
  val result = 
    for {
      game <- start(players)
      _ <- gameLoop(game)
      next <- next(game)
    } yield next
 
  val end = result.run(Deck.shuffle).value

  println(end._2)
}

object Console {
  def print[S](value: Any): State[S, Unit] = State {
    state => (state, println(value))
  }
}