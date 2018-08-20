package tonivade.poker

import cats.data.State

object Main extends App {
  import Game._
  
  def print[S](value: Any): State[S, Unit] = State {
    state => (state, println(value))
  }
  
  val players = List(Player("pepe"), Player("paco"), Player("toni"), Player("curro"), Player("perico"))
  
  val result = for {
    game <- start(players)
    _ <- print(game)
    preFlop <- nextGameHand(game)
    _ <- print(preFlop)
    flop <- GameHand.next(preFlop)
    _ <- print(flop)
    turn <- GameHand.next(flop)
    _ <- print(turn)
    river <- GameHand.next(turn)
    _ <- print(river)
    showdown <- GameHand.next(river)
    next <- next(game)
  } yield next
 
  val end = result.run(Deck.shuffle).value

  println(end._2)
}