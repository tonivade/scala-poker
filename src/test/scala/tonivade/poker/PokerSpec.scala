package tonivade.poker

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class MainSpec extends FlatSpec with Matchers {
  
  val toni = Player("toni")
  val pepe = Player("pepe")
  val paco = Player("paco")

  val game = Game(List(toni, pepe, paco))
  val deck = Deck.shuffle

  val preFlop = Game.nextGameHand(game)
  val flop = for {
    preFlop <- Game.nextGameHand(game)
    flop <- GameHand.next(preFlop)
  } yield flop
  val turn = for {
    preFlop <- Game.nextGameHand(game)
    flop <- GameHand.next(preFlop)
    turn <- GameHand.next(flop)
  } yield turn
  val river = for {
    preFlop <- Game.nextGameHand(game)
    flop <- GameHand.next(preFlop)
    turn <- GameHand.next(flop)
    river <- GameHand.next(turn)
  } yield river
  val showdown = for {
    preFlop <- Game.nextGameHand(game)
    flop <- GameHand.next(preFlop)
    turn <- GameHand.next(flop)
    river <- GameHand.next(turn)
    showdown <- GameHand.next(river)
  } yield showdown

  "Strait" should "value be 4" in {
    Strait.value should be (4)
  }

  "Strait" should "be grater than Pair" in {
    Strait.compare(Pair) should be > 0
  }
  
  "Game Dealer" should "be toni" in {
    game.dealer should be (toni)
  }
  
  "Game Small Blind" should "be pepe" in {
    game.smallBlind should be (pepe)
  }
  
  "Game Big Blind" should "be paco" in {
    game.bigBlind should be (paco)
  }
  
  "Game Hand" should "starts with an empty bet" in {
    val hand = preFlop.runA(deck).value
    hand.pot should be (0)
  }
  
  "Game Hand" should "start with a state PreFlop" in {
    val hand = preFlop.runA(deck).value
    hand.phase should be (PreFlop)
  }
  
  "Game Hand" should "Flop follow PreFlop" in {
    val hand = flop.runA(deck).value
    hand.phase should be (Flop)
  }
  
  "Game Hand" should "Turn follow Flop" in {
    val hand = turn.runA(deck).value
    hand.phase should be (Turn)
  }
  
  "Game Hand" should "River follow Turn" in {
    val hand = river.runA(deck).value
    hand.phase should be (River)
  }
  
  "Game Hand" should "Showdown follow River" in {
    val hand = showdown.runA(deck).value
    hand.phase should be (Showdown)
  }
  
  "Game Hand" should "be updated with two raises two calls to 6" in {
    val hand = preFlop.runA(deck).value
    val newHand = hand
        .update(pepe, Raise(0, 1))
        .update(paco, Raise(1, 1))
        .update(toni, Call(2))
        .update(pepe, Call(1))

    newHand.pot should be (6)
    newHand.bid should be (2)
  }
}