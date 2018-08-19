package tonivade.poker

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class MainSpec extends FlatSpec with Matchers {
  
  val game = Game(List(Player("toni"), Player("pepe"), Player("paco")))
  val deck = Deck.shuffle
  val nextGame = Game.nextGameHand(game).runA(deck).value

  "Strait" should "value be 4" in {
    Strait.value should be (4)
  }

  "Strait" should "be grater than Pair" in {
    Strait.compare(Pair) should be > 0
  }

  "Deck" should "have 52 cards" in {
    deck.cards.size should be (52)
  }

  "Deck" should "not have duplicated cards" in {
    deck.cards should be (deck.cards.distinct)
  }

  "Deck" should "have 51 cards after burn" in {
    val newDeck = deck.burn

    newDeck.cards.size should be (51)
  }

  "Taken Card" should "be the first card" in {
    val card = deck.take

    deck.cards.head should be (card)
  }
  
  "Game Dealer" should "be toni" in {
    game.dealer should be (Player("toni"))
  }
  
  "Game Small Blind" should "be pepe" in {
    game.smallBlind should be (Player("pepe"))
  }
  
  "Game Big Blind" should "be paco" in {
    game.bigBlind should be (Player("paco"))
  }
  
  "Game Hand" should "starts with an empty pot" in {
    nextGame.pot should be (Pot())
  }
  
  "Game Hand" should "start with a state PreFlop" in {
    nextGame.phase should be (PreFlop)
  }
}