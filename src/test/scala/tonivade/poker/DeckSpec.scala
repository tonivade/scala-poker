package tonivade.poker

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class DeckSpec extends FlatSpec with Matchers {

  val deck = Deck.shuffle

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
  
  "Taken Two Cards" should "be different cards" in {
    val take2 = for {
      card1 <- Deck.take
      card2 <- Deck.take
    } yield (card1, card2)
    
    val (c1, c2) = take2.runA(Deck.shuffle).value

    c1 == c2 should be (false)
  }
  
}