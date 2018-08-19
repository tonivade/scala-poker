package tonivade.poker

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class MainSpec extends FlatSpec with Matchers {

  "Strait" should "value be 4" in {
    Strait.value should be (4)
  }

  "Strait" should "be grater than Pair" in {
    Strait.compare(Pair) should be > 0
  }

  "Deck" should "have 52 cards" in {
    val deck = Deck.shuffle

    deck.cards.size should be (52)
  }

  "Deck" should "not have duplicated cards" in {
    val deck = Deck.shuffle

    deck.cards should be (deck.cards.distinct)
  }

  "Deck" should "have 51 cards after take" in {
    val (newDeck, card) = Deck.shuffle.take

    newDeck.cards.size should be (51)
  }
}