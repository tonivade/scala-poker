package tonivade.poker

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class HandSpec extends FlatSpec with Matchers {
  
  "Hand with all different cards" should "be a Highcard" in {
    val hand = FullHand(Card(Spades, Seven), Card(Clubs, Ace), Card(Hearts, Two), Card(Diamonds, Three), Card(Spades, Four))

    hand.hands should be (List(Highcard))
  }
  
  "Hand with two aces" should "be a Pair" in {
    val hand = FullHand(Card(Spades, Ace), Card(Clubs, Ace), Card(Hearts, Two), Card(Diamonds, Three), Card(Spades, Four))

    hand.hands should be (List(Pair, Highcard))
  }
  
  "Hand with two aces and two kings" should "be a Two Pairs" in {
    val hand = FullHand(Card(Spades, Ace), Card(Clubs, Ace), Card(Hearts, King), Card(Diamonds, King), Card(Spades, Four))

    hand.hands should be (List(TwoPairs, Highcard))
  }
  
  "Hand with three aces" should "be a Three of a Kind" in {
    val hand = FullHand(Card(Spades, Ace), Card(Clubs, Ace), Card(Hearts, Ace), Card(Diamonds, Three), Card(Spades, Four))

    hand.hands should be (List(ThreeOfAKind, Highcard))
  }
  
  "Hand with four aces" should "be a four of a Kind" in {
    val hand = FullHand(Card(Spades, Ace), Card(Clubs, Ace), Card(Hearts, Ace), Card(Diamonds, Ace), Card(Spades, Four))

    hand.hands should be (List(FourOfAKind, Highcard))
  }
  
  "Hand with consecutive cards" should "be a Strait" in {
    val hand = FullHand(Card(Spades, Ace), Card(Clubs, Two), Card(Hearts, Three), Card(Diamonds, Four), Card(Spades, Five))

    hand.hands should be (List(Strait, Highcard))
  }
  
  "Hand with tree aces and two kings" should "be a Full House" in {
    val hand = FullHand(Card(Spades, Ace), Card(Clubs, Ace), Card(Hearts, Ace), Card(Diamonds, King), Card(Spades, King))

    hand.hands should be (List(FullHouse, ThreeOfAKind, Pair, Highcard))
  }
  
  "Hand with cards of same suit" should "be a Flush" in {
    val hand = FullHand(Card(Spades, Ace), Card(Spades, Three), Card(Spades, Five), Card(Spades, Six), Card(Spades, Ten))

    hand.hands should be (List(Flush, Highcard))
  }
  
  "Hand with cards of same suit and consecutive" should "be a StraitFlush" in {
    val hand = FullHand(Card(Spades, Ace), Card(Spades, Two), Card(Spades, Three), Card(Spades, Four), Card(Spades, Five))

    hand.hands should be (List(StraitFlush, Flush, Strait, Highcard))
  }
  
  "Hand with cards of same suit and consecutive starting with 10" should "be a RoyalFlush" in {
    val hand = FullHand(Card(Spades, Ace), Card(Spades, Ten), Card(Spades, Jack), Card(Spades, Queen), Card(Spades, King))

    hand.hands should be (List(RoyalFlush, StraitFlush, Flush, Strait, Highcard))
  }
}