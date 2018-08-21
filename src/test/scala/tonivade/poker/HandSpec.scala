package tonivade.poker

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class HandSpec extends FlatSpec with Matchers {
  
  "Hand with two aces" should "be a Pair" in {
    val pair = FullHand(Card(Spades, Ace), Card(Clubs, Ace), Card(Hearts, Two), Card(Diamonds, Three), Card(Spades, Four))

    Hand.all.filter(_.eval(pair)) should be (List(Pair, Highcard))
  }
  
  "Hand with three aces" should "be a Three of a Kind" in {
    val pair = FullHand(Card(Spades, Ace), Card(Clubs, Ace), Card(Hearts, Ace), Card(Diamonds, Three), Card(Spades, Four))

    Hand.all.filter(_.eval(pair)) should be (List(ThreeOfAKind, Highcard))
  }
  
  "Hand with four aces" should "be a four of a Kind" in {
    val pair = FullHand(Card(Spades, Ace), Card(Clubs, Ace), Card(Hearts, Ace), Card(Diamonds, Ace), Card(Spades, Four))

    Hand.all.filter(_.eval(pair)) should be (List(FourOfAKind, Highcard))
  }
  
  "Hand with consecutive cards" should "be a Strait" in {
    val pair = FullHand(Card(Spades, Ace), Card(Clubs, Two), Card(Hearts, Three), Card(Diamonds, Four), Card(Spades, Five))

    Hand.all.filter(_.eval(pair)) should be (List(Strait, Highcard))
  }
  
  "Hand with tree aces and two kings" should "be a Full House" in {
    val pair = FullHand(Card(Spades, Ace), Card(Clubs, Ace), Card(Hearts, Ace), Card(Diamonds, King), Card(Spades, King))

    Hand.all.filter(_.eval(pair)) should be (List(FullHouse, ThreeOfAKind, Pair, Highcard))
  }
  
  "Hand with cards of same suit" should "be a Flush" in {
    val pair = FullHand(Card(Spades, Ace), Card(Spades, Three), Card(Spades, Five), Card(Spades, Six), Card(Spades, Ten))

    Hand.all.filter(_.eval(pair)) should be (List(Flush, Highcard))
  }
  
  "Hand with cards of same suit and consecutive" should "be a StraitFlush" in {
    val pair = FullHand(Card(Spades, Ace), Card(Spades, Two), Card(Spades, Three), Card(Spades, Four), Card(Spades, Five))

    Hand.all.filter(_.eval(pair)) should be (List(StraitFlush, Flush, Strait, Highcard))
  }
  
  "Hand with cards of same suit and consecutive starting with 10" should "be a RoyalFlush" in {
    val pair = FullHand(Card(Spades, Ace), Card(Spades, Ten), Card(Spades, Jack), Card(Spades, Queen), Card(Spades, King))

    Hand.all.filter(_.eval(pair)) should be (List(RoyalFlush, StraitFlush, Flush, Strait, Highcard))
  }
}