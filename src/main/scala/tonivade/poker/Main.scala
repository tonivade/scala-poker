package tonivade.poker

import scala.util.Random
import cats.data.State

sealed trait Suit
case object Clubs extends Suit
case object Spades extends Suit
case object Diamonds extends Suit
case object Hearts extends Suit

sealed trait Figure extends Ordered[Figure] {
  def value: Integer
  def compare(that: Figure) = this.value - that.value
}
case object Two extends Figure { val value = 1 }
case object Three extends Figure { val value = 2 }
case object Four extends Figure { val value = 3 }
case object Five extends Figure { val value = 4 }
case object Six extends Figure { val value = 5 }
case object Seven extends Figure { val value = 6 }
case object Eight extends Figure { val value = 7 }
case object Nine extends Figure { val value = 8 }
case object Ten extends Figure { val value = 9 }
case object Jack extends Figure { val value = 10 }
case object Queen extends Figure { val value = 11 }
case object King extends Figure { val value = 12 }
case object Ace extends Figure { val value = 13 }

sealed trait Hand extends Ordered[Hand] { 
  def value: Integer 
  def compare(that: Hand) = this.value - that.value
}
case object Highcard extends Hand { val value = 0 }
case object Pair extends Hand { val value = 1 }
case object TwoPairs extends Hand { val value = 2 }
case object ThreeOfAKind extends Hand { val value = 3 }
case object Strait extends Hand { val value = 4 }
case object Flush extends Hand { val value = 5 }
case object FullHouse extends Hand { val value = 6 }
case object FourOfAKind extends Hand { val value = 7 }
case object StraitFlush extends Hand { val value = 8 }
case object RoyalFlush extends Hand { val value = 9 }

sealed trait HandPhase
case object PreFlop extends HandPhase
case object Flop extends HandPhase
case object Turn extends HandPhase
case object River extends HandPhase
case object Showdown extends HandPhase

sealed trait Role
case object Regular extends Role
case object Dealer extends Role
case object SmallBlind extends Role
case object BigBlind extends Role

case class Card(suit: Suit, figure: Figure)

case class Deck(cards: List[Card]) {
  def take = (Deck(cards.tail), cards.head)
}

object Deck {
  val cards = for {
    suit <- List(Clubs, Diamonds, Hearts, Spades)
    figure <- List(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)
  } yield Card(suit, figure)

  def ordered = Deck(cards)
  def shuffle = Deck(Random.shuffle(cards))

  def take: State[Deck, Card] = State {
    deck => deck.take
  }
}

case class Player(name: String, score: Integer = Player.DEFAULT_SCORE) {
  def update(value: Integer) = Player(name, score + value)
}

object Player {
  val DEFAULT_SCORE = 5000;
}

case class HandCards(card1: Card, card2: Card, card3: Card, card4: Option[Card] = None, card5: Option[Card] = None)
case class PlayerHand(player: Player, role: Role, card1: Card, card2: Card)
case class GameHand(phase: HandPhase, players: List[PlayerHand], cards: HandCards, pot: Integer)

case class Game(players: List[Player], round: Integer = 0) {
  def next = Game(players.filter(_.score > 0), round + 1)
}

object Game {
  def start(players: List[Player]) = Game(players)

  def initialCards: State[Deck, HandCards] =
    for {
      card1 <- Deck.take
      card2 <- Deck.take
      card3 <- Deck.take
    } yield HandCards(card1, card2, card3)

  def nextGameHand: State[Deck, GameHand] = 
    for {
      cards <- initialCards
    } yield GameHand(PreFlop, List(), cards, 0)

  def newPlayerHand(player: Player): State[Deck, PlayerHand] = 
    for {
      card1 <- Deck.take
      card2 <- Deck.take
    } yield PlayerHand(player, Regular, card1, card2)
}

object Main extends App {
  val players = List(Player("curro"), Player("pepe"), Player("paco"))
  val game = Game.start(players).next
}