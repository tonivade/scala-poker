package tonivade.poker

import scala.util.Random
import cats.data.StateT
import cats.data.StateT._
import cats.effect.IO

sealed trait Suit
case object Clubs extends Suit
case object Spades extends Suit
case object Diamonds extends Suit
case object Hearts extends Suit
object Suit {
  val all = List(Clubs, Spades, Diamonds, Hearts)
}

sealed trait Figure extends Ordered[Figure] {
  val value: Int
  def compare(that: Figure): Int = this.value - that.value
}
case object Two extends Figure { override val value = 0 }
case object Three extends Figure { override val value = 1 }
case object Four extends Figure { override val value = 2 }
case object Five extends Figure { override val value = 3 }
case object Six extends Figure { override val value = 4 }
case object Seven extends Figure { override val value = 5 }
case object Eight extends Figure { override val value = 6 }
case object Nine extends Figure { override val value = 7 }
case object Ten extends Figure { override val value = 8 }
case object Jack extends Figure { override val value = 9 }
case object Queen extends Figure { override val value = 10 }
case object King extends Figure { override val value = 11 }
case object Ace extends Figure { override val value = 12 }
object Figure {
  val all = List(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)
}

case class Card(suit: Suit, figure: Figure)

object Card {
  val all: Seq[Card] = for {
    suit <- Suit.all
    figure <- Figure.all
  } yield Card(suit, figure)
}

case class Deck(cards: Seq[Card]) {
  def take: Card = cards.head
  def burn: Deck = Deck(cards.tail)
}

object Deck {
  def ordered: Deck = Deck(Card.all)
  def shuffle: Deck = Deck(Random.shuffle(Card.all))
  
  def burnAndTake: StateT[IO, Deck, Card] =
    for {
      _ <- burn
      card <- take 
    } yield card

  def burnAndTake3: StateT[IO, Deck, HandCards] =
    for {
      _ <- burn
      card1 <- take
      card2 <- take
      card3 <- take
    } yield HandCards(card1, card2, card3)

  def take: StateT[IO, Deck, Card] = StateT {
    deck => IO(deck.burn, deck.take)
  }
  
  private def burn: StateT[IO, Deck, Unit] = modify(_.burn)
}