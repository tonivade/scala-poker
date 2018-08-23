package tonivade.poker

import scala.util.Random
import cats.data.State
import cats.data.State._

sealed trait Suit
case object Clubs extends Suit
case object Spades extends Suit
case object Diamonds extends Suit
case object Hearts extends Suit
object Suit {
  val all = List(Clubs, Spades, Diamonds, Hearts)
}

sealed trait Figure extends Ordered[Figure] {
  def value: Int
  def compare(that: Figure) = this.value - that.value
}
case object Two extends Figure { val value = 0 }
case object Three extends Figure { val value = 1 }
case object Four extends Figure { val value = 2 }
case object Five extends Figure { val value = 3 }
case object Six extends Figure { val value = 4 }
case object Seven extends Figure { val value = 5 }
case object Eight extends Figure { val value = 6 }
case object Nine extends Figure { val value = 7 }
case object Ten extends Figure { val value = 8 }
case object Jack extends Figure { val value = 9 }
case object Queen extends Figure { val value = 10 }
case object King extends Figure { val value = 11 }
case object Ace extends Figure { val value = 12 }
object Figure {
  val all = List(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)
}

case class Card(suit: Suit, figure: Figure)

object Card {
  val all = for {
    suit <- Suit.all
    figure <- Figure.all
  } yield Card(suit, figure)
}

case class Deck(cards: List[Card]) {
  def take = cards.head
  def burn = Deck(cards.tail)
}

object Deck {
  def ordered = Deck(Card.all)
  def shuffle = Deck(Random.shuffle(Card.all))
  
  def burnAndTake: State[Deck, Card] =
    for {
      _ <- burn
      card <- take 
    } yield card

  def burnAndTake3: State[Deck, HandCards] =
    for {
      _ <- burn
      card1 <- take
      card2 <- take
      card3 <- take
    } yield HandCards(card1, card2, card3)

  def take: State[Deck, Card] = inspect(_.take)
  
  private def burn: State[Deck, Unit] = modify(_.burn)
}