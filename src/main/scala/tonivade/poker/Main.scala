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
object Figure {
  val all = List(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)
}

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
case object End extends HandPhase

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
  val all = for {
    suit <- Suit.all
    figure <- Figure.all
  } yield Card(suit, figure)

  def ordered = Deck(all)
  def shuffle = Deck(Random.shuffle(all))

  def take: State[Deck, Card] = State {
    deck => deck.take
  }
  
  def flopCards: State[Deck, HandCards] =
    for {
      card1 <- Deck.take
      card2 <- Deck.take
      card3 <- Deck.take
    } yield HandCards(card1, card2, card3)
}

case class Player(name: String, score: Integer = Player.DEFAULT_SCORE)

object Player {
  val DEFAULT_SCORE: Integer = 5000
}

case class HandCards(card1: Card, card2: Card, card3: Card, card4: Option[Card] = None, card5: Option[Card] = None) {
  def setCard4(card: Card): HandCards = copy(card4 = Some(card))
  def setCard5(card: Card): HandCards = copy(card5 = Some(card))
}

case class PlayerHand(player: Player, role: Role, card1: Card, card2: Card)
case class GameHand(phase: HandPhase, players: List[PlayerHand], cards: Option[HandCards], pot: Integer)

object GameHand {
  def next(current: GameHand): State[Deck, GameHand] = 
    current.phase match {
       case PreFlop => for {
           cards <- Deck.flopCards
         } yield current.copy(phase = Flop, cards = Some(cards))
       case Flop => for {
           card <- Deck.take 
         } yield current.copy(phase = Turn, cards = current.cards.map(_.copy(card4 = Some(card))))
       case Turn => for {
           card <- Deck.take 
         } yield current.copy(phase = River, cards = current.cards.map(_.copy(card5 = Some(card))))
       case River => pure(current.copy(phase = Showdown))
       case Showdown => pure(current.copy(phase = End))
       case End => pure(current)
    }
}

case class Game(players: List[Player], round: Integer = 0) {
  def next = Game(players.filter(_.score > 0), round + 1)
}

object Game {
  def start(players: List[Player]): State[Deck, Game] = pure(Game(players))

  def nextGameHand(game: Game): State[Deck, GameHand] = 
    for {
      players <- playerList(game.players)
    } yield GameHand(PreFlop, players, None, 0)

  private def playerList(players: List[Player]) : State[Deck, List[PlayerHand]] = 
    players.map(newPlayerHand(_, Regular))
      .foldLeft(pure[Deck, List[PlayerHand]](List()))((sa, sb) => map2(sa, sb)((acc, b) => acc :+ b))

  private def newPlayerHand(player: Player, role: Role): State[Deck, PlayerHand] = 
    for {
      card1 <- Deck.take
      card2 <- Deck.take
    } yield PlayerHand(player, role, card1, card2)
  
  private def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(map: (A, B) => C) : State[S, C] = 
    sa.flatMap(a => sb.map(b => map(a, b)))
}

object Main extends App {
  import Game._
  
  val players = List(Player("pepe"), Player("paco"), Player("toni"), Player("curro"), Player("perico"))
  
  val result = for {
    game <- start(players)
    preFlop <- nextGameHand(game)
    flop <- GameHand.next(preFlop)
    turn <- GameHand.next(flop)
    river <- GameHand.next(turn)
  } yield river
 
  val end = result.run(Deck.shuffle).value

  println(52 - end._1.cards.size)
  println(end._2)
}