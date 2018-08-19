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

sealed trait Role
case object Regular extends Role
case object Dealer extends Role
case object SmallBlind extends Role
case object BigBlind extends Role
case object Folded extends Role

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

  def take: State[Deck, Card] = State {
    deck => (deck.burn, deck.take)
  }
  
  def burn: State[Deck, Unit] = State {
    deck => (deck.burn, ())
  }

  def burnAndTake3: State[Deck, HandCards] =
    for {
      _ <- burn
      card1 <- take
      card2 <- take
      card3 <- take
    } yield HandCards(card1, card2, card3)
  
  def burnAndTake: State[Deck, Card] =
    for {
      _ <- burn
      card <- take 
    } yield card
}

case class Player(name: String, score: Integer = Player.DEFAULT_SCORE)

object Player {
  val DEFAULT_SCORE: Integer = 5000
}

sealed trait Action
case object Fold extends Action
case class Call(value: Integer) extends Action
case class Raise(value: Integer, raise: Integer) extends Action
case object AllIn extends Action

case class HandCards(card1: Card, card2: Card, card3: Card, card4: Option[Card] = None, card5: Option[Card] = None) {
  def setCard4(card: Card): HandCards = copy(card4 = Some(card))
  def setCard5(card: Card): HandCards = copy(card5 = Some(card))
}

case class PlayerHand(player: Player, role: Role, card1: Card, card2: Card, pot: Integer = 0) {
  def update(action: Action): PlayerHand = 
    action match {
      case Fold => copy(role = Folded)
      case Call(value) => copy(pot = pot + value)
      case Raise(value, raise) => copy(pot = pot + value + raise)
      case AllIn => copy(pot = player.score)
    }
}

case class GameHand(phase: HandPhase, players: List[PlayerHand], cards: Option[HandCards]) {
  def toPhase(phase: HandPhase): GameHand = copy(phase = phase)
  def setFlop(cards: HandCards): GameHand = copy(cards = Some(cards))
  def setTurn(card: Card): GameHand = copy(cards = cards.map(_.setCard4(card)))
  def setRiver(card: Card): GameHand = copy(cards = cards.map(_.setCard5(card)))
  
  def update(player: Player, action: Action): GameHand = {
    val newPlayers = players.map {
      playerHand => if (playerHand.player == player) playerHand.update(action) else playerHand
    }

    GameHand(phase, newPlayers, cards)
  }
  
  def pot: Integer = players.map(_.pot).reduce(_ + _)
}

object GameHand {
  def next(current: GameHand): State[Deck, GameHand] = 
    current.phase match {
      case PreFlop => toFlop(current)
      case Flop => toTurn(current)
      case Turn => toRiver(current)
      case River => toShowdown(current)
      case Showdown => pure(current)
    }
  
  def toFlop(current: GameHand): State[Deck, GameHand] = 
    for {
      cards <- Deck.burnAndTake3
    } yield current.toPhase(Flop).setFlop(cards)
  
  def toTurn(current: GameHand): State[Deck, GameHand] =
    for {
      card <- Deck.burnAndTake 
    } yield current.toPhase(Turn).setTurn(card)
  
  def toRiver(current: GameHand): State[Deck, GameHand] =
    for {
      card <- Deck.burnAndTake 
    } yield current.toPhase(River).setRiver(card)
    
  def toShowdown(current: GameHand): State[Deck, GameHand] =
    pure(current.toPhase(Showdown))
}

case class Game(players: List[Player], round: Integer = 1) {
  def dealer: Player = players.head
  def smallBlind: Player = players.tail.head
  def bigBlind: Player = players.tail.tail.head

  def next = {
    val newPlayers = players.filter(_.score > 0)
    Game(newPlayers.tail :+ newPlayers.head, round + 1)
  }
  
  def playerRole(player: Player) = 
    player match {
      case Player(name, _) if name == dealer.name => Dealer
      case Player(name, _) if name == smallBlind.name => SmallBlind
      case Player(name, _) if name == bigBlind.name => BigBlind
      case _ => Regular
    }
}

object Game {
  def start(players: List[Player]): State[Deck, Game] = pure(Game(players))
  
  def next(game: Game): State[Deck, Game] = pure(game.next)

  def nextGameHand(game: Game): State[Deck, GameHand] = 
    for {
      players <- playerList(game)
    } yield GameHand(PreFlop, players, None)

  private def playerList(game: Game): State[Deck, List[PlayerHand]] = 
    game.players.map(newPlayerHand(_, game))
      .foldLeft(pure[Deck, List[PlayerHand]](List()))((sa, sb) => map2(sa, sb)((acc, b) => acc :+ b))

  private def newPlayerHand(player: Player, game: Game): State[Deck, PlayerHand] = 
    for {
      role <- playerRole(player, game)
      card1 <- Deck.take
      card2 <- Deck.take
    } yield PlayerHand(player, role, card1, card2)
  
  private def playerRole(player: Player, game: Game): State[Deck, Role] = State {
    deck => (deck, game.playerRole(player))
  }
  
  private def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(map: (A, B) => C): State[S, C] = 
    sa.flatMap(a => sb.map(b => map(a, b)))
}

object BetTurn {
  
}

object Main extends App {
  import Game._
  
  def print[S](value: Any): State[S, Unit] = State {
    state => (state, println(value))
  }
  
  val players = List(Player("pepe"), Player("paco"), Player("toni"), Player("curro"), Player("perico"))
  
  val result = for {
    game <- start(players)
    _ <- print(game)
    preFlop <- nextGameHand(game)
    _ <- print(preFlop)
    flop <- GameHand.next(preFlop)
    _ <- print(flop)
    turn <- GameHand.next(flop)
    _ <- print(turn)
    river <- GameHand.next(turn)
    _ <- print(river)
    showdown <- GameHand.next(river)
    next <- next(game)
  } yield next
 
  val end = result.run(Deck.shuffle).value

  println(end._2)
}