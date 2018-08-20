package tonivade.poker

import cats.data.State
import cats.data.State._

sealed trait Hand extends Ordered[Hand] { 
  def value: Int 
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

case class Player(name: String, score: Int = Player.DEFAULT_SCORE)

object Player {
  val DEFAULT_SCORE: Int = 5000
}

sealed trait Action
case object Fold extends Action
case class Call(value: Int) extends Action
case class Raise(value: Int, raise: Int) extends Action
case object AllIn extends Action

case class HandCards(card1: Card, card2: Card, card3: Card, card4: Option[Card] = None, card5: Option[Card] = None) {
  def setCard4(card: Card): HandCards = copy(card4 = Some(card))
  def setCard5(card: Card): HandCards = copy(card5 = Some(card))
}

case class PlayerHand(player: Player, role: Role, card1: Card, card2: Card, pot: Int = 0) {
  def update(action: Action): PlayerHand = 
    action match {
      case Fold => copy(role = Folded)
      case AllIn => copy(pot = player.score)
      case Call(value) => copy(pot = pot + value)
      case Raise(value, raise) => copy(pot = pot + value + raise)
    }
}

case class GameHand(phase: HandPhase, players: List[PlayerHand], cards: Option[HandCards]) {
  def pot: Int = players.map(_.pot).reduce(_ + _)
  def bid: Int = players.map(_.pot).max

  def toPhase(phase: HandPhase): GameHand = copy(phase = phase)
  def setFlop(cards: HandCards): GameHand = copy(cards = Some(cards))
  def setTurn(card: Card): GameHand = copy(cards = cards.map(_.setCard4(card)))
  def setRiver(card: Card): GameHand = copy(cards = cards.map(_.setCard5(card)))
  
  def fold(player: Player): GameHand = update(player, Fold)
  def allIn(player: Player): GameHand = update(player, AllIn)
  def call(player: Player): GameHand = 
    diff(player).map(Call(_)).map(update(player, _)).getOrElse(this)
  def raise(player: Player, value: Int): GameHand = 
    diff(player).map(Raise(_, value)).map(update(player, _)).getOrElse(this)
  
  private def update(player: Player, action: Action): GameHand = {
    val newPlayers = players.map {
      playerHand => if (playerHand.player == player) playerHand.update(action) else playerHand
    }

    GameHand(phase, newPlayers, cards)
  }
  
  private def diff(player: Player): Option[Int] =
    players.find(_.player == player).map(bid - _.pot).filter(_ >= 0)
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

case class Game(players: List[Player], round: Int = 1) {
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