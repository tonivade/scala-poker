package tonivade.poker

import cats.data.State
import cats.data.State._

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
  import Console._
  
  val DEFAULT_SCORE: Int = 5000
  
  def speak[S](player: Player): State[S, Action] = 
    for {
      _ <- print(s"$player turn")
      string <- read
    } yield toAction(string).getOrElse(Fold)
    
  def toAction(string: String): Option[Action] = 
    string match {
      case "fold" => Some(Fold)
      case "allin" => Some(AllIn)
      case "call" => Some(Call)
      case "raise" => Some(Raise(1))
      case _ => None
    }
}

sealed trait Action
case object Fold extends Action
case object Call extends Action
case class Raise(raise: Int) extends Action
case object AllIn extends Action

case class HandCards(card1: Card, card2: Card, card3: Card, card4: Option[Card] = None, card5: Option[Card] = None) {
  def setCard4(card: Card): HandCards = copy(card4 = Some(card))
  def setCard5(card: Card): HandCards = copy(card5 = Some(card))
  
  def combinations: List[List[Card]] = toList.combinations(3).toList
  
  private lazy val toList: List[Card] = List(card1, card2, card3) ++ card4.toList ++ card5.toList
}

case class PlayerHand(player: Player, role: Role, card1: Card, card2: Card, pot: Int = 0) {
  def bestHand(cards: HandCards): (Player, FullHand) = 
    (player, hands(cards).reduce((a, b) => if (a.bestHand > b.bestHand) a else b))
  
  def fold = copy(role = Folded)
  def allIn = copy(pot = player.score)
  def call(value: Int) = copy(pot = pot + value)
  def raise(value: Int, raise: Int) = copy(pot = pot + value + raise)

  private def hands(cards: HandCards): List[FullHand] = 
    for {
      combination <- cards.combinations
    } yield FullHand(card1, card2, combination(0), combination(1), combination(2))
}

case class GameHand(phase: HandPhase, players: List[PlayerHand], cards: Option[HandCards]) {
  def pot: Int = players.map(_.pot).reduce(_ + _)
  def bet: Int = players.map(_.pot).max
  def noMoreBets: Boolean = players.exists(_.pot < bet)
  
  val turn: Player = players.head.player
  def nextTurn: GameHand = {
    val newPlayers = players.filter(_.role != Folded)
    copy(players = newPlayers.tail :+ newPlayers.head)
  }

  def toPhase(phase: HandPhase): GameHand = copy(phase = phase)
  def setFlop(cards: HandCards): GameHand = copy(cards = Some(cards))
  def setTurn(card: Card): GameHand = copy(cards = cards.map(_.setCard4(card)))
  def setRiver(card: Card): GameHand = copy(cards = cards.map(_.setCard5(card)))
  
  def update(player: Player, action: Action): GameHand =
    action match {
      case Fold => fold(player)
      case AllIn => allIn(player)
      case Call => call(player)
      case Raise(value) => raise(player, value)
    }
  
  def winner: Option[(Player, FullHand)] = 
    cards.map(c => players.map(_.bestHand(c)).reduce((a, b) => if (a._2 > b._2) a else b))
  
  private def fold(player: Player): GameHand = update(player)(_.fold)
  private def allIn(player: Player): GameHand = update(player)(_.allIn)
  private def call(player: Player): GameHand = 
    diff(player).map(value => update(player)(_.call(value))).getOrElse(this)
  private def raise(player: Player, raise: Int): GameHand = 
    diff(player).map(value => update(player)(_.raise(value, raise))).getOrElse(this)
  
  private def update(player: Player)(action: PlayerHand => PlayerHand): GameHand = 
    copy(players = updatePlayer(player)(action))
  
  private def updatePlayer(player: Player)(action: PlayerHand => PlayerHand): List[PlayerHand] = 
    players.map {
      playerHand => if (playerHand.player == player) action(playerHand) else playerHand
    }
  
  private def diff(player: Player): Option[Int] =
    players.find(_.player == player).map(bet - _.pot).filter(_ >= 0)
}

object GameHand {
  def nextPhase(current: GameHand): State[Deck, GameHand] = 
    current.phase match {
      case PreFlop => toFlop(current)
      case Flop => toTurn(current)
      case Turn => toRiver(current)
      case River => toShowdown(current)
      case Showdown => pure(current)
    }
  
  def betOver: State[GameHand, Boolean] = inspect(_.noMoreBets)
  
  def turn: State[GameHand, Player] = inspect(_.turn)
  def nextTurn: State[GameHand, Unit] = modify(_.nextTurn)  
  
  def update(player: Player, action: Action): State[GameHand, Unit] = 
    modify(_.update(player, action))

  def winner(current: GameHand): State[Deck, Option[(Player, FullHand)]] =
    pure(current.winner)
  
  private def toFlop(current: GameHand): State[Deck, GameHand] = 
    for {
      cards <- Deck.burnAndTake3
    } yield current.toPhase(Flop).setFlop(cards)
  
  private def toTurn(current: GameHand): State[Deck, GameHand] =
    for {
      card <- Deck.burnAndTake 
    } yield current.toPhase(Turn).setTurn(card)
  
  private def toRiver(current: GameHand): State[Deck, GameHand] =
    for {
      card <- Deck.burnAndTake 
    } yield current.toPhase(River).setRiver(card)
    
  private def toShowdown(current: GameHand): State[Deck, GameHand] =
    pure(current.toPhase(Showdown))
}

case class Game(players: List[Player], round: Int = 1) {
  def dealer: Player = players.head
  def smallBlind: Player = players.tail.head
  def bigBlind: Player = players.tail.tail.head

  def next: Game = {
    val newPlayers = players.filter(_.score > 0)
    Game(newPlayers.tail :+ newPlayers.head, round + 1)
  }
  
  def playerRole(player: Player): Role = 
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
      role <- pure(game.playerRole(player))
      card1 <- Deck.take
      card2 <- Deck.take
    } yield PlayerHand(player, role, card1, card2)
  
  private def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(map: (A, B) => C): State[S, C] = 
    sa.flatMap(a => sb.map(b => map(a, b)))
}

object BetTurn {
  import GameHand._
  import Player._
  import Console._
  
  val betLoop: State[GameHand, Unit] = 
    for {
      _ <- nextTurn
      player <- turn
      action <- speak(player)
      _ <- update(player, action)
      _ <- print(s"$player has $action")
      end <- betOver
      _ <- if (end) exit[GameHand] else betLoop
    } yield ()
}