package tonivade.poker

import cats.data.StateT
import cats.effect.IO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PokerSpec extends AnyFlatSpec with Matchers {
  import GameHand._
  
  val toni: Player = Player("toni")
  val pepe: Player = Player("pepe")
  val paco: Player = Player("paco")

  val game: Game = Game(List(toni, pepe, paco))
  val deck: Deck = Deck.shuffle

  val preFlop: StateT[IO, Deck, GameHand] = nextGameHand(game)
  val flop: StateT[IO, Deck, GameHand] = for {
    preFlop <- nextGameHand(game)
    flop <- nextPhase(preFlop)
  } yield flop
  val turn: StateT[IO, Deck, GameHand] = for {
    preFlop <- nextGameHand(game)
    flop <- nextPhase(preFlop)
    turn <- nextPhase(flop)
  } yield turn
  val river: StateT[IO, Deck, GameHand] = for {
    preFlop <- nextGameHand(game)
    flop <- nextPhase(preFlop)
    turn <- nextPhase(flop)
    river <- nextPhase(turn)
  } yield river
  val showdown: StateT[IO, Deck, GameHand] = for {
    preFlop <- nextGameHand(game)
    flop <- nextPhase(preFlop)
    turn <- nextPhase(flop)
    river <- nextPhase(turn)
    showdown <- nextPhase(river)
  } yield showdown

  "Strait" should "value be 4" in {
    Strait.value should be (4)
  }

  "Strait" should "be grater than Pair" in {
    Strait.compare(Pair) should be > 0
  }
  
  "Game Dealer" should "be toni" in {
    game.dealer should be (toni)
  }
  
  "Game Small Blind" should "be pepe" in {
    game.smallBlind should be (pepe)
  }
  
  "Game Big Blind" should "be paco" in {
    game.bigBlind should be (paco)
  }
  
  "Game Hand" should "starts with an empty bet" in {
    val hand = preFlop.runA(deck).unsafeRunSync()
    hand.pot should be (0)
  }
  
  "Hand Cards with 3 card" should "have one combination" in {
    val cards = HandCards(Card(Clubs, Two), Card(Spades, Three), Card(Hearts, Queen))

    cards.combinations.size should be (1)
  }
  
  "Hand Cards with 4 cards" should "have 4 combinations" in {
    val cards = HandCards(Card(Clubs, Two), Card(Spades, Three), Card(Hearts, Queen), Some(Card(Diamonds, Jack)))
    
    cards.combinations.size should be (4)
  }
  
  "Hand Cards with 5 cards" should "have 6 combinations" in {
    val cards = HandCards(Card(Clubs, Two), Card(Spades, Three), Card(Hearts, Queen), Some(Card(Diamonds, Jack)), Some(Card(Hearts, Four)))
    
    cards.combinations.size should be (10)
  }
  
  "GameHand in preFlop" should "no have winner" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(Flop, List(toniHand, pacoHand, pepeHand), None)
    
    hand.winner should be (None)
  }
  
  "GameHand with tree aces" should "win to pair" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val cards = HandCards(Card(Diamonds, Ace), Card(Hearts, Three), Card(Spades, Jack))
    val hand = GameHand(Flop, List(toniHand, pacoHand, pepeHand), Some(cards))
    
    val (winner, winnerHand) = hand.winner.get

    winnerHand.bestHand should be (ThreeOfAKind)
    winner should be (toni)
  }
  
  "GameHand with flull king aces" should "win to three of a kind" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val cards = HandCards(Card(Diamonds, Ace), Card(Spades, King), Card(Spades, Jack), Some(Card(Hearts, King)))
    val hand = GameHand(Flop, List(toniHand, pacoHand, pepeHand), Some(cards))
    
    val (winner, winnerHand) = hand.winner.get

    winnerHand.bestHand should be (FullHouse)
    winner should be (pepe)
  }
  
  "Game Hand" should "start with a state PreFlop" in {
    val hand = preFlop.runA(deck).unsafeRunSync()

    hand.phase should be (PreFlop)
  }
  
  "Game Hand" should "Flop follow PreFlop" in {
    val hand = flop.runA(deck).unsafeRunSync()

    hand.phase should be (Flop)
  }
  
  "Game Hand" should "Turn follow Flop" in {
    val hand = turn.runA(deck).unsafeRunSync()

    hand.phase should be (Turn)
  }
  
  "Game Hand" should "River follow Turn" in {
    val hand = river.runA(deck).unsafeRunSync()

    hand.phase should be (River)
  }
  
  "Game Hand" should "Showdown follow River" in {
    val hand = showdown.runA(deck).unsafeRunSync()

    hand.phase should be (Showdown)
  }
  
  "Game Hand" should "be updated with two raises two calls to 6" in {
    val hand = preFlop.runA(deck).unsafeRunSync()

    val newHand = hand
        .update(pepe, Raise(1))
        .update(paco, Raise(1))
        .update(toni, Call)
        .update(pepe, Call)
        
    newHand.pot should be (6)
    newHand.maxBet should be (2)
  }
  
  "Bet turn" should "be next after dealer" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand)
    
    bets.nextTurn.turn should be (paco)
  }
  
  "Bet turn nextTurn" should "be next user" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand)
    
    bets.nextTurn.nextTurn.turn should be (pepe)
  }
  
  "Bet turn nextTurn nextTurn" should "be next user" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand)
    
    bets.nextTurn.nextTurn.nextTurn.turn should be (toni)
  }
  
  "Bet turn in preflop for big blind user" should "only can raise" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand)
    
    bets.options(pepe) should be (List(Raise(1)))
  }
  
  "Bet turn in preflop for big blind user" should "do any action after first round" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand).copy(bets = List(Raise(1), Raise(1), Call))
    
    bets.options(paco) should be (List(Fold, Check, Raise(1)))
  }
  
  "Bet turn in preflop for small blind user" should "only can raise" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand)
    
    bets.options(paco) should be (List(Raise(1)))
  }
  
  "Bet turn in preflop for small blind user" should "do any action after first round" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand).copy(bets = List(Raise(1), Raise(1), Call))
    
    bets.options(paco) should be (List(Fold, Check, Raise(1)))
  }
  
  "Bet turn in flop for small blind user" should "do any action after first round" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(Flop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand).copy(bets = List(Raise(1), Raise(1), Call))
    
    bets.options(paco) should be (List(Fold, Check, Raise(1)))
  }
  
  "Bet turn in preflop for dealer user" should "do any action" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand)
    
    bets.options(toni) should be (List(Fold, Check, Raise(1)))
  }
  
  "Bet turn in preflop for dealer user with raise" should "do any action but call instead of check" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(PreFlop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand).update(pepe, Raise(1)).update(paco, Raise(1))
    
    bets.options(toni) should be (List(Fold, Call, Raise(1)))
  }
}