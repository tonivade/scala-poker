package tonivade.poker

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class PokerSpec extends FlatSpec with Matchers {
  import Game._
  import GameHand._
  
  val toni = Player("toni")
  val pepe = Player("pepe")
  val paco = Player("paco")

  val game = Game(List(toni, pepe, paco))
  val deck = Deck.shuffle

  val preFlop = nextGameHand(game)
  val flop = for {
    preFlop <- nextGameHand(game)
    flop <- nextPhase(preFlop)
  } yield flop
  val turn = for {
    preFlop <- nextGameHand(game)
    flop <- nextPhase(preFlop)
    turn <- nextPhase(flop)
  } yield turn
  val river = for {
    preFlop <- nextGameHand(game)
    flop <- nextPhase(preFlop)
    turn <- nextPhase(flop)
    river <- nextPhase(turn)
  } yield river
  val showdown = for {
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
    val hand = preFlop.runA(deck).value
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
    val hand = preFlop.runA(deck).value

    hand.phase should be (PreFlop)
  }
  
  "Game Hand" should "Flop follow PreFlop" in {
    val hand = flop.runA(deck).value

    hand.phase should be (Flop)
  }
  
  "Game Hand" should "Turn follow Flop" in {
    val hand = turn.runA(deck).value

    hand.phase should be (Turn)
  }
  
  "Game Hand" should "River follow Turn" in {
    val hand = river.runA(deck).value

    hand.phase should be (River)
  }
  
  "Game Hand" should "Showdown follow River" in {
    val hand = showdown.runA(deck).value

    hand.phase should be (Showdown)
  }
  
  "Game Hand" should "be updated with two raises two calls to 6" in {
    val hand = preFlop.runA(deck).value

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
    val hand = GameHand(Flop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand)
    
    bets.nextTurn.turn should be (paco)
  }
  
  "Bet turn nextTurn" should "be next user" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(Flop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand)
    
    bets.nextTurn.nextTurn.turn should be (pepe)
  }
  
  "Bet turn nextTurn nextTurn" should "be next user" in {
    val toniHand = PlayerHand(toni, Dealer, Card(Clubs, Ace), Card(Spades, Ace))
    val pacoHand = PlayerHand(paco, SmallBlind, Card(Clubs, Four), Card(Spades, Five))
    val pepeHand = PlayerHand(pepe, BigBlind, Card(Hearts, Ace), Card(Diamonds, King))
    val hand = GameHand(Flop, List(toniHand, pacoHand, pepeHand), None)
    val bets = BetTurn.from(hand)
    
    bets.nextTurn.nextTurn.nextTurn.turn should be (toni)
  }
}