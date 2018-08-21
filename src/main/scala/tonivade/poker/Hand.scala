package tonivade.poker

sealed trait Hand extends Ordered[Hand] { 
  def value: Int
  def eval(hand: FullHand): Boolean
  def compare(that: Hand) = this.value - that.value
}

case object Highcard extends Hand { 
  val value = 0 
  def eval(hand: FullHand): Boolean = true
}

case object Pair extends Hand { 
  val value = 1 
  def eval(hand: FullHand): Boolean = hand.count.exists(_._2 == 2)
}

case object TwoPairs extends Hand { 
  val value = 2 
  def eval(hand: FullHand): Boolean = hand.count.filter(_._2 == 2).size == 2
}

case object ThreeOfAKind extends Hand {
  val value = 3 
  def eval(hand: FullHand): Boolean = hand.count.exists(_._2 == 3)
}

case object Strait extends Hand { 
  val value = 4 
  def eval(hand: FullHand): Boolean = hand.conseq == 4
}

case object Flush extends Hand { 
  val value = 5 
  def eval(hand: FullHand): Boolean = hand.sameSuit
}

case object FullHouse extends Hand { 
  val value = 6 
  def eval(hand: FullHand): Boolean = 
    hand.count.exists(_._2 == 3) && hand.count.exists(_._2 == 2)
}

case object FourOfAKind extends Hand { 
  val value = 7 
  def eval(hand: FullHand): Boolean = hand.count.exists(_._2 == 4)
}

case object StraitFlush extends Hand { 
  val value = 8 
  def eval(hand: FullHand): Boolean = hand.sameSuit && hand.conseq == 4
}

case object RoyalFlush extends Hand { 
  val value = 9 
  def eval(hand: FullHand): Boolean = hand.sameSuit && 
    hand.contains(Ace) &&
    hand.contains(King) &&
    hand.contains(Queen) &&
    hand.contains(Jack) &&
    hand.contains(Ten)
}

object Hand {
  val all = List(RoyalFlush, StraitFlush, FourOfAKind, FullHouse, Flush, Strait, ThreeOfAKind, TwoPairs, Pair, Highcard)
}

case class FullHand(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) {
  lazy val toList: List[Card] = card1 :: card2 :: card3 :: card4 :: card5 :: Nil

  def sameSuit: Boolean = toList.map(_.suit).distinct.size == 1
  def count: Map[Figure, Int] = toList.groupBy(_.figure).mapValues(_.size)
  def contains(figure: Figure): Boolean = toList.map(_.figure).contains(figure)
  def conseq: Int = 
    toList.map(_.figure).map(_.value).sorted.sliding(2).toArray.count(a => (a(0) + 1) % 13 == a(1))
}