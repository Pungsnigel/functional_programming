{-
	size hand2
	= size (Add (Card (Numeric 2) Hearts)
	(Add (Card Jack Spades) Empty))
	= 1 + size (Add (Card Jack Spades) Empty))
	= 1 + 1 + size Empty
	=2
-}

module BlackJack where
import Cards
import Wrapper
import System.Random
import Test.QuickCheck

-- Return an empty hand
empty :: Hand
empty = Empty

-- Calculate and return the total value of a hand
value :: Hand -> Integer
value hand 
    | maxHand hand > 21 = maxHand hand - 10 * numberOfAces hand
    | otherwise         = maxHand hand	
   where maxHand Empty = 0
         maxHand (Add card hand) = valueCard card + maxHand hand 

-- Given a rank, return its value
valueRank :: Rank -> Integer
valueRank Ace 		  = 11
valueRank (Numeric n) = n
valueRank _ 		  = 10

-- Given a card, return its value
valueCard :: Card -> Integer
valueCard c = valueRank (rank c)

-- Given a hand of cards, return the total number of aces in the hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand) 
		| rank card == Ace = 1 + numberOfAces hand
		| otherwise 	   = numberOfAces hand

-- Return true if the hand has reached a value higher than 21, else false.
gameOver :: Hand -> Bool
gameOver h = value h > 21

-- Given two hands, calculate the winner.
winner :: Hand -> Hand -> Player
winner guest bank 
		| gameOver guest           = Bank 
		| gameOver bank            = Guest
		| value guest > value bank = Guest
		| otherwise                = Bank

-- Operator for putting two hands together
(<+) :: Hand -> Hand -> Hand
(<+) Empty hand2           = hand2
(<+) (Add card hand) hand2 = (Add card Empty) <+ (hand <+ hand2)

-- Test that <+ is associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

-- Test that <+ doesn't change the size
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size (p1 <+ p2) == (size p1 + size p2)

-- Generate and return a full deck of cards
fullDeck :: Hand
fullDeck = fullSuit Hearts <+ fullSuit Spades <+ fullSuit Diamonds <+ fullSuit Clubs

-- Generate and return a full set of cards from a given suit
fullSuit :: Suit -> Hand
fullSuit suit = Add (Card Ace suit) (Add (Card King suit) 
                (Add (Card Queen suit) (Add (Card Jack suit) (fullNumeric suit))))

-- Generate and return all numerics from a given suit
fullNumeric :: Suit -> Hand
fullNumeric suit = generateNumerics suit 10 Empty
    where generateNumerics suit 2 hand = (Add (Card (Numeric 2) suit) hand)
          generateNumerics suit i hand = (Add (Card (Numeric i) suit) (generateNumerics suit (i-1) hand))

-- Given a deck and a hand, draw a card from the deck and add it to the hand
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _              = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, (Add card hand))

-- Given a deck, generate the hand of the bank
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty
    where playBank' deck hand 
                | value hand' > 16 = hand'
                | otherwise        = playBank' deck' hand'
              where (deck', hand') = draw deck hand

-- Shuffles the given hand
shuffle :: StdGen -> Hand -> Hand
shuffle r deck = shuffle' r deck Empty
    where shuffle' _ Empty newDeck = newDeck
          shuffle' g deck newDeck  = shuffle' g' deck' (Add card newDeck)
              where (n,g')         = randomR(1, size deck)g
                    (card, deck')  = deck -!! n

-- Return the nth card from the given hand
takeCard :: Hand -> Integer -> Card
takeCard (Add card hand) 1 = card
takeCard (Add card hand) n = takeCard hand (n-1)

-- Remove the nth card from the given hand
dropCard :: Hand -> Integer -> Hand
dropCard hand n = dropCard' hand Empty n
    where  dropCard' (Add card hand1) hand2 1 = hand2 <+ hand1
           dropCard' hand1 hand2 n            = dropCard' hand1' hand2' (n-1) 
               where (hand1', hand2')         = draw hand1 hand2

-- Remove the nth card from the given hand, and return it
pickCard :: Hand -> Integer -> (Card,Hand)
pickCard hand n = (card', hand')
    where hand' = dropCard hand n
          card' = takeCard hand n

(-!!) = pickCard

-- Test that a shuffle doesn't remove or add any cards
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

-- Check if a card belongs to a hand
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty      = False 
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Test that a shuffle doesn't change the size of a hand
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == (size $ shuffle g h)

-- Package up functions as described in lab spec
implementation = Interface {
   iEmpty = empty
   , iFullDeck = fullDeck
   , iValue = value
   , iGameOver = gameOver
   , iWinner = winner
   , iDraw = draw
   , iPlayBank = playBank
   , iShuffle = shuffle
}
 
main :: IO ()
main = runGame implementation