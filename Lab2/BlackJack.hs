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
