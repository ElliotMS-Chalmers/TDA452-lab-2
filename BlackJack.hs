module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

-- | A0
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 2 + size Empty
            , 2]

-- | A1
display :: Hand -> String
display Empty               = "There are no cards in your hand"
display (Add card Empty)    = show (rank card) ++ " of " ++ show (suit card) ++ "."
display (Add card hand)     = show (rank card) ++ " of " ++ show (suit card) ++ ", " ++ display hand

-- | A2
initialValue :: Hand -> Integer
initialValue Empty                             = 0
initialValue (Add (Card Ace _) hand)           = 11  + initialValue hand
initialValue (Add (Card (Numeric num) _) hand) = num + initialValue hand
initialValue (Add _ hand)                      = 10  + initialValue hand

numberOfAces :: Hand -> Integer
numberOfAces Empty                   = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand
numberOfAces (Add _ hand)            = 0 + numberOfAces hand

value :: Hand -> Integer
value hand = if val > 21 then val - 10 * numberOfAces hand else val 
  where val = initialValue hand

-- | A3
gameOver :: Hand -> Bool
gameOver Empty  = False
gameOver hand   = value hand > 21

-- | A4
winner :: Hand -> Hand -> Player
winner guest bank 
    | gameOver guest            = Bank
    | gameOver bank             = Guest
    | value guest > value bank  = Guest
    | otherwise                 = Bank

-- | B1
(<+) :: Hand -> Hand -> Hand
Empty <+ hand2            = hand2
(Add card hand1) <+ hand2 = Add card (hand1 <+ hand2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool 
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = (size h1::Int) + (size h2::Int) == (size (h1 <+ h2)::Int)

-- -- | B2
fullDeck :: Hand
fullDeck = fullSuit Diamonds <+ fullSuit Clubs <+ fullSuit Hearts <+ fullSuit Spades

fullSuit:: Suit -> Hand
fullSuit s = Add(Card Jack s) (Add (Card Queen s) (Add (Card King s) Empty)) <+
             Add (Card Ace s) (listOfCardsToHand [Card (Numeric num) s | num <- [2..10]])
             
listOfCardsToHand :: [Card] -> Hand
listOfCardsToHand cs = foldr Add Empty cs


-- | B3
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _              = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, Add card hand) 


-- | B4
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty 

playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck bank
  | value biggerHand < 16 = playBankHelper smallerDeck biggerHand
  | otherwise             = biggerHand
  where (smallerDeck, biggerHand) = draw deck bank

-- | B5
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g deck = shuffleDeckHelper g deck Empty

shuffleDeckHelper :: StdGen -> Hand -> Hand -> Hand
shuffleDeckHelper _ Empty acc               = acc
shuffleDeckHelper g currentHand acc         = shuffleDeckHelper newGen remainingHand (Add removedCard acc)
  where 
    (index, newGen)                     = randomR (0, (size currentHand::Integer) - 1) g
    (removedCard, remainingHand)        = removeCardAt currentHand index

removeCardAt :: Hand -> Integer -> (Card, Hand)
removeCardAt Empty _                     = error "removeIndex: The hand is empty"
removeCardAt hand index
  | index < 0                            = error "removeIndex: Index less than 0"
  | index > ((size hand::Integer) - 1)   = error "removeIndex: Index larger than size of hand"
removeCardAt (Add card rest) 0           = (card, rest)
removeCardAt (Add card rest) index       = (newCard, Add card newRest)
  where (newCard, newRest)               = removeCardAt rest (index - 1)

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool 
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffleDeck g h

belongsTo :: Card -> Hand -> Bool 
c `belongsTo` Empty = False 
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size hand == size (shuffleDeck g hand)

-- | B6
implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }
  
main :: IO () 
main = runGame implementation