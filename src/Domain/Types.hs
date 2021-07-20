module Domain.Types where

import RIO
import RIO.Partial (read, succ, toEnum)

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Show, Enum, Bounded, CyclicEnum, Ord)

data Player = Player {direction :: Direction, name :: Text}
  deriving (Eq, Show)

data MatchRoom
  = OpenRoom
  | ClosedRoom
  deriving (Eq, Show)

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Eq, Ord, Show, Bounded, Enum)

data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Card = Card {suit :: Suit, rank :: Rank}
  deriving (Eq, Ord, Show)

data HandProps = HandProps {visibility :: HandVisibility}
  deriving (Eq, Show, Ord)

data HandVisibility
  = Hidden
  | Shown
  deriving (Eq, Show, Ord)

defaultHandProps :: HandProps
defaultHandProps = HandProps Shown

data Hand = Hand
  { handProps :: HandProps,
    spades :: Set Card,
    hearts :: Set Card,
    diamonds :: Set Card,
    clubs :: Set Card
  }
  deriving (Eq, Ord, Show)

type Deck = Map Direction Hand

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  next :: a -> a
  next v
    | v == maxBound = minBound
    | otherwise = succ v


data Vul
  = NoneVul
  | NorthSouthVul
  | EastWestVul
  | BothVul
  | UnknownVul
  deriving (Eq, Show)

data BidLevel
  = LevelOne
  | LevelTwo
  | LevelThree
  | LevelFour
  | LevelFive
  | LevelSix
  | LevelSeven
  deriving (Eq, Ord, Show)

data BidSuit
  = Trump Suit
  | NoTrump
  deriving (Eq, Show)

data BidCall
  = Pass
  | Double
  | Redouble
  | YourCall
  | BidCall BidLevel BidSuit
  deriving (Eq, Show)


data Contract = Contract
  { contractLevel :: ContractLevel,
    suit :: BidSuit,
    scoreModifier :: Maybe Jeopardy,
    contractGoal :: Maybe ContractGoal,
    declarer :: Direction,
    onLead :: Direction
  }
  deriving (Eq, Show)

data ContractLevel
  = TrickGoal
  | ContractLevel BidLevel
  deriving (Eq, Show)

data Jeopardy
  = Doubled
  | Redoubled
  deriving (Eq, Show)

data ContractGoal
  = MaximumTricks
  | NumberOfTricks NumberOfTricks
  deriving (Eq, Show)

data NumberOfTricks
  = ZeroTricks
  | OneTrick
  | TwoTricks
  | ThreeTricks
  | FourTricks
  | FiveTricks
  | SixTricks
  | SevenTricks
  | EightTricks
  | NineTricks
  | TenTricks
  | ElevenTricks
  | TwelveTricks
  | ThirteenTricks
  deriving (Eq, Show, Enum)

instance Num NumberOfTricks where
  fromInteger x = toEnum (fromInteger x) :: NumberOfTricks
  x + y = toEnum (fromEnum x + fromEnum y)
  x * y = toEnum (fromEnum x * fromEnum y)
  abs = id
  signum _ = 1
  negate = id

data PlayedCard
  = PlayedCard
      { playedCardType :: PlayedCardType, -- TODO: rename maybe
        suit :: Suit,
        rank :: Rank,
        annotation :: Maybe Annotation
      }
  | LowestUnplayed -- either '-' or '~'
  | HighestUnplayed -- '+'
  | ImmaterialDiscard -- '.'
  deriving (Eq, Show)

data PlayedCardType
  = Discard
  | SuitFollowed
  | SuitLed
  deriving (Eq, Show)

data Trick = Trick
  { firstCard :: PlayedCard,
    secondCard :: PlayedCard,
    thirdCard :: PlayedCard,
    fourthCard :: PlayedCard
  }
  deriving (Eq, Show)

type Play = [Trick]

data Annotation
  = Good
  | Poor
  | VeryGood
  | VeryPoor
  | Speculative
  | Questionable
  | Conventional
  | Note Int Text
  deriving (Eq, Show)

data Bid = Bid
  { bidCall :: BidCall,
    annotation :: Maybe Annotation
  }
  deriving (Eq, Show)

data Auction = Auction
  { dealer :: Direction,
    vul :: Vul,
    auction :: [Bid]
  }
  deriving (Eq, Show)
