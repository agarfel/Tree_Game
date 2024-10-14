-- Tree Data (a == Ending b == State)
import Data.Tree

data BinCxt a = Hole
  | B0 (BinCxt a) (BinTree a)
  | B1 (BinTree a) (BinCxt a)

type BinZip a = (BinCxt a, BinTree a)

data BinTree a b = Leaf a | Node b (BinTree a b) (BinTree a b)
  deriving (Show,Eq)

-- Ending Data

-- Effect Data
data Effect = Effect { oncredits :: Int, onhappiness :: Int }

-- Event Data
data Event = Event { name :: String, effects :: (Effect, Effect)}

-- Stats Data
data Stats = Stats { credits :: Int, happiness :: Int }

-- State Data
data State = State Event Stats 

-- Commands Data
data Cmd = Choose_1 | Choose_2 | Regret | SeeStats | Quit
  deriving (Show,Read)


-- Global Data --

game :: BinTree String State

events :: [Event]
events = [
  Event {name="Say Hi", effects=(Effect {oncredits= -1, onhappiness=1}, Effect {oncredits=1, onhappiness= -1})}]


-- FUNCTIONS --

checkOver :: Stats -> Int
checkOver s = if credits s >= 180 then 1 else if happiness s <= 0 then 2 else 0

