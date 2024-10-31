import System.Random (randomRIO)
import pdv101

-- Commands Data
data Cmd = Choose_1 | Choose_2 | Regret | SeeStats | Quit
  deriving (Show,Read)


-- Global Data --
events :: [Event]
events = [
  Event {name="Say Hi", effects=(Effect {oncredits= -1, onhappiness=1}, Effect {oncredits=1, onhappiness= -1})}]

-- FUNCTIONS --

checkOver :: Stats -> Int
checkOver s = if credits s >= 180 then 1 else if happiness s <= 0 then 2 else 0

getEvent :: [Event] -> IO Event
getEvent xs = do
    idx <- randomRIO (0, length xs - 1)
    return (xs !! idx)