import qualified Data.Tree as Tree
import Control.Monad.State
import System.Random (randomRIO)
import System.IO


-- Effect Data
data Effect = Effect { oncredits :: Int, onhappiness :: Int }
  deriving (Show,Eq)

-- Event Data
data Event = Event { name :: String, effects :: (Effect, Effect)}

instance Show Event where
  show (Event name (effect1, effect2)) =
    "\"" ++ name ++ "\""

-- Stats Data
data Stats = Stats { credits :: Int, happiness :: Int }
  deriving (Show,Eq)

getStats :: State Stats Stats
getStats = get

setStats :: Int -> Int -> State Stats ()
setStats newCredits newHappiness = put (Stats newCredits newHappiness)

updateStats :: Effect -> State Stats ()
updateStats effect = do
    Stats credits happiness <- getStats
    setStats (credits + oncredits effect) (happiness + onhappiness effect)


-- Commands Data
data Cmd = Choose_1 | Choose_2 | Regret | SeeStats | Quit
  deriving (Show,Read)


-- Global Data --
events :: [Event]
events = [
  Event {name="Go to Analysis Lecture", effects=(Effect {oncredits= 1, onhappiness= -1}, Effect {oncredits= -1, onhappiness= 1})},
  Event {name="Go to the Halloween Party", effects=(Effect {oncredits= -1, onhappiness=3}, Effect {oncredits=1, onhappiness= -2})},
  Event {name="You have a Stats exam you haven't started studying for tomorrow. Spend the night studying?", effects=(Effect {oncredits= 3, onhappiness= -2}, Effect {oncredits= -1, onhappiness= -1})},
  Event {name="You have a CS project due tomorrow. Get ChatGPT to do it for you (y) or have a coffee and work all night (n)", effects=(Effect {oncredits= -1, onhappiness=5}, Effect {oncredits=3, onhappiness= -2})}
  ]

-- FUNCTIONS --
checkOver :: Stats -> Int
checkOver s = if credits s >= 180 then 1 else if happiness s <= 0 then 2 else 0

getEvent :: [Event] -> IO Event
getEvent xs = do
    idx <- randomRIO (0, length xs - 1)
    return (xs !! idx)

-- Tree Data ( a == Ending (Leaf) b == Event (Node) ) + Zipper

-- Define the binary tree data type
data BinTree a b = Leaf a | Node b (BinTree a b) (BinTree a b)
  deriving (Show,Eq)

-- Define the context data type for the zipper
data BinCxt a b = Hole
  | B0 b (BinCxt a b) (BinTree a b)
  | B1 (BinTree a b) b (BinCxt a b)


-- Function to plug the context back into the tree
plug :: BinCxt a b -> BinTree a b -> BinTree a b
plug Hole t = t
plug (B0 b c t2) t = plug c (Node b t t2)
plug (B1 t1 b c) t = plug c (Node b t1 t)


-- Type alias for the zipper
type BinZip a b = (BinCxt a b, BinTree a b)

-- Move left in the zipper
go_left :: BinZip a b -> Maybe (BinZip a b)
go_left (c, Node b t1 t2) = Just (B0 b c t2,t1)   -- Move to the left child
go_left(c, Leaf a) = Nothing -- Cannot move left from a leaf

-- Move right in the zipper
go_right :: BinZip a b -> Maybe (BinZip a b)
go_right (c, Node b t1 t2) = Just (B1 t1 b c, t2)  -- Move to the right child
go_right (_, Leaf a) = Nothing  -- Cannot move right from a leaf

-- Move down in the zipper (plug back into the tree)
go_down :: BinZip a b -> Maybe (BinZip a b)
go_down (B0 b c t2, t) = Just (c, Node b t t2)  -- From the left context
go_down (B1 t1 b c, t) = Just (c, Node b t1 t)  -- From the right context
go_down (Hole, t) = Just (Hole, t)  -- Cannot go down from the root


-- Drawing Trees

treeFromBin :: Show a => Show b => BinTree a b -> Tree.Tree String
treeFromBin (Leaf x)     = Tree.Node (show x) []
treeFromBin (Node b t1 t2) = Tree.Node (show b) [treeFromBin t1, treeFromBin t2]

treeCxtFromBinCxt :: Show a => Show b => BinCxt a b -> Tree.Tree String -> Tree.Tree String
treeCxtFromBinCxt Hole      t = t
treeCxtFromBinCxt (B0 b c t2) t = treeCxtFromBinCxt c (Tree.Node (show b) [t, treeFromBin t2])
treeCxtFromBinCxt (B1 t1 b c) t = treeCxtFromBinCxt c (Tree.Node (show b) [treeFromBin t1, t])

treeFromBinZip :: Show a => Show b => BinZip a b -> Tree.Tree String
treeFromBinZip (c,t) = treeCxtFromBinCxt c (Tree.Node (Tree.rootLabel t' ++ marker) (Tree.subForest t'))
  where
    t' = treeFromBin t
    marker = "@ <--you"

drawBin :: Show a => Show b => BinTree a b -> String
drawBin = Tree.drawTree . treeFromBin

drawBinZip :: Show a => Show b => BinZip a b -> String
drawBinZip = Tree.drawTree . treeFromBinZip


start_tree :: BinTree Int Event
start_tree = Node (head events) (Leaf 0) (Leaf 0)

start_stats :: Stats
start_stats = Stats {credits = 0, happiness = 100 }


play :: IO()
play = do
    putStrLn "Welcome to the Bachelor!\n"
    putStrLn "Right now you are happy but stupid!"
    putStrLn "Learn and graduate! Be careful to not get depressed!\n"

    -- Start the game with initial state
    go (Hole, start_tree) start_stats
  where
    go :: BinZip Int Event -> Stats -> IO ()
    go z stats = case z of
      (c, Leaf 0) -> 
        case checkOver stats of
          2 -> do
            putStrLn "You worked too much without results! You decide to drop out.\n"
            return ()
          1 -> do
            putStrLn "Congratulations! You have graduated!\n"
            return ()
          _ -> do
            -- Get a random event and create a new node
            event <- getEvent events 
            go (c, Node event (Leaf 0) (Leaf 0)) stats

      (_, Node e t1 t2) -> do
        putStrLn (show e)  -- Show the event

        line <- getLine     -- Get a line of input
        putStrLn line       -- Echo the input

        -- Here you could process the input and update stats, for example:
        -- if line == "learn" then updateStats (someEffect) else ...
        return ()