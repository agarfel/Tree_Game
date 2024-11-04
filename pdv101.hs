import qualified Data.Tree as Tree
import Control.Monad.State
import System.Random (randomRIO)
import System.IO


-- Effect Data
data Effect = Effect { oncredits :: Int, onhappiness :: Int }
  deriving (Eq, Show)
-- Event Data
data Event = Event { name :: (String, String, String), effects :: (Effect, Effect)}

instance Show Event where
  show (Event (name, opt1, opt2) (effect1, effect2)) =
    "\n" ++ name ++ "\n Option 1: " ++ opt1 ++ "\n Option 2: " ++ opt2

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


undoStats :: Effect -> State Stats ()
undoStats effect = do
    Stats credits happiness <- getStats
    setStats (credits - oncredits effect) (happiness - onhappiness effect)



-- Commands Data
data Cmd = Choose_1 | Choose_2 | Regret | SeeStats | Quit
  deriving (Show,Read)


-- Global Data --
events :: [Event]
events = [
  Event {name=("You have an Analysis Lecture", "Go", "Skip"), effects=(Effect {oncredits=3, onhappiness= -3}, Effect {oncredits= 0, onhappiness= 2})},
  Event {name=("The Halloween Party is tonight. Are you going?", "Yes!!!", "No, I need to study"), effects=(Effect {oncredits= -1, onhappiness=7}, Effect {oncredits=3, onhappiness= -5})},
  Event {name=("You have a Stats exam you haven't started studying for tomorrow.","Spend the night studying", "Sleep and pray"), effects=(Effect {oncredits= -1 , onhappiness= -3}, Effect {oncredits= -7, onhappiness= -5})},
  Event {name=("You have a CS project due tomorrow.", "Get ChatGPT to do it for you", "have a coffee and work all night"), effects=(Effect {oncredits= 2, onhappiness=3}, Effect {oncredits=2, onhappiness= -2})},
  Event {name=("You wake up. It's 23:58 and you realise you haven't submitted your Stats Homework", "Get up and submit it", "Hope your teammate submitted"), effects=(Effect {oncredits= 1, onhappiness= -1}, Effect {oncredits= -1, onhappiness= -3})},
  Event {name=("It's the Christmas Concert. You have 4 finals next week", "Take a break from studying and go to the concert.", "Grab some tissues and continue studying"), effects=(Effect {oncredits= -2, onhappiness=7}, Effect {oncredits=4, onhappiness= -3})},
  Event {name=("Dinner time.", "Get a Kebab", "Make some pasta"), effects=(Effect {oncredits= 0, onhappiness=3}, Effect {oncredits=0, onhappiness= 0})},
  Event {name=("You were accepted on exchange. Are you going?", "YES! I hate Palaiseau", "No. I want to get a good education"), effects=(Effect {oncredits= 0, onhappiness=10}, Effect {oncredits=7, onhappiness= -5})},
  Event {name=("You wake up. It's 9am. Your final started at 8am", "Cry", "Scream"), effects=(Effect {oncredits= -10, onhappiness= -10}, Effect {oncredits= -10, onhappiness= -11})},
  Event {name=("There's a presentation on Moebius Transformations given by Lucas Massot.", "Let's go, it'll be interesting", "You stay to study How to write math"), effects=(Effect {oncredits= 1, onhappiness=3}, Effect {oncredits=1, onhappiness= -2})},
  Event {name=("You're in the middle of a Measure Theory lecture. The professor is explaining a key concept of the course. You need to go to the bathroom...", "Hold it in. I can't miss this part", "I can read the slides later"), effects=(Effect {oncredits=1, onhappiness= -1}, Effect {oncredits= -1, onhappiness= -3})},
  Event {name=("Do you want to campaign for the L'Ore", "Yes. It'll be so much fun. Let's make the bachelor a better place:", "No. I want no work and free crepes."), effects=(Effect {oncredits= -5, onhappiness= -10}, Effect {oncredits=5, onhappiness= 7})},
  Event {name=("You're sick. Next doctor's appointment is next week", "Go to class and suffer", "Rest and take an abscence."), effects=(Effect {oncredits= 1, onhappiness= -3}, Effect {oncredits= -1, onhappiness= 3})},
  Event {name=("You have a family issue and are feeling down. What do you do:", "Mental breakdown is not on your to do list. Stick to it.", "Follow my therapist's recommendation: drink soy milk"), effects=(Effect {oncredits= 2, onhappiness= -10}, Effect {oncredits= -1, onhappiness= -11})},
  Event {name=("You have to write your art of decision making pdv essay:", "Do you trauma dump in the hope of getting a good grade", "Talk about why you choose the bachelor."), effects=(Effect {oncredits= 3, onhappiness= -3}, Effect {oncredits=1, onhappiness= 0})},
  Event {name=("The dishes are piling up in the sink.", "Clean them", "They're not mine"), effects=(Effect {oncredits= -1, onhappiness=1}, Effect {oncredits=0, onhappiness= -1})},
  Event {name=("You open youtube. Do you watch:", "3Blue 1Brown", "Actor on Actor"), effects=(Effect {oncredits= 1, onhappiness=1}, Effect {oncredits= -1, onhappiness=2})},
  Event {name=("You just joined the bachelor's book club. What do you read:", "The book of the month", "Quantum Physics chapter 7"), effects=(Effect {oncredits= 0, onhappiness=3}, Effect {oncredits=1, onhappiness= -5})},
  Event {name=("It's the end of campaign week. Do you:", "Read the drama on the BX on Campus groupchat", "Listen to the lecture"), effects=(Effect {oncredits= -1, onhappiness=7}, Effect {oncredits=1, onhappiness= -3})},
  Event {name=("The TD is feeling very long.", "Keep focusing, only one hour left", "Take a 1 sudoku break. Or 2, or 3 or ..."), effects=(Effect {oncredits= 1, onhappiness= -2}, Effect {oncredits=0, onhappiness= 5})},
  Event {name=("You don't have a bachelor thesis. You don't know what you're doing for master's. You have a project due tomorrow", "Work on the project", "Wonder what to do with your life"), effects=(Effect {oncredits= 3, onhappiness=3}, Effect {oncredits= -2, onhappiness= -7})},
  Event {name=("It's Integration!", "Study for your test", "Meet the newcommers"), effects=(Effect {oncredits= 3, onhappiness= -3}, Effect {oncredits= -1, onhappiness= 2})},
  Event {name=("You finished your project. Your computer ran out of battery before you could save it.", "Go for a walk", "Punch a wall"), effects=(Effect {oncredits= -2, onhappiness= -3}, Effect {oncredits= -2, onhappiness= -10})},
  Event {name=("It's the holidays. Do you:", "Escape Palaiseau's weather for a warmer destination", "Stay and study for your 5 midterms"), effects=(Effect {oncredits= -2, onhappiness=7}, Effect {oncredits=5, onhappiness= -5})},
  Event {name=("You just finished an assignment", "Go touch some grass", "Start one of the other 7 assignments"), effects=(Effect {oncredits=1, onhappiness=5}, Effect {oncredits=2, onhappiness= -2})},
  Event {name=("Time to choose a language! Do you take:", "Spanish, it'll be chill", "German, I want to learn a new language!"), effects=(Effect {oncredits= 2, onhappiness=2}, Effect {oncredits=1, onhappiness= -3})},
  Event {name=("You're taking healthy mind in a healthy body. Do you:", "Fall asleep during the meditation", "Realise how unhealth your lifestyle is"), effects=(Effect {oncredits= 0, onhappiness=2}, Effect {oncredits=1, onhappiness= -3})},
  Event {name=("HSS film studies final is comming.", "It's alright, it's always Citizen Kane", "I should review and rewatch all 3 films"), effects=(Effect {oncredits= -3, onhappiness=2}, Effect {oncredits=3, onhappiness=2})},
  Event {name=("It's Friday night. What do you do with your friends?", "Yams and tea", "Party and Vodka"), effects=(Effect {oncredits= 0, onhappiness=5}, Effect {oncredits= -1, onhappiness=7})},
  Event {name=("You have a homework to do. Do you team up with:", "The hardest workers", "Your friends"), effects=(Effect {oncredits= 2, onhappiness= -1}, Effect {oncredits=1, onhappiness=2})}
  ]

-- FUNCTIONS --
checkOver :: Stats -> Int
checkOver s = if credits s >= 50 then 1 else if happiness s <= 0 then 2 else 0

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
go_down :: BinZip a Event -> Maybe ((BinZip a Event), Effect)
go_down (B0 b c t2, t) = Just ((c, Node b t t2), fst (effects b)) -- From the left context
go_down (B1 t1 b c, t) = Just ((c, Node b t1 t), snd (effects b)) -- From the right context
go_down (Hole, t) = Just ((Hole, t), Effect {oncredits= 0, onhappiness= 0})  -- Cannot go down from the root


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
start_tree = Leaf 0 -- Node (head events) (Leaf 0) (Leaf 0)

start_stats :: Stats
start_stats = Stats {credits = 0, happiness = 30}


get_input :: Stats -> IO Int
get_input stats = do
  line <- getLine
  case line of
    "1" -> return 1
    "2" -> return 2
    "back" -> return 3
    "stats" -> do
      putStrLn (show stats)
      get_input stats
    "quit" -> return 4
    "see" -> return 5
    _ -> do
      putStrLn "Invalid command. Input 1 | 2 | back | stats | quit"
      get_input stats  -- recursively call get_input on invalid input

play :: IO()
play = do
    putStrLn "Welcome to the Bachelor!\n"
    putStrLn "The following commands are available: 1 | 2 | back | stats | quit\n"
    putStrLn "Right now you are happy but stupid!\n"
    putStrLn "Learn and graduate! Be careful to not get depressed!\n"

    -- Start the game with initial state
    go (Hole, start_tree) start_stats
  where
    go :: BinZip Int Event -> Stats -> IO ()
    go z stats = case z of
      (c, Leaf 0) -> 
        case checkOver stats of
          2 -> do
            putStrLn "You worked too much without results! You decide to drop out and open a coffee shop with cats."
            return ()
          1 -> do
            putStrLn "Congratulations it's your Commencement Ceremony!\n(Have you graduated though?)"
            return ()
          _ -> do
            -- Get a random event and create a new node
            event <- getEvent events 
            go (c, Node event (Leaf 0) (Leaf 0)) stats

      (_, Node e t1 t2) -> do
        putStrLn (show e)  -- Show the event

        cmd <- get_input stats    -- Get a line of input
        let (effect1, effect2) = effects e
        case cmd of
          1 -> do
            let new_stats = execState (updateStats effect1) stats
            case go_left z of
              Just z0 -> go z0 new_stats
              Nothing -> putStrLn "Cannot move left from here." >> go z new_stats
          2 -> do
            let new_stats = execState (updateStats effect2) stats
            case go_right z of
              Just z0 -> go z0 new_stats
              Nothing -> putStrLn "Cannot move left from here." >> go z new_stats
          3 -> do
            putStrLn "You wake up in sweat from a bad dream where you were making poor life choices."
            case go_down z of
                Just (z0, unEffect) -> do
                  let new_stats = execState (undoStats unEffect) stats
                  go z0 new_stats
                Nothing -> putStrLn "Cannot move back from here." >> go z stats
          4 -> do
            putStrLn "You decide to drop out. You didn't even try.\n"
            
          5-> do
            putStrLn (drawBinZip z)
            go z stats

        return ()