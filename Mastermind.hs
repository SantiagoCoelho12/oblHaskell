module Mastermind where

import System.IO
import System.Process
import System.IO.Unsafe
import System.Exit
import System.Directory
import Data.Time
import Data.Maybe
import Text.Read

data Result = Win | Lost deriving (Show)
data Difficulty = Easy | Hard deriving (Show)
data SO = Windows | Mac deriving (Show)

correctChar :: Char
correctChar = '/'

wrongPositionChar :: Char
wrongPositionChar = '~'

notPresentChar :: Char
notPresentChar = 'x'

clueExplanation :: IO ()
clueExplanation =
    do
      putStrLn "Clue: "
      putStrLn "/ Represents the number is in the correct position"
      putStrLn "~ Represents the number is correct but in a wrong position"
      putStrLn "x Represents the number is not correct"

filePath :: String
filePath = "mastermindLog.txt"

title :: IO ()
title = putStrLn "              Mastermind"

type Code = Integer
type Turn = Integer

play :: SO -> IO ()
play so = do
    clear so
    title
    putStrLn "Main menu:"
    putStrLn "1) Play on easy"
    putStrLn "2) Play on hard"
    putStrLn "3) Autoguesser"
    putStrLn "4) See log"
    putStrLn "5) Exit"
    putStrLn "Select option:"
    option <- getLine
    case option of
        "1" -> do
            pcCode <- randomInt Easy
            play' Easy so pcCode [] 0
            play so
        "2" -> do
            pcCode <- randomInt Hard
            play' Hard so pcCode [] 0
            play so
        "3" -> do
            autogueser so
            play so
        "4" -> do
            readLog so
            play so
        "5" -> do
            clear so
            exitSuccess
        _ -> play so

clear :: SO -> IO ()
clear Windows = do
    _ <- system "cls"
    return ()
clear Mac = do
    _ <- system "clear"
    return ()

getTotalTurns :: Difficulty -> Turn
getTotalTurns Easy = 5
getTotalTurns Hard = 10

endGameByTurns :: Difficulty -> Turn -> Bool
endGameByTurns difficulty currentTurn = currentTurn >= getTotalTurns difficulty

codeSolve :: Code -> Code -> Bool
codeSolve pcCode userCode = pcCode == userCode

getListOfClue :: String -> String -> String -> String
getListOfClue [] [] _ = []
getListOfClue (pc1:pc) (usr1:user) complete
  | pc1 == usr1 = correctChar : getListOfClue pc user complete
  | elem usr1 complete = wrongPositionChar : getListOfClue pc user complete
  | otherwise = notPresentChar : getListOfClue pc user complete

printClue :: Code -> [Code] -> IO ()
printClue pcCode [] = clueExplanation
printClue pcCode (x:xs) = do
    let pcCodeString = show pcCode
    let userCodeString = show x
    let list = getListOfClue pcCodeString userCodeString pcCodeString
    clueExplanation
    putStrLn ("Your clue for " ++ show x ++ " is: " ++ list)

randomInt :: Difficulty -> IO Integer
randomInt Easy = randomInt' 100 900
randomInt Hard = randomInt' 10000 90000

randomInt'::Integer -> Integer -> IO Integer
randomInt' sum max =
    do
        time <- getCurrentTime
        let base = (`mod` max)
             $ read $ take 6 $ formatTime defaultTimeLocale "%q" time
        return (sum + base)

isNumber :: String -> Bool
isNumber text = isJust (readMaybe text :: Maybe Int)

checkLength :: Difficulty -> Code -> Bool
checkLength Easy code = code >= 100 && code <= 999
checkLength Hard code = code >= 10000 && code <= 99999

writeLog :: Difficulty -> Result -> IO ()
writeLog d r = do
     let text = "Difficulty: " ++ show d ++ ", Result: " ++ show r ++ "\n"
     appendFile filePath text

logCounter :: [String] -> (Integer, Integer, Integer, Integer)
logCounter [] = (0,0,0,0)
logCounter xs = logCounter' xs (0,0,0,0)

logCounter' ::  [String] -> (Integer, Integer, Integer, Integer) -> (Integer, Integer, Integer, Integer)
logCounter' [] (win,lose,easy,hard) = (win,lose,easy,hard)
logCounter' (x:xs) (win,lose,easy,hard)
    | words x == ["Difficulty:", "Easy,", "Result:", "Win"]  = logCounter' xs (win + 1, lose, easy + 1, hard)
    | words x == ["Difficulty:", "Easy,", "Result:", "Lost"] = logCounter' xs (win, lose + 1, easy + 1, hard)
    | words x == ["Difficulty:", "Hard,", "Result:", "Win"]  = logCounter' xs (win + 1, lose, easy, hard + 1)
    | words x == ["Difficulty:", "Hard,", "Result:", "Lost"] = logCounter' xs (win, lose + 1, easy, hard + 1)
    | otherwise = logCounter' xs (win, lose, easy, hard)

readLog :: SO -> IO ()
readLog so =
    do
        fileExists <- doesFileExist filePath
        if fileExists
            then do
                clear so
                title
                fileText <- readFile filePath
                let l = lines fileText
                putStrLn ("Total games: " ++ show (length l))
                let (win, lose, easy, hard) = logCounter l
                putStrLn ("Total wins: " ++ show win)
                putStrLn ("Total losses: " ++ show lose)
                putStrLn ("Total easy: " ++ show easy)
                putStrLn ("Total hard: " ++ show hard)
                end <- getLine
                clear so
            else do
                clear so
                title
                putStrLn "This is your first game!"
                end <- getLine
                clear so

play' :: Difficulty -> SO -> Code ->[Code] -> Turn -> IO ()
play' d so pcCode cs t = do
    clear so
    title
    putStrLn ("Difficulty: " ++ show d ++ "  |  Turns left: " ++ show (getTotalTurns d - t))
    --putStrLn ("Code: " ++ show pcCode)
    putStrLn ("Your previous codes: " ++ show cs)
    printClue pcCode cs
    if endGameByTurns d t
        then do
            clear so
            putStrLn "Game Over! Thanks for playing"
            writeLog d Lost
            end <- getLine
            clear so
        else do
            putStrLn "Insert code:"
            charCode <- getLine
            if isNumber charCode
                then do
                    let code = read charCode
                    if codeSolve pcCode code
                        then do
                            clear so
                            putStrLn "You won! Thanks for playing"
                            writeLog d Win
                            end <- getLine
                            clear so
                        else if checkLength d code
                            then do
                                play' d so pcCode (code:cs) (t+1)
                                clear so
                            else do
                                putStrLn "Invalid input, try again!"
                                end <- getLine
                                play' d so pcCode cs t
                                clear so
                else do
                    putStrLn "We only accept numbers you know"
                    end <- getLine
                    play' d so pcCode cs t
                    clear so


autogueser:: SO -> IO ()
autogueser so = do
    clear so
    title
    putStrLn "Insert code:"
    usrCode <- getLine
    -- validar codigo
    let (n,c,s,i) = autoguess "_" usrCode nodeTree 0 []
    putStrLn ("The clue was resolved in " ++ show i ++ " steps")
    end <- getLine
    clear so

nodeTree :: Node
nodeTree = Empty

type Clue = String
type Solution = String
data Node =
      Empty
      | Node String Node Node Node Integer Integer   -- valor nodos hijos altura estado 
data Control = 
    P Integer Char -- valor posicion +
    | C Integer -- valor presente
    | N Integer Char -- valor posicion -

autoguess:: Clue -> Solution -> Node -> Integer -> [Control] ->(Node,Clue,Solution,Integer)
autoguess ("////") _ _ counter _ = (Empty,"","",counter)
autoguess ("_") solution (Node value b1 b2 b3 i1 i2 ) counter controls = case (getListOfClue solution value) of{
     clues -> autoguess clues solution (Node value b1 b2 b3 i1 i2 ) (counter++) (generateControls clues solution 0)
    }
autoguess clue solution node counter controls = case (cut node controls) of{
    n' -> case (getListOfClue solution (getTallestNodeValue n' 0)) of{
        clues ->  autoguess clue solution n' (counter++) (controls++generateControls clues solution 0)
    }
}

cut::Node -> [Control] -> Node
cut Empty _ = Empty
cut n [] = n
cut (Node value n1 n2 n3 h e) (x:xs) = case x of{
    P i c -> case i==h of{
        True -> case (check' value i c) of{
            True -> cut (Node value n1 n2 n3 h e) xs;
            False -> Empty;
        };
        False -> case (check' value i c) of{
            True -> cut (Node value n1 n2 n3 h e) xs;
            False -> cut (Node value (cut n1 x) (n2 x) (n3 x) h 0) xs;
        }
    };
    C c -> check' l 0 c || check' l 1 c || check' l 2 c;
    P i c -> case i==h of{
        True -> case (not (check' value i c)) of{
            True -> cut (Node value n1 n2 n3 h e) xs;
            False -> Empty;
        };
        False -> case (not (check' value i c)) of{
            True -> cut (Node value n1 n2 n3 h e) xs;
            False -> cut (Node value (cut n1 x) (n2 x) (n3 x) h 0) xs;
        }
    }
}

generateControls:: String -> String ->Integer -> [Control]
generateControls [] _ _ = []  
generateControls (x:xs) (y:ys) counter = case x of {
    correctChar -> [P counter y] ++ (generateControls xs ys counter++);
    wrongPositionChar ->[C y]++(generateControls xs ys counter++);
    notPresentChar ->[N counter y]++(generateControls xs ys counter++)
}

check::String -> Control ->Bool
check l control = case control of{
    P i c -> check' l i c;
    C c -> check' l 0 c || check' l 1 c || check' l 2 c;
    P i c -> not (check' l i c)
}

check':: String-> Integer -> Char -> Bool
check' (y:ys) 0 v = v==y
check' (y:ys) 1 v = check' ys 0 v
check' (y:ys) 2 v = check' ys 1 v

getTallestNodeValue:: Node-> Integer -> String 
getTallestNodeValue (N value n1 n2 n3 h e) i = case getHeight

getHeight:: Node -> Integer
getHeight Empty = 0
getHeight (N v n1 n2 n3 h e) = e + getHeight n1 + getHeight n2 + getHeight n3;
