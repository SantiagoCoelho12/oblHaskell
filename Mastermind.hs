-- Santiago Coelho 204113
-- Luca Bessonart 231764
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
data Node =
      Empty
      | Node String Node Node Node Integer Integer   -- valor nodos hijos altura estado
      deriving (Show)
data Control = 
    P Integer Char -- valor posicion +
    | C Char -- valor presente
    | N Integer Char -- valor posicion -
    deriving (Show)

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

nodeTree :: Node
nodeTree = Empty

buildTree::Node
buildTree = Node "110"
            (Node "002" 
                (Node "002" (Node "000" Empty Empty Empty 2 1) (Node "001" Empty Empty Empty 2 1) Empty 1 1) 
                (Node "012" (Node "010" Empty Empty Empty 2 1) (Node "011" Empty Empty Empty 2 1) Empty 1 1)
                (Node "022" (Node "020" Empty Empty Empty 2 1) (Node "021" Empty Empty Empty 2 1) Empty 1 1)
                0
                1
            )
            (Node "102" 
                (Node "102" (Node "100" Empty Empty Empty 2 1) (Node "101" Empty Empty Empty 2 1) Empty 1 1) 
                (Node "112" Empty (Node "111" Empty Empty Empty 2 1) Empty 1 1) 
                (Node "122" (Node "120" Empty Empty Empty 2 1) (Node "121" Empty Empty Empty 2 1) Empty 1 1)
                0
                1
            )
            (Node "202" 
                (Node "202" (Node "200" Empty Empty Empty 2 1) (Node "201" Empty Empty Empty 2 1) Empty 1 1) 
                (Node "212" (Node "210" Empty Empty Empty 2 1) (Node "211" Empty Empty Empty 2 1) Empty 1 1)
                (Node "222" (Node "220" Empty Empty Empty 2 1) (Node "221" Empty Empty Empty 2 1) Empty 1 1)
                0
                1
            )
            (-1)
            1

type Code = Integer
type Turn = Integer
type Clue = String
type Solution = String

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
    putStrLn "Insert code:" -- se asume codigo de 3 digitos 0, 1 o 2
    usrCode <- getLine
    -- validar codigo
    let (n,c,s,i) = autoguess "_" usrCode buildTree 0 []
    --let (Node v b1 b2 b3 i1 i2) = getTallestNode (cut buildTree [C '2']) (-1)
    --putStrLn (show v)
    putStrLn ("The clue was resolved in " ++ show i ++ " steps")
    end <- getLine
    clear so

autoguess:: Clue -> Solution -> Node -> Integer -> [Control] ->(Node,Clue,Solution,Integer)
autoguess ("///") _ _ counter _ = (Empty,"","",counter)
autoguess ("_") solution (Node value b1 b2 b3 i1 i2 ) counter controls = case (getListOfClue solution value solution) of{
     clues -> autoguess clues solution (Node value b1 b2 b3 i1 i2 ) ((counter+1)) (generateControls clues value 0)
    }
autoguess clue solution node counter controls = case (cut node controls) of{
    n' -> case (getValue (getTallestNode n' 0)) of{
            value-> case (getListOfClue solution value solution) of{
            newclues ->  autoguess newclues solution (cut' n' value) ((counter+1)) (controls++(generateControls newclues value 0))
        }
    }
}

cut::Node -> [Control] -> Node
cut Empty _ = Empty
cut n [] = n
cut (Node value n1 n2 n3 h e) (x:xs) = case x of{
    P i c -> case (i==h) of{
        True -> case (check' value i c) of{
            True -> cut (Node value (cut n1 [x]) (cut n2 [x]) (cut n3 [x]) h e) xs;
            False -> Empty;
        };
        False -> case (check' value i c) of{
            True -> cut (Node value (cut n1 [x]) (cut n2 [x]) (cut n3 [x]) h e) xs;
            False -> cut (Node value (cut n1 [x]) (cut n2 [x]) (cut n3 [x]) h 0) xs;
        }
    };
    C c -> case (check' value 0 c || check' value 1 c || check' value 2 c) of{
            True-> cut (Node value (cut n1 [x]) (cut n2 [x]) (cut n3 [x]) h e) xs;
            False-> cut (Node value (cut n1 [x]) (cut n2 [x]) (cut n3 [x]) h 0) xs;
    };
    N i c -> case (i==h) of{
        True -> case (not (check' value i c)) of{
            True -> cut (Node value (cut n1 [x]) (cut n2 [x]) (cut n3 [x]) h e) xs;
            False -> Empty;
        };
        False -> case (not (check' value i c)) of{
            True -> cut (Node value (cut n1 [x]) (cut n2 [x]) (cut n3 [x]) h e) xs;
            False -> cut (Node value (cut n1 [x]) (cut n2 [x]) (cut n3 [x]) h 0) xs;
        }
    }
}

generateControls:: String -> String ->Integer -> [Control]
generateControls [] _ _ = []  
generateControls (x:xs) (y:ys) counter = case x of {
    '/' -> [P counter y] ++ (generateControls xs ys (counter+1));
    '~' ->[C y]++(generateControls xs ys (counter+1));
    'x' ->[N counter y]++(generateControls xs ys (counter+1))
}

check::String -> Control ->Bool
check l control = case control of{
    P i c -> check' l i c;
    C c -> check' l 0 c || check' l 1 c || check' l 2 c;
    N i c -> not (check' l i c)
}

check':: String-> Integer -> Char -> Bool
check' (y:ys) 0 v = v==y
check' (y:ys) 1 v = check' ys 0 v
check' (y:ys) 2 v = check' ys 1 v

getTallestNode:: Node-> Integer -> Node 
getTallestNode (Node value n1 n2 n3 h e) i = case e of{
        1-> (Node value n1 n2 n3 h e);
        0-> (getTallestSubNode ([(n1,(getHeight n1)),(n2,(getHeight n2)),(n3,(getHeight n3))]) (-1) Empty);
        _-> error "estado incorrecto"
}   
getTallestNode Empty i = Empty

getTallestSubNode:: [(Node,Integer)]-> Integer -> Node -> Node
getTallestSubNode [ ] i n = n
getTallestSubNode ((newnode,i):xs) max maxnode = case (i>max) of{
    True -> getTallestSubNode xs i newnode;
    False ->  getTallestSubNode xs max maxnode 
}

getValue:: Node -> String
getValue (Node v _ _ _ _ _ )= v
getValue Empty = error "empty node"

getHeight:: Node -> Integer
getHeight Empty = 0
getHeight (Node v n1 n2 n3 h e) = e + getHeight n1 + getHeight n2 + getHeight n3;

cut':: Node -> String -> Node
cut' Empty _ = Empty
cut' (Node v n1 n2 n3 h e) string = case (v==string) of {
    True -> Node v (cut' n1 string) (cut' n2 string) (cut' n3 string) h 0;
    False -> Node v (cut' n1 string) (cut' n2 string) (cut' n3 string) h e;  
}