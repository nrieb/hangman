import Data.Char
import Control.Monad
import System.IO
import System.Random

logo :: [String]
logo = ["--------------------------------------------",
        "| #  #   #   #   #  #### #   #   #   #   # |",
        "| #  #  # #  ##  # #     ## ##  # #  ##  # |",
        "| #### ##### # # # #  ## # # # ##### # # # |",
        "| #  # #   # #  ## #   # #   # #   # #  ## |",
        "| #  # #   # #   #  ###  #   # #   # #   # |",
        "--------------------------------------------",
        ""]

showLogo :: IO ()
showLogo = do
  mapM_ putStrLn (logo)

intro :: String -> [String]
intro currentWord = ["Welcome to the game Hangman!",
         "",
         "The objective in this game is to guess the word.",
         "You can enter both uppercase and lowercase letters.",
         "If you think you know the word, you can type it in.",
         "You will lose if you have guessed 10 letters wrong.",
         "",
         "This is the word you need to guess: " ++ currentWord,
         ""]

showIntro :: String -> IO ()
showIntro currentWord = do 
  mapM_ putStrLn (intro currentWord)

gallows :: (Eq a, Num a) => a -> [String]
gallows 0 = (replicate 6 "") ++
            ["____________",
             ""]
gallows 1 = [""] ++
            (replicate 5 "  |") ++
            ["__|_________",
             ""]
gallows 2 = ["  _______"] ++
            (replicate 5 "  |") ++
            ["__|_________",
             ""]
gallows 3 = ["  _______",
             "  |/"] ++ 
            (replicate 4 "  |") ++
            ["__|_________",
             ""]
gallows 4 = ["  _______",
             "  |/   | ",
             "  |    O "] ++
            (replicate 3 "  |") ++
            ["__|_________",
             ""]
gallows 5 = ["  _______",
             "  |/   | ",
             "  |    O "] ++
            (replicate 2 "  |    |") ++
            ["  |",
             "__|_________",
             ""]
gallows 6 = ["  _______",
             "  |/   | ",
             "  |    O ", 
             "  |   \\|",
             "  |    | ",
             "  |",
             "__|_________",
             ""]
gallows 7 = ["  _______",
             "  |/   | ",
             "  |    O ", 
             "  |   \\|/",
             "  |    | ",
             "  |",
             "__|_________",
             ""]
gallows 8 = ["  _______",
             "  |/   | ",
             "  |    O ", 
             "  |   \\|/",
             "  |    | ",
             "  |   /",
             "__|_________",
             ""]
gallows 9 = ["  _______",
             "  |/   | ",
             "  |    O ", 
             "  |   \\|/",
             "  |    | ",
             "  |   / \\",
             "__|_________",
             ""]
gallows 10 = ["  _______",
             "  |/   | ",
             "  |    X ", 
             "  |   \\|/",
             "  |    | ",
             "  |   / \\",
             "__|_________",
             ""]

gallows _ = error "Only 0-10 are available for gallows."

invalidChar :: [String]
invalidChar = ["Only alphabetic symbols are allowed (a-z, A-Z), try again:"]

results :: [String]
results = ["---------------",
           "--- Results ---",
           "---------------",
           ""]

failure :: String -> [String]
failure guessWord = ["You guessed the wrong word. The word was " ++
                     guessWord ++
                     ". Better luck next time!",
                     ""]
success :: [String]
success = ["Congratulations you guessed the right word!", ""]


-- makeNewWord 'h' "haskell" "......." == "h......"
makeNewWord :: Eq a => a -> [a] -> [a] -> [a]
makeNewWord c = 
    zipWith (\ left right -> if left == c then left else right)

roundEnd :: (Eq a, Num a, Show a) => String -> a -> [String]
roundEnd currentWord numErrors = 
    ["The word including the letters you guessed: " ++ currentWord,
     "",
     "Amount of wrong letters: " ++ (show numErrors),
     ""] ++
    gallows numErrors

data RoundState = RoundState {roundNumber :: Int,
                              currentWord :: String,
                              numErrors :: Int,
                              output :: [String],
                              proceed :: Bool
                             } deriving (Show)

processIncorrect :: Int -> String -> String -> Int -> Char -> RoundState
processIncorrect roundNumber guessWord currentWord numErrors c =
    let newRound = (roundNumber + 1)
        newWord = makeNewWord c guessWord currentWord
        newErrors = (numErrors + 1)
        output = map (++ ['\n']) (["", "That letter was incorrect.", ""] ++
                                  (roundEnd newWord newErrors))
    in
      if newErrors < 10 then
          let addendum = 
                  [(show newRound) ++ ".     Enter the letter(s) you want to guess: "]
          in RoundState newRound newWord newErrors (output ++ addendum) True
      else
          let addendum = 
                  map (++ ['\n']) (results ++ (failure guessWord))
          in RoundState newRound newWord newErrors (output ++ addendum) False

processCorrect :: Int -> String -> String -> Int -> Char -> RoundState
processCorrect roundNumber guessWord currentWord numErrors c = 
    let newRound = (roundNumber + 1)
        newWord = makeNewWord c guessWord currentWord
        output = map (++ ['\n']) (["", "That letter was correct.", ""] ++
                                  (roundEnd newWord numErrors))
    in
      if newWord /= guessWord then
          let addendum = 
                  [(show newRound) ++ ".     Enter the letter(s) you want to guess: "]
          in RoundState newRound newWord numErrors (output ++ addendum) True
      else
          let addendum = map (++ ['\n']) (results ++ success)
          in RoundState newRound newWord numErrors (output ++ addendum) False

processValid :: Int -> String -> String -> Int -> Char -> RoundState
processValid roundNumber guessWord currentWord numErrors c = 
    if c `elem` guessWord then
        processCorrect roundNumber guessWord currentWord numErrors c
    else
        processIncorrect roundNumber guessWord currentWord numErrors c

processNonNull :: Int -> String -> String -> Int -> Char -> RoundState
processNonNull roundNumber guessWord currentWord numErrors c = 
    if (not . isAlpha) c then
        RoundState roundNumber currentWord numErrors (map (++ ['\n']) invalidChar) True
    else
        processValid roundNumber guessWord currentWord numErrors c


processGuess :: Int -> String -> String -> Int -> Char -> RoundState
processGuess roundNumber guessWord currentWord numErrors c = 
    if c == '\n' then
        RoundState roundNumber currentWord numErrors [] True
    else
        processNonNull roundNumber guessWord currentWord numErrors c

playRound :: Int -> String -> String -> Int -> IO ()
playRound roundNumber guessWord currentWord numErrors = do
  c <- liftM toLower getChar
  let (RoundState nextRound nextWord newErrors output proceed) = 
          processGuess roundNumber guessWord currentWord numErrors c
  mapM_ putStr output
  hFlush stdout
  if proceed then do
      playRound nextRound guessWord nextWord newErrors
  else do
      return ()

runGame :: String -> IO ()
runGame filename = do
  hangmanWords <- liftM lines (readFile filename)
  idx <- randomRIO (0, (length hangmanWords) - 1)
  let guessWord = hangmanWords !! idx
  let currentWord = map (const '.') guessWord
  putStrLn ("guessWord = " ++ guessWord)
  showLogo
  showIntro currentWord
  putStr "1.     Enter the letter(s) you want to guess: "
  hFlush stdout
  playRound 1 guessWord currentWord 0

main :: IO ()
main = do
  runGame "moderate-words.txt"
