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
        "--------------------------------------------"]

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
         "",
         "This is the word you need to guess: " ++ currentWord,
         "",
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

printGallows :: (Eq a, Num a) => a -> IO ()
printGallows n = do
  mapM_ putStrLn (gallows n)

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

playRound :: (Ord a, Num a, Show a, Num b, Show b) => b -> String -> String -> a -> IO ()
playRound roundNumber guessWord currentWord numErrors = do
  c <- liftM toLower getChar
  let nextRound = (roundNumber + 1)
  if c == '\n' then  do
      playRound roundNumber guessWord currentWord numErrors
  else do
      if (not . isAlpha) c then do
          putStrLn "Only alphabetic symbols are allowed (a-z, A-Z), try again:"
          playRound nextRound guessWord currentWord numErrors
      else do
          let newWord = makeNewWord c guessWord currentWord
          if c `elem` guessWord then do
              mapM_ putStrLn (["", "That letter was correct.", ""] ++
                              (roundEnd newWord numErrors))
              if guessWord == newWord then do
                  mapM_ putStrLn (results ++ success)
              else do
                putStr ((show nextRound) ++ ".     Enter the letter(s) you want to guess: ")
                hFlush stdout
                playRound nextRound guessWord newWord numErrors
          else do 
              mapM_ putStrLn (["", "That letter was incorrect.", ""] ++ 
                            (roundEnd newWord (numErrors + 1)))
              if (numErrors + 1) < 10 then do
                  putStr ((show nextRound) ++ ".     Enter the letter(s) you want to guess: ")
                  hFlush stdout
                  playRound nextRound guessWord newWord (numErrors + 1)
              else do
                  mapM_ putStrLn (results ++ (failure guessWord))

runGame :: String -> IO ()
runGame filename = do
  hangmanWords <- liftM lines (readFile filename)
  idx <- randomRIO (0, (length hangmanWords) - 1)
  let guessWord = hangmanWords !! idx
  let currentWord = map (const '.') guessWord
  showIntro currentWord
  putStr "1.     Enter the letter(s) you want to guess: "
  hFlush stdout
  playRound 1 guessWord currentWord 0

main :: IO ()
main = do
  showLogo
  runGame "moderate-words.txt"
