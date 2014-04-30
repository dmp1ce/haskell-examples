-- Random - http://hackage.haskell.org/package/random

import System.Random (getStdRandom, randomR, random)

-- From package documentation
rollDice :: IO Int
rollDice = getStdRandom $ randomR (1, 6)

-- Add dice rolls together (shows how to unwrap the IO Monad to use the underlying data)
rollTwoDice :: IO Int
rollTwoDice = rollDice >>= (\i1 -> rollDice >>= (\i2 -> return $ i1 + i2) )

-- Alternate syntax, without parentheses
rollTwoDice' :: IO Int
rollTwoDice' = rollDice >>=
  \i1 -> rollDice >>=
  \i2 -> return $ i1 + i2

-- Roll two dice (syntantic sugar edition)
rollTwoDice_Sugar :: IO Int
rollTwoDice_Sugar = do
  i1 <- rollDice
  i2 <- rollDice
  return $ i1 + i2

-- For random numbers it is important to note that they necessarily must be wrapped in 
-- the IO monad because in order for the random number to be truely random, it needs
-- to be collected from the outside world using INPUT from the host OS.
-- This means, to do anything with the random number requires binding the data wrapped 
-- in the IO monand, using it for whatever you want, and then putting it back in the 
-- IO monad.
-- For this reason, all 'random' function will return 'IO Type' instead of just 'Type'

-- Basic functions for producing various random types
randomIntRange :: (Int, Int) -> IO Int
randomIntRange (a, b) = getStdRandom $ randomR (a, b)

-- Random Int based on the Int default range
randomInt :: IO Int
randomInt = getStdRandom random

-- Random Int based on the Char default range
randomChar :: IO Char
randomChar = getStdRandom random

-- Simple way to get an ASCII character from a range
randomASCIIChar :: IO Char
randomASCIIChar = getStdRandom $ randomR ('0', 'z')

-- Get a random item out of a list
selectRandomElement :: [a] -> IO a
selectRandomElement [] = error "Cannot select an element from an empty list."
selectRandomElement list = randomIntWithinRange >>=
  \r -> return $ list !! r
  where
  randomIntWithinRange = getStdRandom $ randomR (0, length list - 1)

-- Example of getting a random a-zA-Z 
getRandomLetter :: IO Char
getRandomLetter = selectRandomElement "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-"

-- Example of getting a random string composed of a-zA-Z
getRandomString :: Int -> IO String
getRandomString size = addRandomLettersToString "" size
  where
  addRandomLettersToString :: String -> Int -> IO String
  addRandomLettersToString s n
    | length s >= n = return s
    | length s < n = do
      a <- getRandomLetter
      b <- addRandomLettersToString (s ++ a:[]) n
      return b
  addRandomLetterToString :: String -> IO String
  addRandomLetterToString s = do
    a <- getRandomLetter
    return (s ++ a:[])

main :: IO ()
main = do
  putStrLn "The purpose of this project is to show some examples of the random package."
  putStrLn "To explore the various examples run 'cabal repl' and then try ':browse'" 
  putStrLn "to see the various examples that can be run."
