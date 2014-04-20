-- Random - http://hackage.haskell.org/package/random

import System.Random (getStdRandom, randomR)

-- From package documentation
rollDice :: IO Int
rollDice = getStdRandom (randomR (1, 6))

-- Add dice rolls together (shows how to unwrap the IO Monad to use the underlying data)
rollTwoDice :: IO Int
rollTwoDice = rollDice >>= (\i1 -> rollDice >>= (\i2 -> return $ i1 + i2) )

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
randomIntRange (a, b) = getStdRandom (randomR (a, b))

randomChar :: Char
randomChar = 'h'

main :: IO ()
main = do
  putStrLn "The purpose of this project is to show some examples of the random package."
  putStrLn "To explore the various examples run 'cabal repl' and then try ':browse'" 
  putStrLn "to see the various examples that can be run."
