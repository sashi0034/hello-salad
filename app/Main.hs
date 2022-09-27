module Main where

import qualified Game (main)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Game.main
