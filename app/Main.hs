module Main where

import Interface
import Database (initializeDatabase)

main :: IO ()
main = do
  initializeDatabase
  runInterface
  