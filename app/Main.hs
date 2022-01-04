module Main where

import System.Environment

import AOC2019.Day5

main :: IO ()
main = do
  args <- getArgs
  inputs <- load (head args)
  exec inputs
