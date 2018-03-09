module Main where

import Lib (generateKey, mapMessage, prepare)

main :: IO ()
main = do
  putStrLn "Please enter e to encode or d to decode:"
  mode <- getLine
  processMode mode

processMode :: String -> IO ()
processMode "e" = do
  putStrLn "Please enter a message to encode:"
  message <- getLine
  processMessage "e" $ prepare message
processMode "d" = do
  putStrLn "Please enter a message to decode:"
  message <- getLine
  processMessage "d" $ prepare message
processMode _ = do
  putStrLn "Wrong mode!"
  main

processMessage :: String -> String -> IO ()
processMessage mode "" = do
  putStrLn "The prepared message is an empty string. Can not process an empty string!"
  processMode mode
processMessage mode message =
  let
    key = generateKey message
    (f, label) = if mode == "e" then ((+), "The encoded message:") else ((-), "The decoded message:")
  in
    do
      putStrLn "The key:"
      putStrLn key
      putStrLn label
      putStrLn $ mapMessage f message key
