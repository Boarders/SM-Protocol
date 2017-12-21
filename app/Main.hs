{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Protocol
import Data.Char(isDigit)

main :: IO ()
main = do
         putStrLn "First Input: "
         x <- getLine
         let x' = removeSpaces x
         if any (not <$> isDigit) x' then 
           print Error
         else 
           do  let xInteger :: Integer = read x'
               putStrLn "Second Input: "
               y <- getLine
               let y' = removeSpaces y
               if any (not <$> isDigit) y' then 
                 print Error
               else
                 do  let yInteger :: Integer = read y'  
                     result <- protocol (xInteger,yInteger)
                     print result

removeSpaces = head.words