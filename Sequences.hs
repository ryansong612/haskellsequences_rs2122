module Sequences where

import Data.Char (ord, chr)

-- Returns the first argument if it is larger than the second,
-- the second argument otherwise
maxOf2 :: Int -> Int -> Int
maxOf2 
  = undefined

-- Returns the largest of three Ints
maxOf3 :: Int -> Int -> Int -> Int
maxOf3
  = undefined

-- Returns True if the character represents a digit '0'..'9';
-- False otherwise
isADigit :: Char -> Bool
isADigit
  = undefined

-- Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
-- False otherwise
isAlpha :: Char -> Bool
isAlpha
  = undefined

-- Returns the integer [0..9] corresponding to the given character.
-- Note: this is a simpler version of digitToInt in module Data.Char,
-- which does not assume the precondition.
digitToInt :: Char -> Int
-- Pre: the character is one of '0'..'9'
digitToInt
  = undefined

-- Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.
toUpper :: Char -> Char
toUpper 
  = undefined

--
-- Sequences and series
--

-- Arithmetic sequence
arithmeticSeq :: Double -> Double -> Int -> Double
arithmeticSeq 
  = undefined

-- Geometric sequence
geometricSeq :: Double -> Double -> Int -> Double
geometricSeq 
  = undefined

-- Arithmetic series
arithmeticSeries :: Double -> Double -> Int -> Double
arithmeticSeries 
  = undefined

-- Geometric series
geometricSeries :: Double -> Double -> Int -> Double
geometricSeries 
  = undefined
