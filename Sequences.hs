{-# LANGUAGE BlockArguments #-}
module Sequences where

import Data.Char (ord, chr)
import Data.Time.Format.ISO8601 (yearFormat)
import Distribution.Simple.Utils (xargs)

-- Returns the first argument if it is larger than the second,
-- the second argument otherwise
maxOf2 :: Int -> Int -> Int
maxOf2 x y
  | x > y = x
  | otherwise = y

-- Returns the largest of three Ints
maxOf3 :: Int -> Int -> Int -> Int
maxOf3 x y z
  = maxOf2 x (maxOf2 y z)

-- Returns True if the character represents a digit '0'..'9';
-- False otherwise
isADigit :: Char -> Bool
isADigit a
  | ord(a) >= 48 && ord(a) <= 57 = True
  | otherwise = False

-- Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
-- False otherwise
isAlpha :: Char -> Bool
isAlpha a
  | (ord(a) >= 65 && ord(a) <= 90) || (ord(a)>= 97 && ord(a) <= 122) = True
  | otherwise = False
-- Returns the integer [0..9] corresponding to the given character.
-- Note: this is a simpler version of digitToInt in module Data.Char,
-- which does not assume the precondition.
digitToInt :: Char -> Int  -- perhaps it is possible to use a condition check if the function inserted is an unwanted/invalid input, e.g. not a digit (perhaps an alphabet input)
-- Pre: the character is one of '0'..'9'
digitToInt x =
  if isADigit x then ord(x) - ord('0') -- if x is a digit, then we perform the transformation
  else 404 -- if x is not a digit, then the function returns 99, which is not a possible transformation since our inputs are single digits, indicating an error

-- Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.
toUpper :: Char -> Char
toUpper b =
  if ord(b) >= ord('a') && ord(b) <= ord('z') then chr(ord(b) - ord(' ')) -- for the case where the input is a lower case and requires a transformation to uppercase
  else if ord(b) >= ord('A') && ord(b) <= ord('Z') then b -- for the case where the input is an upper case and does not require further transformation
  else '0'   -- returns the character 0 if the input is invalid (not an alphabet based on ASCII)
--
-- Sequences and series
--

-- Arithmetic sequence
arithmeticSeq :: Double -> Double -> Int -> Double
arithmeticSeq a d n
  = fromIntegral n * d + a
-- Geometric sequence
geometricSeq :: Double -> Double -> Int -> Double
geometricSeq a r n
  = a * r ^ fromIntegral n
-- Arithmetic series
arithmeticSeries :: Double -> Double -> Int -> Double
arithmeticSeries a d n
  = (1 + fromIntegral n) * (a + (d * fromIntegral n) / 2)
-- Geometric series
geometricSeries :: Double -> Double -> Int -> Double
geometricSeries a r n =
  if r == 1 then a * (fromIntegral n + 1)
  else a * (1 - r^(fromIntegral n + 1)) / (1 - r)
  