module CalcFunctions(adds, subs, muls, divs, pis, validateUser, loadHistory, addToHistory, history2String, splitLines, loadFile, getUserHistory) where

import Prelude
import System.IO.Unsafe
import Control.Exception
import Numeric (showIntAtBase)
import Data.Char

{- functions for the basic arithmetic operations, returning a string -}

adds a b = show (a+b)

subs a b = show (a-b)

muls a b = show (a*b)

divs a b = show (a `div` b)

{- Converts a history (list of lists of strings) to a string -}
history2String history = foldr (\x y->x++"\n"++y) "" $ map (\line->foldr (\x y->x++":"++y) "" line) history

{- returns the history of a given user -}
getUserHistory history user = filter (\(uname:_)->uname==user) history

{- adds the given info to the given user's history -}
addToHistory user info history | userExists = (user : info : userHistory):removeUserFromHistory
                               | otherwise  = [user,info]:history
  where
    userExists = (length finduser) > 0
    userHistory = tail $ head $ finduser
    finduser = filter (\(uname:_)->uname==user) history
    removeUserFromHistory = filter (\(uname:_)->uname/=user) history
    
lineToString :: [String] -> String
lineToString line = foldr (\s1 s2->s1++":"++s2) "" line

{- loads the history and returns it as a list -}
loadHistory = map (\line->splitLines line ':') $ splitLines (loadFile "history.txt") '\n'

loadFile f = unsafePerformIO . readFile $ f
   
{- Validates the given user and pasword -}   
validateUser user pass = 0 < (length $ 
                                filter (\[u,p]->user==u && pass==p) $ 
                                   map (\line->splitLines line ' ') $ 
                                     splitLines (loadFile "users.txt") '\n')

splitLines [] _ = []
splitLines line split = (takeWhile (\c -> c /= split) line) : continue (dropWhile (\c -> c/=split) line)
   where
    continue [] = []
    continue x = splitLines (tail x) split

    
-- Pi --

{- pow a b computes a^b -}
pow :: Integer -> Integer -> Integer
pow base 0 = 1
pow base 1 = base
pow base exp | even exp  = pow2 * pow2
             | otherwise = base * pow base (exp - 1)
   where
     pow2 = pow base (div exp 2)

{- powmod a b c computes (a^b)mod c -}
powmod base 0 _ = 1
powmod base 1 m = mod base m
powmod base exp m | even exp  = mod (pow2 * pow2) m
                  | otherwise = mod (base * powmod base (exp - 1) m) m
     where
        pow2 = powmod base (div exp 2) m

{- compute a part of S in the formula for BBP algorithm -}
part1 j n = part1' j n n
part1' j n (-1) = 0
part1' j n k =  decimalPart $ ((part1' j n (k-1))) + ((fromIntegral (powmod 16 (n-k) r)) / fromIntegral r)
 where
   r = 8*k+j
  
{- Returns the decimal part of a double -}  
decimalPart :: Double -> Double
decimalPart x = x - fromIntegral (floor x)

{- compute the second part of S in the formula for BBP algorithm -}

part2 j n = part2' j n 1 0.0

part2' :: Integer -> Integer -> Integer -> Double -> Double
part2' j n k oldt | t == oldt = t
                  | otherwise = part2' j n (k+1) t
  where
     t = oldt + 1.0/((fromIntegral (pow 16 k))*(fromIntegral (8*k+j)))

{- used for BBP algorithm -}
s :: Integer -> Integer -> Double
s j n = (part1 j n) + (part2 j n)
   
{- The BBP algorithm. pis n returns the n-th hexadecimal digit of Pi. -}
pis n =  head $ showIntAtBase 16 intToDigit (floor (x*fromInteger (pow 16 14))) ""
  where
    x = decimalPart (4 * (s 1 n2) - 2*(s 4 n2) - (s 5 n2) - (s 6 n2))
    n2 = n - 1
    