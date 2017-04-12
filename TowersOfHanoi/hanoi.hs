module Hanoi where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 start end aux = [(start, end)] 
hanoi n start end aux = hanoi (n-1) start aux end ++ [(start, end)] ++ hanoi (n-1) aux end start

