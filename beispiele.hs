import Data.List
queens n = place 1 [1..n] [] []
place c [] ud dd = [[]]
place c rs ud dd = [(q:qs) | q <- rs, (q-c) `notElem` ud, (q+c) `notElem` dd, qs <- place (c+1) (delete q rs) ((q-c):ud) ((q+c):dd)]



fak' 0 = 1
fak' x = x*fak'(x-1)


factorial n = product [1..n] 

--Für den Interpreter, for the lulz
--  take 12 (cycle "LOL ")

--Für den Interpreter
--  [ x | x <- [50..100], x `mod` 7 == 3]


fibs :: [Integer]
fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))
-- drop 29 (take 30 fibs))

