import Data.List
queens n = place 1 [1..n] [] []
place c [] ud dd = [[]]
place c rs ud dd = [(q:qs) | q <- rs, (q-c) `notElem` ud, (q+c) `notElem` dd, qs <- place (c+1) (delete q rs) ((q-c):ud) ((q+c):dd)]

fak' 1 = 1
fak' x = x*fak'(x-1)

fibs :: [Integer]
fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))


fix :: (a -> a) -> a
fix f = f (fix f)
