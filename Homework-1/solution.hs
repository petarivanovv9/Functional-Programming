--Task 6

--1) ["strings", "safe", "get", "setting", "elon"]

--1)' [["strings"], ["safe"], ["get"], ["setting"], ["elon"]]

--2) [
--  ["safe", "strings"], -> наобратно добавяме думите които могат да се конкатенират
--                          и винаги ще взимаме head-а
--  ["strings", "setting"],
--  ["safe", "elon"],
--]

--3) [
--  ["strings", "safe", "elon"],
--  ["strings", "setting", "get"],
--]

--4) [ .. ]

lst = ["strings", "safe", "get", "setting", "elon"]

res = makeStrList lst

canConcat s1 s2 = last s1 == head s2

getMaxListWords words = foldr ( \ x res -> if getLengthList x > getLengthList res then x else res ) [] words

getLengthList lst = foldr ( \ x res -> length x + res ) 0 lst - length lst + 1

makeStrList lst = [ [x] | x<-lst ]

makeWord lst res = [ x : y | x<-lst, y<-res, x /= head y, canConcat x  (head y) ]

genListStrings lst res currMaxList
  | null res = currMaxList
  | getLengthList currMaxList < getLengthList currMax = genListStrings lst newRes currMax
  | otherwise = genListStrings lst newRes currMaxList
  where newRes = makeWord lst res
        currMax = getMaxListWords newRes

longestWord lst
  | null wordsList = []
  | otherwise = foldr1 ( \ x (_ : res) -> x ++ res ) wordsList
  where wordsList = (genListStrings lst (makeStrList lst) [])


--Task 2

isPrime 1 = False
isPrime n = null [ d | d<-[2..(n-1) ], mod n d == 0 ]

primes = [ x | x<-[2..99], isPrime x ]

isSquare n = not (null [ x | x<-[1..n], x*x == n ])

squares = [ x | x<-[1..99], isSquare x ]

oddNums = [ x | x<-[10..99], odd x, not(isPrime x) ]

myFunc = [ x | x<-[10..99], y<-primes, z<-squares, x==y+(2*z) ]

result = length([ x | x<-(oddNums), not(x `elem` myFunc) ])
