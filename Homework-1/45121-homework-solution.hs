--Task 1

hailstone 1 = [1]
hailstone n
  | even n = n : hailstone (n `div` 2)
  | otherwise = n : hailstone (3 * n + 1)


--Task 2

isPrime 1 = False
isPrime n = null [ d | d<-[2..(n-1) ], mod n d == 0 ]

primes = [ x | x<-[2..99], isPrime x ]

isSquare n = not (null [ x | x<-[1..n], x*x == n ])

squares = [ x | x<-[1..99], isSquare x ]

oddNums = [ x | x<-[10..99], odd x, not(isPrime x) ]

myFunc = [ x | x<-[10..99], y<-primes, z<-squares, x==y+(2*z) ]

result = length([ x | x<-(oddNums), not(x `elem` myFunc) ])


--Task 4

intercalate' str [x] = x
intercalate' str (x:xs) = x ++ str ++ (intercalate' str xs)


--Task 5

makeTreeLevel [] = []
makeTreeLevel (x:xs) = (x + (head xs)) : (makeTreeLevel (tail xs))

makeTree l = helper [l]
  where helper lst
          | length (last lst) == 1 = lst
          | otherwise = helper (lst ++ [makeTreeLevel (last lst)])

fenwick lst = (mapM_) print (reverse (makeTree lst))


--Task 6

--използвам метода на пълното изчерпване, т.е гледам всички възможни
--комбинации и взимам тази с най-голяма дължина

--1) ["strings", "safe", "get", "setting", "elon"]
--2) [["strings"], ["safe"], ["get"], ["setting"], ["elon"]]
--добавяме наобратно думите, които могат да се конкатенират и винаги ще взимаме head-а за по-удобно
--3) [["safe", "strings"], ["setting", "strings"], ["elon", "safe"], ["get", "setting"]]
--4) и т.н докато стигнем до празен списък
--5) [ ... ], когато стигнем до празен списък, т.е няма повече възможни комбинации
--то взимаме най-дългият стринг на предната итерация

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
