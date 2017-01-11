--Task 1

modulus :: Floating a => (a, a) -> a
modulus (x, y) = sqrt (x^2 + y^2)

--modulus p = sqrt ((fst p)^2 + (snd p)^2)


--Task 2

complAdd :: Floating a => (a,a) -> (a,a) -> (a,a)
complAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)


--Task 5

replicate' :: Integral a => a -> b -> [b]
replicate' 0 _ = []
replicate' n el = el : (replicate' (n-1) el)


--Task 6

take' :: Integral a => a -> [b] -> [b]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : (take' (n-1) xs)


--Task 7 - list comprehension and range

prime :: Integral a => a -> Bool
prime 1 = False
prime n = null [ d | d<-[2..(n-1)], mod n d == 0 ]


--Task 8

--take 5 primes -> [2,3,5,7,11]
primes :: Integral a => [a]
primes = [ x | x<-[2..], prime x ]


--Task 10

--removeNth 3 [1..10] -> [1,2,4,5,7,8,10]
removeNth :: Int -> [a] -> [a]
removeNth _ [] = []
removeNth n l = (take (n-1) l) ++ removeNth n (drop n l)


--Task 11 - Сито на Ератостен

--take 5 sieve -> [2,3,5,7,11]
sieve :: [Int]
sieve = helper [2..]
  where helper (x:xs) = x : helper (removeNth x xs)
