--take' 3 [1..5] -> [1,2,3]
--take' 5 [10,20,30] -> [10,20,30]

take' _ [] = []
take' 0 _  = []
take' n (x:xs) = x : (take' (n-1) xs)


isPrime 1 = False
isPrime n = null [ d | d<-[2..(n-1)], mod n d == 0 ]

primes = [ x | x<-[2..], isPrime x ]

nthPrime n = primes!!n

removeNth _ [] = []
removeNth n lst = (take (n-1) lst) ++ (removeNth n (drop n lst))

--myFunc ["twostrings", "safe", "get", "setting", "elon"] -> "twostrings"

myFunc m = foldr ( \ x res -> if length x > length res then x else res ) [] m


makeSet [] = []
makeSet lst = helper lst []
  where helper [] res = res
        helper (x:xs) res
          | x `elem` res = helper xs res
          | otherwise    = helper xs (x:res)
