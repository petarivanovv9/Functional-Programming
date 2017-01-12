--Task 4

--compress [1,1,2,3,3,3,4,2,2,2,1,1] -> [(1,2),(2,1),(3,3),(4,1),(2,3),(1,1)]
--compress "abba" -> [('a',1),('b',2),('a',1)]

compress [] = []
compress lst = ((head lst),common) : compress rest
  where common = length (takeWhile ( \ x -> x == (head lst) ) lst )
        rest   = dropWhile ( \ x -> x == (head lst) ) lst


--Task 5

--по списък от стойности връща дължината на най-дългия подсписък, съставен от еднакви стойности
--maxRepeated [1,1,2,3,3,3,4,2,2,2,1,1] -> 3

maxRepeated lst = maximum (map ( \ (x,y) -> y ) (compress lst))


--Task 6

--makeSet [1,1,2,3,3,3,4,2,2,2,1,1] -> [1,2,3,4]
--makeSet "abba" -> "ab"

makeSet [] = []
makeSet lst = helper lst []
  where helper [] res = res
        helper (x:xs) res
          | x `elem` res = helper xs res
          | otherwise    = helper xs (x:res)
