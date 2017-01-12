--Task 1

--sumProducts [[1,2,3], [4,5], [], [-2,3,0,5,1]] -> 27 -- 27 = 6 + 20 + 1 + 0

sumProducts m = sum (map product nonEmptyLists)
  where nonEmptyLists = filter ( \ x -> not (null x)) m

sumProducts2 m = sum products
  where products = map ( \ lst -> (foldr ( \ x res -> x*res ) 1 lst) ) m


--Task 2

--occurrences [1..6] [1,3,4,3,2,3,3,0,5,3,1] -> [2,1,5,1,1,0]

occurrences lst1 lst2 = [ count elem lst2 | elem<-lst1 ]
  where count elem lst = length (filter ( \ x -> x == elem) lst)


--Task 6

--matchLengths [[1..4],[0..3],[5,4,8,10]] -> True
--matchLengths [[1..4],[0..3],[],[5,4,8,10]] -> False

matchLengths lst = allEquals (map length lst)
  where allEquals l = all ( \ x -> x == head l ) l


