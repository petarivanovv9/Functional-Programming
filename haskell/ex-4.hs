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


--Task 7

--setUnion [1,2,3,5] [2,4,5,6,7] -> [1,2,3,4,5,6,7]
--setIntersect [1,2,3,5] [2,4,5,6,7] -> [2,5]
--setDiff [1,2,3,5] [2,4,5,6,7] -> [1,3]
--setDiff [2,4,5,6,7] [1,2,3,5] -> [4,6,7]

setUnion s1 [] = s1
setUnion [] s2 = s2
setUnion (x:xs) (y:ys)
  | x < y  = x : setUnion xs (y:ys)
  | x > y  = y : setUnion (x:xs) ys
  | x == y = x : setUnion xs ys


setIntersect s1 [] = []
setIntersect [] s2 = []
setIntersect (x:xs) (y:ys)
  | x < y  = setIntersect xs (y:ys)
  | x > y  = setIntersect (x:xs) ys
  | x == y = x : setIntersect xs ys


setDiff s1 [] = s1
setDiff [] s2 = []
setDiff (x:xs) (y:ys)
  | x == y = setDiff xs ys
  | x < y  = x : setDiff xs (y:ys)
  | x > y  = setDiff (x:xs) ys


setSumDiff s1 [] = s1
setSumDiff [] s2 = s2
setSumDiff s1 s2 = setUnion (setDiff s1 s2) (setDiff s2 s1)


setSum s1 [] = s1
setSum [] s2 = s2
setSum (x:xs) (y:ys)
  | x == y = [x,y] ++ setSum xs ys
  | x < y  = x : setSum xs (y:ys)
  | x > y  = y : setSum (x:xs) ys
