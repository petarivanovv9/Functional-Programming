-- глава на списък
head' (x:xs) = x
-- head' [2,3,4] => 2

-- опашка на списък
tail' (x:xs) = xs
-- tail' [2,3,4] => [3,4]

--  дали елемент е член на списък
elem' x [] = False
elem' x (y:ys) = x==y || elem' x ys

-- сума от елементите на списък
sum' [] = 0
sum' (y:ys) =  y + sum' ys

countOccurences x [] = 0
countOccurences x (y:ys) = (if x==y then 1 else 0) + countOccurences x ys

-- ++ - конкатенира 2 списъка
-- [1,2,3] ++ [5] => [1,2,3,5]

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- обединение, сечение, разлика на 2 списъка
-- без повторение
-- x `elem` l <=> elem' x l
union [] l = l
union (x:xs) l = if x `elem'` l
  then xs `union` l
  else x:xs `union` l
  -- else [x]++


-- функции от по-висок ред
-- let x = ['a'..'z']
-- x!!2 => 'c'

-- 2^2 => 4

allEvens l = filter even l

square' l = map ( \x -> x*x ) l

filterMatrix p m = map ( \x -> filter p x ) m

getColumn idx m = map ( \row -> row!!idx ) m

getMainDiagonal m = map ( \idx -> (m!!idx)!!idx ) [0..(length m - 1)]

transpose m = map ( \idx -> getColumn idx m ) [0..(length (m!!0) - 1)]

func array = [a | a<-array, a>5, a<8]

-- quick sort
quickSort [] = []
quickSort (x:xs) =
  let smaller = quickSort [a | a<-xs, a<=x]
      bigger  = quickSort [a | a<-xs, a>x]
  in smaller ++ [x] ++ bigger
