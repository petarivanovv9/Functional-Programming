--Task 2

--hlsort ["moo", "bee", "eve", "abracadabra",
--        "abcdefg", "mama", "z"]
-- -> ["abracadabra", "bee", "eve", "abcdefg", "mama", "moo", "z"]

--1) за всеки елемент от списъка строим хистограма
--[ [("m",1), ("o",2)], [("b",1), ("e",2)], ...]

--2) намираме от всяка хистограма кой е най-често срещания елемент,
--т.е максималния

--3)

--[ ("dsada", 'd') ]
--[ "ddsada"]

hlsort m = map ( \ (_,s) -> s) $ quickSort (combineStrMaxChar m (getMaxHisto (makeHist m))) ( \ (c, _) (c2, _) -> c < c2 )

makeHist m = map ( \ x -> histogram x ) m

histogram [] = []
histogram lst = histogramRep(map ( \x -> (x, occurences x lst) ) lst)

histogramRep [] = []
histogramRep (x:xs) = x : histogramRep (filter (/= x) xs)

occurences _ [] = 0
occurences y (x:xs)
  | y == x = 1 + occurences y xs
  | otherwise = occurences y xs


--getMaxHisto hist = map ( \ x -> foldr checkMaxTuple ('a',-1) x ) hist
getMaxHisto hist = map ( \ x -> foldr1 checkMaxTuple x ) hist

checkMaxTuple (char, count) (maxChar, maxCount)
  | count > maxCount = (char, count)
  | count == maxCount && char > maxChar = (char, count)
  | otherwise = (maxChar, maxCount)

combineStrMaxChar m w = zipWith ( \ x (c,_) -> (c, x) ) m w

quickSort [] pred = []
quickSort [x] pred = [x]
quickSort (x:xs) pred = quickSort l1 pred ++ [x] ++ quickSort l2 pred
  where l1 = [ i | i<-xs, pred i x ]
        l2 = [ i | i<-xs, not (pred i x) ]

a = ["moo", "bee", "eve", "abracadabra", "abcdefg", "mama", "z"]
b = getMaxHisto (makeHist a)
bam = combineStrMaxChar a b
hoho = quickSort bam ( \ (c, _) (c2, _) -> c < c2 )

test = map ( \ (_,s) -> s) hoho
