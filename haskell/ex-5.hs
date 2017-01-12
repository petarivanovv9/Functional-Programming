import Data.List -- за функциите за специална подредба minimumBy, maximumBy


--Task 1

--maxDistance [(-1.1, 1), (1.8, 2), (3, 1), (-1, -2)] -> 5

type Point = (Double, Double)

points = [(-1.1, 1), (1.8, 2), (3, 1), (-1, -2)]

--dist :: Point -> Point -> Double
dist (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

--maxDistance :: [Point] -> Double
maxDistance pts = maximum [ dist p1 p2 | p1<-pts, p2<-pts ]


-- бонус: да приемаме и функцията за разстояние като параметър
maxDistanceBy f pts = maximum [ f p1 p2 | p1<-pts, p2<-pts ]
-- тогава maxDistance = maxDistanceBy dist


-- ако искаме да върнем не само най-голямото разстояние,
-- ами и точките, между които е то: изграждаме списък (d,p1,p2)
-- и му взимаме елемента с най-голяма стойност на първата позиция

--maxDistance1 [(-1.1, 1), (1.8, 2), (3, 1), (-1, -2)]
-- -> (5.0,(-1.0,-2.0),(3.0,1.0))

maxDistance1 pts = maximumBy ( \ p@(d1,_,_) q@(d2,_,_) -> compare d1 d2 ) [ (dist p1 p2, p1, p2) | p1<-pts, p2<-pts ]
