{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

import SearchModule
import Data.List

data Toy = Buzz 
         | Hamm 
         | Rex 
         | Woody  
      deriving (Eq,Ord,Show)

data Pos = L 
         | R                      
      deriving (Eq,Show)      

type StateToys = (Pos,[Toy])
type Move = Either Toy [Toy]


toys :: [Toy]
toys = [Buzz,Hamm,Rex,Woody]

toyToTime :: Toy -> Int
toyToTime Buzz  = 5
toyToTime Woody = 10
toyToTime Rex   = 20
toyToTime Hamm  = 25

totalTime :: [Move] -> Int
totalTime [] = 0
totalTime ((Left x):xs) = toyToTime x + totalTime xs
totalTime ((Right x):xs) = maximum (map toyToTime x) + totalTime xs

instance SearchScheme StateToys Move where
  transition (side,s) = case side of
    L -> [(Right [a,b],(R,delete b toysleft)) | 
              a <- s,
              let toysleft = delete a s, b <- toysleft, 
              a<b] 
    R -> [(Left a,(L,sort (a:(intersect toys s)))) | 
              a <- (toys \\ s) ] 
  criteria (ms,s) 
    | s == (R,[]) && totalTime ms <= 60 = True
    | otherwise = False

solution :: [[Move]]
solution = map fst $ solutions (L,toys)