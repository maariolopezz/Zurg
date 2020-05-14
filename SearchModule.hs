{-#LANGUAGE MultiParamTypeClasses #-}

module SearchModule where

class SearchScheme s m where
  transition       :: s -> [(m,s)]
  searchSpace      :: s -> [([m],s)]
  criteria         :: ([m],s) -> Bool
  solutions        :: s -> [([m],s)]

  searchSpace s = step ++ expand step where
                    step = [([m],t) | 
                      (m,t) <- transition s]
                    expand ss = [ (ms++ns,t) | 
                      (ms,s) <- ss, 
                      (ns,t) <- searchSpace s ]  
  solutions s = filter criteria $ searchSpace s