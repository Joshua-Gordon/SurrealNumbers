module  Number where

import Set

data Number = Num (Set Number) (Set Number)
  deriving Show

lessThanOrEqualTo :: Number -> Number -> Bool
lessThanOrEqualTo (Num xL xR) (Num yL yR) = Set.lessThanOrEqualTo (Num yL yR) xL && Set.lessThanOrEqualToSet yR (Num xL xR)

instance Ord Number where
  x <= y = Number.lessThanOrEqualTo x y

instance Eq Number where
  x == y = x <= y && y <= x


generateNumbers :: Int -> [Number]
generateNumbers 0 = [Num [] []]
generateNumbers x = let numList = generateNumbers $ x-1
                    in intersperseNums numList ++ [Num numList []]
                    where intersperseNums :: [Number] -> [Number]
                          intersperseNums (n:ns) = Num [n] ns : intersperseNums ns
                          intersperseNums n = [Num n []]
