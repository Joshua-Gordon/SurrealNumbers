module Operations where

import Number
import Set

{-Three numbers defined for convenience-}
zero :: Number
zero = Num [] []

one :: Number
one = Num [zero] []

negone :: Number
negone = Num [] [zero]

negateNum :: Number -> Number
negateNum (Num [] []) = Num [] []
negateNum (Num xL xR) = Num (negateSet xR) (negateSet xL)

negateSet :: Set Number -> Set Number
negateSet xs = [negateNum x | x <- xs]

addNumToSet :: Number -> Set Number -> Set Number
addNumToSet (Num [] []) ys = ys
addNumToSet x ys = [addNumToNum x y | y <- ys]

multNumToSet :: Number -> Set Number -> Set Number
multNumToSet (Num [] []) _ = []
multNumToSet n ys = [multNumToNum n y | y <- ys]

addSetToNum :: Set Number -> Number -> Set Number
addSetToNum ys (Num [] []) = ys
addSetToNum ys x = [addNumToNum y x | y <- ys]

multSetToNum :: Set Number -> Number -> Set Number
multSetToNum _ (Num [] []) = [Num [] []]
multSetToNum xs n = [multNumToNum x n | x <- xs]

multSetToSet :: Set Number -> Set Number -> Set Number
multSetToSet [] _ = []
multSetToSet _ [] = []
multSetToSet xs ys = [multNumToNum x y | x <- xs, y <- ys]

addSetToSet :: Set Number -> Set Number -> Set Number
addSetToSet xs [] = xs
addSetToSet [] ys = ys
addSetToSet xs ys = [addNumToNum x y | x <- xs, y<-ys]

addNumToNum :: Number -> Number -> Number
addNumToNum (Num [] []) x = x
addNumToNum y (Num [] []) = y
addNumToNum (Num xL xR) (Num yL yR) = Num leftSide rightSide
                                        where leftSide = addSetToNum xL (Num yL yR) `union ` addNumToSet (Num xL xR) yR
                                              rightSide = addSetToNum yL (Num xL xR) `union` addNumToSet (Num yL yR) xR

multNumToNum :: Number -> Number -> Number
multNumToNum (Num [] []) _ = Num [] []
multNumToNum _ (Num [] []) = Num [] []
multNumToNum (Num xL xR) (Num yL yR) = Num (leftSide1 `union` leftSide2) (rightSide1 `union` rightSide2)
                                        where leftSide1 = addSetToSet (multSetToNum xL (Num yL yR)) (addSetToSet (multNumToSet (Num xL xR) yL) (negateSet $ multSetToSet xL yL))
                                              leftSide2 = addSetToSet (multSetToNum xR (Num yL yR)) (addSetToSet (multNumToSet (Num xL xR) yR) (negateSet $ multSetToSet xR yR))
                                              rightSide1 = addSetToSet (multSetToNum xL (Num yL yR)) (addSetToSet (multNumToSet (Num xL xR) yR) (negateSet $ multSetToSet xL yR))
                                              rightSide2 = addSetToSet (multNumToSet (Num xL xR) yL) (addSetToSet (multNumToSet (Num yL yR) xR) (negateSet $ multSetToSet xR yL))
