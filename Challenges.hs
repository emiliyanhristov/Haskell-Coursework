{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) Emiliyan Hristov, University of Southampton 2021
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (Atoms,Interactions,Pos,EdgePos,Side(..),Marking(..),
                   LamExpr(..),LetExpr(..),CLExpr(..),
                   calcBBInteractions,
                   solveBB,
                   prettyPrint,
                   parseLet,
                   clTransform,
                   innerRedn1,outerRedn1,innerCLRedn1,outerCLRedn1,compareInnerOuter
                   )
where


-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
-- Your other imports here
import Data.Function

instance NFData CLExpr
instance NFData LetExpr
instance NFData LamExpr
instance NFData Marking
instance NFData Side

-- Challenge 1
-- Calculate Interactions in the Black Box

type Atoms = [ Pos ]
type Interactions = [  ( EdgePos , Marking )  ] 
type Pos = (Int, Int)   -- top left is (1,1) , bottom right is (N,N) where N is size of grid
type EdgePos = ( Side , Int ) -- int range is 1 to N where N is size of grid

data Side = North | East | South | West
            deriving (Show, Eq, Ord, Generic)

data Marking =  Absorb | Reflect | Path EdgePos
                deriving (Show, Eq, Generic)


calcBBInteractions :: Int -> Atoms -> Interactions
calcBBInteractions n as | n <= 0 = []
                        | atomsHelper n as == False  = []
                        | otherwise = calcBBInteractionsHelper (rayPositions n 1 North) n as

{- Helper method that finds the Interacton of an EdgePosition in a list of Edgepositions -}
calcBBInteractionsHelper :: [EdgePos] -> Int -> Atoms -> Interactions
calcBBInteractionsHelper [] n as = []
calcBBInteractionsHelper (l:ls) n as | fst l == North = columnTrackInc l (positionTransformer l n) n as : calcBBInteractionsHelper ls n as
                                     | fst l == East = rowTrackDec l (positionTransformer l n) n as : calcBBInteractionsHelper ls n as
                                     | fst l == South = columnTrackDec l (positionTransformer l n) n as : calcBBInteractionsHelper ls n as
                                     | otherwise = rowTrackInc l (positionTransformer l n) n as : calcBBInteractionsHelper ls n as

{- Method that decides whether an atom is outside of the box -}
atomsHelper :: Int -> Atoms -> Bool
atomsHelper n [] = True
atomsHelper n (a:as) | fst a < 1 || fst a > n = False
                     | snd a < 1 || snd a > n = False
                     | otherwise = atomsHelper n as

{- Method that takes an EdgePosition and transforms it into a Position of the type (Int, Int) -}
positionTransformer :: EdgePos -> Int -> Pos
positionTransformer x n | fst x == North = (snd x, 0)
                        | fst x == East = (n+1, snd x)
                        | fst x == South = (snd x, n+1)
                        | otherwise = (0, snd x)

{- Method that takes the size of the board and creates an EdgePosition from every possible angle -}
rayPositions :: Int -> Int -> Side -> [EdgePos]
rayPositions n c s | c <= n = (s, c) : rayPositions n (c+1) s
                   | c > n && s == North = rayPositions n 1 East
                   | c > n && s == East = rayPositions n 1 South
                   | c > n && s == South = rayPositions n 1 West
                   | otherwise = []

{- Four methods that decide the path of the ray from an Edgeposition:
   (1) Each method remembers the starting edge position and follows the movement of the ray
   (2) Depending whether the ray is going North, East, South or West a different method is called
   (3) When a method is called it does the following:
       (i) Checks whether the ray have exited the box and if so, checks if it have Reflected or tells us the position of exiting
       (ii) Checks whether the next thing in it's path is atom and if so, Absorbs
       (iii) Checks whether it has atoms to the front left or right and decides what direction it should take
       (iv) If all else fails it just extends it's path by 1 in the corresponding direction
-}
columnTrackInc :: EdgePos -> Pos -> Int -> Atoms -> (EdgePos, Marking)
columnTrackInc ep pos n as | snd pos > n && positionTransformer ep n == pos = (ep, Reflect)
                           | snd pos > n = (ep, Path (South, fst pos))
                           | (fst pos, (snd pos)+1) `elem` as = (ep, Absorb)
                           | snd pos == 0 && (((fst pos)-1, (snd pos)+1) `elem` as || ((fst pos)+1, (snd pos)+1) `elem` as) = (ep, Reflect)
                           | ((fst pos)-1, (snd pos)+1) `elem` as = rowTrackInc ep pos n as
                           | ((fst pos)+1, (snd pos)+1) `elem` as = rowTrackDec ep pos n as
                           | otherwise = columnTrackInc ep (fst pos, (snd pos)+1) n as

columnTrackDec :: EdgePos -> Pos -> Int -> Atoms -> (EdgePos, Marking)
columnTrackDec ep pos n as | snd pos < 1 && positionTransformer ep n == pos = (ep, Reflect)
                           | snd pos < 1 = (ep, Path (North, fst pos))
                           | (fst pos, (snd pos)-1) `elem` as = (ep, Absorb)
                           | snd pos == n+1 && (((fst pos)-1, (snd pos)-1) `elem` as || ((fst pos)+1, (snd pos)-1) `elem` as) = (ep, Reflect)
                           | ((fst pos)-1, (snd pos)-1) `elem` as = rowTrackInc ep pos n as
                           | ((fst pos)+1, (snd pos)-1) `elem` as = rowTrackDec ep pos n as
                           | otherwise = columnTrackDec ep (fst pos, (snd pos)-1) n as

rowTrackInc :: EdgePos -> Pos -> Int -> Atoms -> (EdgePos, Marking)
rowTrackInc ep pos n as | fst pos > n && positionTransformer ep n == pos = (ep, Reflect)
                        | fst pos > n = (ep, Path (East, snd pos))
                        | ((fst pos)+1, snd pos) `elem` as = (ep, Absorb)
                        | fst pos == 0 && (((fst pos)+1, (snd pos)-1) `elem` as || ((fst pos)+1, (snd pos)+1) `elem` as) = (ep, Reflect)
                        | ((fst pos)+1, (snd pos)+1) `elem` as = columnTrackDec ep pos n as
                        | ((fst pos)+1, (snd pos)-1) `elem` as = columnTrackInc ep pos n as
                        | otherwise = rowTrackInc ep ((fst pos)+1, snd pos) n as

rowTrackDec :: EdgePos -> Pos -> Int -> Atoms -> (EdgePos, Marking)
rowTrackDec ep pos n as | fst pos < 1 && positionTransformer ep n == pos = (ep, Reflect)
                        | fst pos < 1 = (ep, Path (West, snd pos))
                        | ((fst pos)-1, snd pos) `elem` as = (ep, Absorb)
                        | fst pos == n+1 && (((fst pos)-1, (snd pos)-1) `elem` as || ((fst pos)-1, (snd pos)+1) `elem` as) = (ep, Reflect)
                        | ((fst pos)-1, (snd pos)+1) `elem` as = columnTrackDec ep pos n as
                        | ((fst pos)-1, (snd pos)-1) `elem` as = columnTrackInc ep pos n as
                        | otherwise = rowTrackDec ep ((fst pos)-1, snd pos) n as

-- Challenge 2
{- 
   Solve function that finds a n number of atoms on a black box from the interactions.
   Finding as many atoms as possible with helper methods and brute forcing the remaining ones.
   The function may not work if two of the atoms are placed in two opposite corners of the board and the rest are in the middle.
   The function may exceed the time limit if there are three unfound atoms and the board is 10x10
-}
solveBB :: Int -> Interactions -> Atoms 
solveBB n ints | n - length (atoms ints) == 0 = atoms ints
               | n - length (atoms ints) == 1 = findSolution (boardLength ints) (combineAtoms (atoms ints) (findOneAtom (boardLength ints) 1 1)) ints
               | n - length (atoms ints) == 2 = findSolution (boardLength ints) (combineAtoms (atoms ints) (findTwoAtoms (boardLength ints) 1 1 1 1)) ints
               | n - length (atoms ints) == 3 = findSolution (boardLength ints) (combineAtoms (atoms ints) (findThreeAtoms (boardLength ints) 1 1 1 1 1 1)) ints
               | otherwise = []


-- Function that removes the duplicate atoms and the (0,0) atom (which means undecidable) from the list of algorithmically found atoms
atoms :: Interactions -> Atoms
atoms ints = atomsFixer $ atomsOrderer2 $ atomsFixer $ atomsOrderer $ combineAtomSearches ints

-- Function that finds the board length
boardLength :: Interactions -> Int
boardLength ints = findLastSpace (northInteractions ints) 0

-- Function that compares a possible list of atoms to the list of Interactions, if they match a solution is found
findSolution :: Int -> [Atoms] -> Interactions -> Atoms
findSolution n [] ints = []
findSolution n (a:ats) ints | calcBBInteractions n a == ints = a
                            | otherwise = findSolution n ats ints

-- The next three functions generate a list of one, two or three atoms that represents the list of atoms that have not been found
findOneAtom :: Int -> Int -> Int -> [Atoms]
findOneAtom n l w | w <= n = [(l,w)] : findOneAtom n l (w+1)
                  | l < n = findOneAtom n (l+1) 1
                  | otherwise = []

findTwoAtoms :: Int -> Int -> Int -> Int -> Int -> [Atoms]
findTwoAtoms n l1 w1 l2 w2 | w2 <= n = [(l1,w1),(l2,w2)] : findTwoAtoms n l1 w1 l2 (w2+1)
                           | l2 < n = findTwoAtoms n l1 w1 (l2+1) 1
                           | w1 < n = findTwoAtoms n l1 (w1+1) 1 1
                           | l1 < n = findTwoAtoms n (l1+1) 1 1 1
                           | otherwise = []

findThreeAtoms :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Atoms]
findThreeAtoms n l1 w1 l2 w2 l3 w3 | w3<= n = [(l1,w1),(l2,w2),(l3,w3)] : findThreeAtoms n l1 w1 l2 w2 l3 (w3+1)
                                   | l3 < n = findThreeAtoms n l1 w1 l2 w2 (l3+1) 1
                                   | w2 < n = findThreeAtoms n l1 w1 l2 (w2+1) 1 1
                                   | l2 < n = findThreeAtoms n l1 w1 (l2+1) 1 1 1
                                   | w1 < n = findThreeAtoms n l1 (w1+1) 1 1 1 1
                                   | l1 < n = findThreeAtoms n (l1+1) 1 1 1 1 1
                                   | otherwise = []

-- Function that combines our list of found atoms with the rest of possible atom positions
combineAtoms :: Atoms -> [Atoms] -> [Atoms]
combineAtoms at [] = []
combineAtoms at (x:ats) = (at++x) : combineAtoms at ats

-- Function that finds a list of possible atoms using an algorithm explained below
combineAtomSearches :: Interactions -> Atoms
combineAtomSearches ints = [findFirstAtomNorth north west] ++ [findFirstAtomEast east northR] ++ [findFirstAtomSouth south westR] 
                        ++ [findFirstAtomWest west north] ++ [findFirstAtomNorthReverse northR east] ++ [findFirstAtomEastReverse eastR southR] 
                        ++ [findFirstAtomSouthReverse southR eastR] ++ [findFirstAtomWestReverse westR south]
                  where north = orderInteractions (northInteractions ints) 1 (findLastSpace (northInteractions ints) 0) []
                        east = orderInteractions (eastInteractions ints) 1 (findLastSpace (eastInteractions ints) 0) []
                        south = orderInteractions (southInteractions ints) 1 (findLastSpace (southInteractions ints) 0) []
                        west = orderInteractions (westInteractions ints) 1 (findLastSpace (westInteractions ints) 0) []
                        northR = reverse $ orderInteractions (northInteractions ints) 1 (findLastSpace (northInteractions ints) 0) []
                        eastR = reverse $ orderInteractions (eastInteractions ints) 1 (findLastSpace (eastInteractions ints) 0) []
                        southR = reverse $ orderInteractions (southInteractions ints) 1 (findLastSpace (southInteractions ints) 0) []
                        westR = reverse $ orderInteractions (westInteractions ints) 1 (findLastSpace (westInteractions ints) 0) []

-- Function that removes the duplicating atoms from the list of possible atoms
atomsFixer :: Atoms -> Atoms
atomsFixer [x] = [x]
atomsFixer (x:y:ats) | x == y = atomsFixer (x:ats)
                     | x == (0,0) = atomsFixer (y:ats)
                     | otherwise = x : atomsFixer (y:ats)

-- Helper function that orders the different atoms in a list
atomsOrderer :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
atomsOrderer = sortBy (compare `on` fst)

-- Helper function that orders the different atoms in a list
atomsOrderer2 :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
atomsOrderer2 = sortBy (compare `on` snd)

-- The next four functions separate the interactions into a different groups including the interactions from North, East, South, West
northInteractions :: Interactions -> Interactions
northInteractions [] = []
northInteractions (i:ints) | fst (fst i) == North = i : northInteractions ints
                           | otherwise = northInteractions ints

eastInteractions :: Interactions -> Interactions
eastInteractions [] = []
eastInteractions (i:ints) | fst (fst i) == East = i : eastInteractions ints
                          | otherwise = eastInteractions ints

southInteractions :: Interactions -> Interactions
southInteractions [] = []
southInteractions (i:ints) | fst (fst i) == South = i : southInteractions ints
                           | otherwise = southInteractions ints

westInteractions :: Interactions -> Interactions
westInteractions [] = []
westInteractions (i:ints) | fst (fst i) == West = i : westInteractions ints
                          | otherwise = westInteractions ints

-- Function that orders the interactions if they are unordered
orderInteractions :: Interactions -> Int -> Int -> Interactions -> Interactions
orderInteractions [] c l e | c > l = []
                           | otherwise = orderInteractions e c l []
orderInteractions (i:ints) c l e | snd (fst i) == c = i : orderInteractions ints (c+1) l e
                                 | otherwise = orderInteractions ints c l (e++[i])

--Function that checks whether an interaction absorbs
checkAbsorb :: (EdgePos,Marking) -> Bool
checkAbsorb i | snd i == Absorb = True
              | otherwise = False

-- Function that checks whether an edgepost will absorb by a list of interactions
checkAbsorbByPos :: EdgePos -> Interactions -> Bool
checkAbsorbByPos ep [] = False
checkAbsorbByPos ep (i:ints) | ep == fst i && snd i == Absorb = True
                             | otherwise = checkAbsorbByPos ep ints

-- Function that checks whether an intercation reflects
checkReflect :: (EdgePos,Marking) -> Bool
checkReflect i | snd i == Reflect = True
               | otherwise = False        

-- Function that finds the possition of the first absorb in a list of interactions
findFirstAbsorb :: Interactions -> Int
findFirstAbsorb [] = 0
findFirstAbsorb (i:ints) | snd i == Absorb = snd(fst i)
                         | otherwise = findFirstAbsorb ints

-- Function that finds the possition of the first reflect in a list of interactions
findFirstReflect :: Interactions -> Int
findFirstReflect [] = 0
findFirstReflect (i:ints) | snd i == Reflect = snd(fst i)
                          | otherwise = findFirstReflect ints

-- Function that finds the number of the last row/column in a list of interactions
findLastSpace :: Interactions -> Int -> Int
findLastSpace [] c = c
findLastSpace (i:ints) c | snd(fst i) > c = findLastSpace ints (snd(fst i))
                         | otherwise = findLastSpace ints c

-- Function that checks whether a given interaction has a reverse path
findReversePath :: (EdgePos, Marking) -> Interactions -> Int
findReversePath _ [] = 0
findReversePath x (y:ps2) | snd x == Path(fst y) && snd y == Path(fst x) = snd(fst y)
                          | otherwise = findReversePath x ps2

{-
  Functions that find the first atom from each side and its reverse
  1. If the interaction is to the opposite side and the same position it means that it is a straight line and goes to the next interaction
  2. If it is reflection returnt the next position on the same row or column
  3. If it absorbs and there are atoms on the 3rd position of the coresponding row or column it returns (0,0), cause it is hard to identify where the atom is
  4. If it absorbs and there are multiple consequent atoms to find the position we will look for reflect but if there is a hole inbetween the consequent
  atoms, we take the atom before the hole
  5. If it absorbs, it takes the atom on which it first reflects
  6. If there is a case where there is a basket forming with the wall (e.g. from north the atoms are positioned (2,2),(1,3)) it will take the upper atom
  7. If all else fails and there is a Path we look for the reverse path to find the positon of the atom
  If there are not atoms on the board, we just get an (0,0) atom  
-}
findFirstAtomNorth :: Interactions -> Interactions -> Pos
findFirstAtomNorth [] intsW = (0,0)
findFirstAtomNorth (x:intsN) intsW | snd x == Path(South, snd(fst x)) = findFirstAtomNorth intsN intsW
                                   | checkReflect x = (snd(fst x)+1, 1)
                                   | checkAbsorb x && findFirstReflect intsN == 2 && findFirstReflect intsW == 2 = (0,0)
                                   | checkAbsorb x && checkAbsorbByPos (West, (findFirstReflect intsW)-1) intsW && (checkAbsorbByPos (West, (findFirstReflect intsW)+1) intsW) /= True = (1, findFirstReflect intsW - 1)
                                   | checkAbsorb x = (1, findFirstReflect intsW + 1)
                                   | snd x /= Path(South, snd(fst x)) && snd(fst x) == 1 && findFirstReflect intsW < findFirstAbsorb intsW && findFirstReflect intsW /= 0 = (snd(fst x)+1, findFirstReflect intsW)
                                   | otherwise = (snd(fst x)+1, (findReversePath x intsW)+1)

findFirstAtomEast :: Interactions -> Interactions -> Pos
findFirstAtomEast [] intsNR = (0,0)
findFirstAtomEast (x:intsE) intsNR | snd x == Path(West, snd(fst x)) = findFirstAtomEast intsE intsNR
                                   | checkReflect x = (findLastSpace intsNR 0, snd(fst x)+1)
                                   | checkAbsorb x && findFirstReflect intsE == 2 && findFirstReflect intsNR == (findLastSpace intsNR 0)-1 = (0,0)
                                   | checkAbsorb x && checkAbsorbByPos (North, (findFirstReflect intsNR)-1) intsNR && (checkAbsorbByPos (North, (findFirstReflect intsNR)+1) intsNR) /= True = (findFirstReflect intsNR - 1, 1)
                                   | checkAbsorb x = (findFirstReflect intsNR - 1, 1)
                                   | snd x /= Path(West, snd(fst x)) && snd(fst x) == 1 && findFirstReflect intsNR > findFirstAbsorb intsNR && findFirstReflect intsNR /= 0 = (findFirstReflect intsNR, snd(fst x)+1)
                                   | otherwise = ((findReversePath x intsNR)-1, snd(fst x)+1)

findFirstAtomSouth :: Interactions -> Interactions -> Pos
findFirstAtomSouth [] intsWR = (0,0)
findFirstAtomSouth (x:intsS) intsWR | snd x == Path(North, snd(fst x)) = findFirstAtomSouth intsS intsWR
                                    | checkReflect x = (snd(fst x)+1, findLastSpace (x:intsS) 0)
                                    | checkAbsorb x && findFirstReflect intsS == 2 && findFirstReflect intsWR == (findLastSpace intsWR 0)-1 = (0,0)
                                    | checkAbsorb x && checkAbsorbByPos (West, (findFirstReflect intsWR)-1) intsWR && (checkAbsorbByPos (West, (findFirstReflect intsWR)+1) intsWR) /= True = (1, findFirstReflect intsWR - 1)
                                    | checkAbsorb x = (1, findFirstReflect intsWR - 1)
                                    | snd x /= Path(North, snd(fst x)) && snd(fst x) == 1 && findFirstReflect intsWR > findFirstAbsorb intsWR && findFirstReflect intsWR /= 0 = (snd(fst x)+1, findFirstReflect intsWR)
                                    | otherwise = (snd(fst x)+1, (findReversePath x intsWR)-1)

findFirstAtomWest :: Interactions -> Interactions -> Pos
findFirstAtomWest [] intsN = (0,0)
findFirstAtomWest (x:intsW) intsN | snd x == Path(East, snd(fst x)) = findFirstAtomWest intsW intsN
                                  | checkReflect x = (1, snd(fst x)+1)
                                  | checkAbsorb x && findFirstReflect intsW == 2 && findFirstReflect intsN == 2 = (0,0)
                                  | checkAbsorb x && checkAbsorbByPos (North, (findFirstReflect intsN)-1) intsN && (checkAbsorbByPos (North, (findFirstReflect intsN)+1) intsN) /= True = (findFirstReflect intsN - 1, 1)
                                  | checkAbsorb x = (findFirstReflect intsN + 1, 1)
                                  | snd x /= Path(East, snd(fst x)) && snd(fst x) == 1 && findFirstReflect intsN < findFirstAbsorb intsN && findFirstReflect intsN /= 0 = (findFirstReflect intsN, snd(fst x)+1)
                                  | otherwise = ((findReversePath x intsN)+1, snd(fst x)+1)

findFirstAtomNorthReverse :: Interactions -> Interactions -> Pos
findFirstAtomNorthReverse [] intsE = (0,0)
findFirstAtomNorthReverse (x:intsNR) intsE | snd x == Path(South, snd(fst x)) = findFirstAtomNorthReverse intsNR intsE
                                           | checkReflect x = (snd(fst x)-1, 1)
                                           | checkAbsorb x && findFirstReflect intsNR == (findLastSpace intsE 0)-1 && findFirstReflect intsE == 2 = (0,0)
                                           | checkAbsorb x && checkAbsorbByPos (East, (findFirstReflect intsE)-1) intsE && (checkAbsorbByPos (East, (findFirstReflect intsE)+1) intsE) /= True = (findLastSpace intsE 0, findFirstReflect intsE - 1)
                                           | checkAbsorb x = (findLastSpace intsE 0, findFirstReflect intsE + 1)
                                           | snd x /= Path(South, snd(fst x)) && snd(fst x) == (findLastSpace intsE 0) && findFirstReflect intsE < findFirstAbsorb intsE && findFirstReflect intsE /= 0 = (snd(fst x)-1, findFirstReflect intsE)
                                           | otherwise = (snd(fst x)-1, (findReversePath x intsE)+1)

findFirstAtomEastReverse :: Interactions -> Interactions -> Pos
findFirstAtomEastReverse [] intsSR = (0,0)
findFirstAtomEastReverse (x:intsER) intsSR | snd x == Path(West, snd(fst x)) = findFirstAtomEastReverse intsER intsSR
                                           | checkReflect x = (findLastSpace intsSR 0, snd(fst x)-1)
                                           | checkAbsorb x && findFirstReflect intsER == (findLastSpace intsSR 0)-1 && findFirstReflect intsSR == (findLastSpace intsSR 0)-1 = (0,0)
                                           | checkAbsorb x && checkAbsorbByPos (South, (findFirstReflect intsSR)+1) intsSR && (checkAbsorbByPos (South, (findFirstReflect intsSR)-1) intsSR) /= True = (findFirstReflect intsSR + 1, findLastSpace intsSR 0)
                                           | checkAbsorb x = (findFirstReflect intsSR - 1, findLastSpace intsSR 0)
                                           | snd x /= Path(West, snd(fst x)) && snd(fst x) == findLastSpace intsSR 0 && findFirstReflect intsSR > findFirstAbsorb intsSR && findFirstReflect intsSR /= 0 = (findFirstReflect intsSR, snd(fst x)-1)
                                           | otherwise = ((findReversePath x intsSR)-1, snd(fst x)-1)

findFirstAtomSouthReverse :: Interactions -> Interactions -> Pos
findFirstAtomSouthReverse [] intsER = (0,0)
findFirstAtomSouthReverse (x:intsSR) intsER | snd x == Path(North, snd(fst x)) = findFirstAtomSouthReverse intsSR intsER
                                            | checkReflect x = (snd(fst x)-1, findLastSpace intsER 0)
                                            | checkAbsorb x && findFirstReflect intsSR == (findLastSpace intsER 0)-1 && findFirstReflect intsER == (findLastSpace intsER 0)-1 = (0,0)
                                            | checkAbsorb x && checkAbsorbByPos (East, (findFirstReflect intsER)+1) intsER && (checkAbsorbByPos (East, (findFirstReflect intsER)-1) intsER) /= True = (findLastSpace intsER 0, findFirstReflect intsER + 1)
                                            | checkAbsorb x = (findLastSpace intsER 0, findFirstReflect intsER - 1)
                                            | snd x /= Path(North, snd(fst x)) && snd(fst x) == findLastSpace intsER 0 && findFirstReflect intsER > findFirstAbsorb intsER && findFirstReflect intsER /= 0 = (snd(fst x)-1, findFirstReflect intsER)
                                            | otherwise = (snd(fst x)-1, (findReversePath x intsER)-1) 

findFirstAtomWestReverse :: Interactions -> Interactions -> Pos
findFirstAtomWestReverse [] intsS = (0,0)
findFirstAtomWestReverse (x:intsWR) intsS | snd x == Path(East, snd(fst x)) = findFirstAtomWestReverse intsWR intsS
                                          | checkReflect x = (1, snd(fst x)-1)
                                          | checkAbsorb x && findFirstReflect intsWR == (findLastSpace intsS 0)-1 && findFirstReflect intsS == 2 = (0,0)
                                          | checkAbsorb x && checkAbsorbByPos (South, (findFirstReflect intsS)-1) intsS && (checkAbsorbByPos (South, (findFirstReflect intsS)+1) intsS) /= True = (findFirstReflect intsS - 1, findLastSpace intsS 0)
                                          | checkAbsorb x = (findFirstReflect intsS + 1, findLastSpace intsS 0)
                                          | snd x /= Path(East, snd(fst x)) && snd(fst x) == findLastSpace intsS 0 && findFirstReflect intsS < findFirstAbsorb intsS && findFirstReflect intsS /= 0 = (findFirstReflect intsS, snd(fst x)-1)
                                          | otherwise = ((findReversePath x intsS)+1, snd(fst x)-1)

-- Challenge 3
-- Pretty Printing Lambda with Scott Numerals

data LamExpr =  LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int 
                deriving (Eq, Show, Read, Generic)

-- Pretty Printing Lambda Expresions with Scott Numerals using helper functions
prettyPrint :: LamExpr -> String
prettyPrint expr = fixNumber (scottEncFixer (scottEncOne (scottEncZero (expression expr []) []) [] 0 0) []) []

-- Turning an expression into string with pattern matching
expression :: LamExpr -> String -> String
expression (LamVar i) e = e ++ "x" ++ show i
expression (LamAbs i r) e = expression r (e ++ "\\x" ++ show i ++ " -> ")
expression (LamApp r@(LamAbs _ _) y) e = "(" ++ expression r e ++ ") " ++ expression y []
expression (LamApp r y@(LamApp _ _) ) e =  expression r e ++ " (" ++ expression y [] ++ ")"
expression (LamApp x y) e = expression x e ++ " " ++ expression y []

-- Function that removes the brackets from the LamApp on a Number
fixNumber :: String -> String -> String
fixNumber [] e = e
fixNumber (s:st) e | s == '(' && takeNum st /= [] = fixNumber (drop ((length $ takeNum st)+2) (s:st)) (e ++ takeNum st)
                   | otherwise = fixNumber st (e ++ [s])

-- Functions that decides which part of a string is a number and prints it
takeNum :: String -> String
takeNum [] = []
takeNum (s:st) | s == '0' || s == '1' || s == '2' || s == '3' || s == '4' || s == '5' || s == '6' || s == '7' || s == '8' || s == '9' = s : takeNum st
               | otherwise = []

{- The three following functions take an expression that was turned into a string and substitute every part that represents zero.
   1. Finds the first lambda symbol and checks if what follows is a zero with the helper function:
    (i) If it is possible divides the string into three sub-parts which represent the different \\x's, if not returns the same string
    (ii) After the division compares the three parts to see if it matches the pattern for zero and if it does returns "0" with the rest of the string
    (iii) If it doesn't, returns the same string
   2. If it is zero, the function replaces a part of the string with length of the expression that was checked with "0" and continues
    (i) The length of the expression is found similarly but a counterHelper function where a counter is updated for each character
   3. If it is not zero it just continues until the end of the string is reached
-}
scottEncZero :: String -> String -> String
scottEncZero [] rm = rm
scottEncZero (s:st) rm | s == '\\' && (take 1 (scottEncHelperZero (s:st) [] [] [] [] 0)) == "0" = scottEncZero (drop (scottEncHelperCounterZero (s:st) [] [] [] [] 0 0) (s:st)) (rm++"0")
                       | otherwise = scottEncZero st (rm++[s])

scottEncHelperZero :: String -> String -> String -> String -> String -> Int -> String
scottEncHelperZero [] s1 s2 s3 e c | s1 == s3 && s1 /= [] = "0"
                                   | otherwise = e
scottEncHelperZero (s:st) s1 s2 s3 e c | s1 == s3 && s2 /= s1 && s1 /= [] = "0" ++ (s:st) 
                                       | s == '\\' || s == 'x' || s == ' ' || s == '-' || s == '>' = scottEncHelperZero st s1 s2 s3 (e ++ [s]) (c+1)
                                       | s1 == [] && c == 2 = scottEncHelperZero (drop (length $ takeNum (s:st)) (s:st)) (takeNum (s:st)) s2 s3 (e++takeNum (s:st)) c
                                       | s2 == [] && c == 8 = scottEncHelperZero (drop (length $ takeNum (s:st)) (s:st)) s1 (takeNum (s:st)) s3 (e++takeNum (s:st)) c
                                       | s3 == [] && c == 13 = scottEncHelperZero (drop (length $ takeNum (s:st)) (s:st)) s1 s2 (takeNum (s:st)) (e++takeNum (s:st)) c
                                       | otherwise = e ++ (s:st)

scottEncHelperCounterZero :: String -> String -> String -> String -> String -> Int -> Int -> Int
scottEncHelperCounterZero [] s1 s2 s3 e c hc | s1 == s3 && s1 /= [] = hc
                                             | otherwise = 0
scottEncHelperCounterZero (s:st) s1 s2 s3 e c hc | s1 == s3 && s2 /= s1 && s1 /= [] = hc
                                                 | s == '\\' || s == 'x' || s == ' ' || s == '-' || s == '>' = scottEncHelperCounterZero st s1 s2 s3 (e ++ [s]) (c+1) (hc+1)
                                                 | s1 == [] && c == 2 = scottEncHelperCounterZero (drop (length $ takeNum (s:st)) (s:st)) (takeNum (s:st)) s2 s3 (e++takeNum (s:st)) c (hc+(length $ takeNum (s:st)))
                                                 | s2 == [] && c == 8 = scottEncHelperCounterZero (drop (length $ takeNum (s:st)) (s:st)) s1 (takeNum (s:st)) s3 (e++takeNum (s:st)) c (hc+(length $ takeNum (s:st)))
                                                 | s3 == [] && c == 13 = scottEncHelperCounterZero (drop (length $ takeNum (s:st)) (s:st)) s1 s2 (takeNum (s:st)) (e++takeNum (s:st)) c (hc+(length $ takeNum (s:st)))
                                                 | otherwise = 0

{- The next three functions are similar to the zero functions with the differance that every time a successor of zero (one) is found
   the function checks the string again in order to check, whather that successor has successor of its own, meaning the number is two, three
   or heigher. For every successor the part of the string representing it is replaced by "1".
-}
scottEncOne :: String -> String -> Int -> Int -> String
scottEncOne [] rm c hc | c /= hc = scottEncOne rm [] 0 0
                       | otherwise = rm
scottEncOne (s:st) rm c hc | s == '\\' && (take 1 (scottEncHelperOne (s:st) [] [] [] [] 0)) == "1" = scottEncOne (drop (scottEncHelperCounterOne (s:st) [] [] [] [] 0 0) (s:st)) (rm++"1") (c+1) hc
                           | otherwise = scottEncOne st (rm++[s]) c hc

scottEncHelperOne :: String -> String -> String -> String -> String -> Int -> String
scottEncHelperOne [] s1 s2 s3 e c = e
scottEncHelperOne (s:st) s1 s2 s3 e c | s2 == s3 && s2 /= [] && (((s:st) !! 1) == '0' && ((s:st) !! 0 /= 'x') || ((s:st) !! 1) == '1' && ((s:st) !! 0) /= 'x') = "1" ++ (s:st) 
                                      | s == '\\' || s == 'x' || s == ' ' || s == '-' || s == '>' = scottEncHelperOne st s1 s2 s3 (e ++ [s]) (c+1)
                                      | s1 == [] && c == 2 = scottEncHelperOne (drop (length $ takeNum (s:st)) (s:st)) (takeNum (s:st)) s2 s3 (e++takeNum (s:st)) c
                                      | s2 == [] && c == 8 = scottEncHelperOne (drop (length $ takeNum (s:st)) (s:st)) s1 (takeNum (s:st)) s3 (e++takeNum (s:st)) c
                                      | s3 == [] && c == 13 = scottEncHelperOne (drop (length $ takeNum (s:st)) (s:st)) s1 s2 (takeNum (s:st)) (e++takeNum (s:st)) c
                                      | otherwise = e ++ (s:st)

scottEncHelperCounterOne :: String -> String -> String -> String -> String -> Int -> Int -> Int
scottEncHelperCounterOne (s:st) s1 s2 s3 e c hc | s2 == s3 && s2 /= [] && (((s:st) !! 1) == '0' && ((s:st) !! 0 /= 'x') || ((s:st) !! 1) == '1' && ((s:st) !! 0) /= 'x') = hc
                                                | s == '\\' || s == 'x' || s == ' ' || s == '-' || s == '>' = scottEncHelperCounterOne st s1 s2 s3 (e ++ [s]) (c+1) (hc+1)
                                                | s1 == [] && c == 2 = scottEncHelperCounterOne (drop (length $ takeNum (s:st)) (s:st)) (takeNum (s:st)) s2 s3 (e++takeNum (s:st)) c (hc+(length $ takeNum (s:st)))
                                                | s2 == [] && c == 8 = scottEncHelperCounterOne (drop (length $ takeNum (s:st)) (s:st)) s1 (takeNum (s:st)) s3 (e++takeNum (s:st)) c (hc+(length $ takeNum (s:st)))
                                                | s3 == [] && c == 13 = scottEncHelperCounterOne (drop (length $ takeNum (s:st)) (s:st)) s1 s2 (takeNum (s:st)) (e++takeNum (s:st)) c (hc+(length $ takeNum (s:st)))
                                                | otherwise = 0

-- The Fixer function replaces every successor number with the corresponding number, e.g. "1 1 0" becomes "2" by keeping the count ofevery "1"
-- into a counter until a "0" is met.
scottEncFixer :: String -> String -> String
scottEncFixer [] e = e
scottEncFixer (s:st) e | s == '1' && (length e == 0 || (e !! (length e - 1)) == ' ' || (e !! (length e - 1)) == '(') = scottEncFixer (drop (scottEncAdderCounter (s:st) 0) (s:st)) (e ++ scottEncAdder (s:st) 0)
                       | otherwise = scottEncFixer st (e++[s])

scottEncAdder :: String -> Int -> String
scottEncAdder (s:st) c | s == '0' = show c
                       | s == '1' = scottEncAdder st (c+1)
                       | otherwise = scottEncAdder st c

scottEncAdderCounter :: String -> Int -> Int
scottEncAdderCounter (s:st) hc | s == '0' = hc+1
                               | s == '1' = scottEncAdderCounter st (hc+1)
                               | otherwise = scottEncAdderCounter st (hc+1)

-- Challenge 4 
-- Some of the code for Challenge 4 was inspired and extended from (https://github.com/chrisw53/COMP2209-CW2/blob/master/Challenges.hs)
-- Parsing Let Expressions

data LetExpr =  LetApp LetExpr LetExpr | LetDef [([Int], LetExpr)] LetExpr | 
                LetFun Int | LetVar Int | LetNum Int
                deriving (Show, Eq, Generic) 

-- Parsing depending on the type of the expression if possible, otherwise return Nothing
parseLet :: String -> Maybe LetExpr
parseLet p | parsed == [] = Nothing
           | otherwise = Just (fst(head(parsed)))
     where parsed = parse (letAppParser <|> parserHelper <|> letParser <|> takeVarParser) p

-- Takes the number of the LetVar
takeLetVar :: Parser Int
takeLetVar = do char 'x'
                num <- nat
                space
                return (num)

-- Takes the number of the LetFun
takeLetFun :: Parser Int
takeLetFun = do char 'f'
                num <- nat
                space
                return (num)

-- Gives us the LetFun
takeNumParser :: Parser LetExpr
takeNumParser = do num <- nat
                   space
                   return (LetNum num)

-- Gives us the LetVar
takeVarParser :: Parser LetExpr
takeVarParser = do var <- takeLetVar
                   return (LetVar var)

-- Gives us the LetFun
takeFunParser :: Parser LetExpr
takeFunParser = do fun <- takeLetFun
                   return (LetFun fun)

-- Gives us a the tuple for the LetDef
tupleParser :: Parser ([Int], LetExpr)
tupleParser = do many (symbol "; ")
                 fun <- takeLetFun
                 space
                 num <- some takeLetVar
                 space
                 char '='
                 space
                 coNum <- letAppParser <|> parserHelper <|> takeVarParser <|> takeNumParser <|> takeFunParser
                 return ([fun]++num, coNum)

-- Gives us the list of tuples for the let expression
listParser :: Parser [([Int], LetExpr)]
listParser = do a <- many tupleParser
                return a

-- Parses the LetApp case by checking if there are more then two expressions in the LetApp
letAppParser :: Parser LetExpr
letAppParser = do expr1 <- parserHelper <|> takeVarParser <|> takeFunParser <|> takeNumParser
                  expr2 <- parserHelper <|> takeVarParser <|> takeFunParser <|> takeNumParser
                  exprM <- many (letAppParser <|> takeVarParser <|> takeFunParser <|> takeNumParser)
                  if exprM == []
                  then return (LetApp expr1 expr2)
                  else return (LetApp (LetApp expr1 expr2) (head exprM))

-- Parses the expressions with parantheses
parserHelper :: Parser LetExpr
parserHelper = do space
                  char '('
                  r <- letAppParser <|> letParser <|> takeVarParser <|> takeFunParser <|> takeNumParser
                  char ')'
                  space
                  return r

-- Parses the let expressions
letParser :: Parser LetExpr
letParser = do symbol "let"
               letExpr <- listParser
               symbol "in"
               end <- letAppParser <|> letParser <|> takeVarParser <|> takeFunParser <|> takeNumParser
               return (LetDef letExpr end)


-- Challenge 5
-- Encode lambda terms as combinators 

data CLExpr = S | K | I | CLVar Int | CLApp CLExpr CLExpr 
              deriving (Show,Read,Eq, Generic) 

-- new type for Combined Expressions that has both Lam and CL expressions
data CExpr = CS | CK | CI | CVar Int | CApp CExpr CExpr | CAbs Int CExpr
              deriving (Show, Read, Eq, Generic)

clTransform :: LamExpr -> CLExpr
clTransform x =  cTransformer (lamCTransformer x)

-- finding whether a variable is free in the combined expression
freeC :: Int -> CExpr -> Bool
freeC x y | y == CS || y == CK || y == CI = False
freeC x (CVar y) = x == y
freeC x (CAbs y z) | x == y = False
                   | otherwise = freeC x z
freeC x (CApp y z) = (freeC x y) || (freeC x z)

-- transformation function from Lam to Combined Expression
lamCTransformer :: LamExpr -> CExpr
lamCTransformer (LamVar x) = (CVar x)
lamCTransformer (LamAbs x (LamVar y)) | x == y = CI
                                      | otherwise = (CApp (CK) (CVar y))
lamCTransformer (LamAbs x (LamApp a b)) = (CApp (CApp (CS) (lamCTransformer (LamAbs x a))) (lamCTransformer (LamAbs x b)))
lamCTransformer (LamAbs x (LamAbs y z)) = cabsTransformer (CAbs x (lamCTransformer (LamAbs y z)))
lamCTransformer (LamApp x y) = (CApp (lamCTransformer x) (lamCTransformer y))

-- transformation function for the nested Abs cases in the Combined Expression
cabsTransformer :: CExpr -> CExpr
cabsTransformer (CAbs x (CVar y)) | x == y = CI
                                  | otherwise = (CApp (CK) (CVar y))
cabsTransformer (CAbs x (CApp a b)) | freeC x (CApp a b) = (CApp (CApp (CS) (cabsTransformer (CAbs x a))) (cabsTransformer (CAbs x b)))
                                    | otherwise = (CApp CK (CApp a b))
cabsTransformer (CAbs x CI) = (CApp (CK) (CI))
cabsTransformer (CAbs x CK) = (CApp (CK) (CK))
cabsTransformer (CAbs x CS) = (CApp (CK) (CS))

-- transformation function from Combined to CL expression
cTransformer :: CExpr -> CLExpr
cTransformer CI = I
cTransformer CK = K
cTransformer CS = S
cTransformer (CVar x) = (CLVar x)
cTransformer (CApp x y) = (CLApp (cTransformer x) (cTransformer y))

-- Challenge 6
-- Compare Innermost and Outermost Reduction for Lambda and Combinators 

outerRedn1 :: LamExpr -> Maybe LamExpr
outerRedn1 _ = Just (LamVar 0)

innerRedn1 :: LamExpr -> Maybe LamExpr
innerRedn1 _ = Just (LamVar 0)

outerCLRedn1 :: CLExpr -> Maybe CLExpr
outerCLRedn1 _ = Just (CLVar 0)

innerCLRedn1 :: CLExpr -> Maybe CLExpr
innerCLRedn1 _ = Just (CLVar 0)

compareInnerOuter :: LamExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter _ _ = (Just 0, Just 0, Just 0, Just 0) 
