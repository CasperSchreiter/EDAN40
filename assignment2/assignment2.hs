import Data.Char
import Data.List

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"

type AlignmentType = (String, String)

-------------------------------------------------------------------------------------------------
-- SimilarityScore
-------------------------------------------------------------------------------------------------

similarityScore :: String -> String -> Int
similarityScore [] []     = 0
similarityScore [] (s2:ss2) = score '-' s2 + similarityScore [] ss2
similarityScore (s1:ss1) [] = score s1 '-' + similarityScore ss1 []
similarityScore (s1:ss1) (s2:ss2) = max3 (score s1 s2 + similarityScore ss1 ss2,
                                          score s1 '-' + similarityScore ss1 (s2:ss2),
                                          score '-' s2 + similarityScore (s1:ss1) ss2) 


similarityScore' :: String -> String -> Int
similarityScore' s1 s2 = simLen (length s1) (length s2)
  where 
    simLen i j = simTable !! i !! j
    simTable = [[simEntry i j | j <- [0..]] | i <- [0..]]
    simEntry :: Int -> Int -> Int
    simEntry i 0 = i * scoreSpace
    simEntry 0 j = j * scoreSpace
    simEntry i j = score x y + max3 (simLen (i-1) (j-1),
                                     simLen i (j-1), 
                                     simLen (i-1) j)
      where
        x = s1 !! (i-1)
        y = s2 !! (j-1)


-------------------------------------------------------------------------------------------------
-- OptimalAlignment
-------------------------------------------------------------------------------------------------


optimalAlignments :: String -> String -> [AlignmentType]
optimalAlignments [] [] = [([], [])]
optimalAlignments [] (s2:ss2) = attachHeads '-' s2 (optimalAlignments [] ss2)
optimalAlignments (s1:ss1) [] = attachHeads s1 '-' (optimalAlignments ss1 [])
optimalAlignments (s1:ss1) (s2:ss2) = maximaBy stringScore $ concat [attachHeads s1 s2 (optimalAlignments ss1 ss2),
                                                             attachHeads s1 '-' (optimalAlignments ss1 (s2:ss2)),
                                                             attachHeads '-' s2 (optimalAlignments (s1:ss1) ss2)]


optimalAlignments' :: String -> String -> [AlignmentType]
optimalAlignments' s1 s2 = map (\(a, b) -> (reverse a, reverse b)) $ snd $ optLen (length s1) (length s2)
  where
    optLen i j = optTable !! i !! j
    optTable = [[optEntry i j | j <- [0..]] | i <- [0..]]
    optEntry :: Int -> Int -> (Int, [AlignmentType])
    optEntry 0 0 = (0, [("","")])
    optEntry i 0 = (scoreSpace + (fst $ optLen (i-1) 0), attachHeads (s1 !! (i-1)) '-' (snd $ optLen (i-1) 0))
    optEntry 0 j = (scoreSpace + (fst $ optLen 0 (j-1)), attachHeads '-' (s2 !! (j-1)) (snd $ optLen 0 (j-1)))
    optEntry i j = (fst (head z), concatMap snd z)
      where 
        x = s1 !! (i-1)
        y = s2 !! (j-1)
        z = maximaBy fst $ [(score x y + (fst $ optLen (i-1) (j-1)),  attachHeads x y $ snd $ optLen (i-1) (j-1)),
                           (score '-' y + (fst $ optLen i (j-1)),     attachHeads '-' y $ snd $ optLen i (j-1)),
                           (score x '-' + (fst $ optLen (i-1) j),     attachHeads x '-' $ snd $ optLen (i-1) j)]

-------------------------------------------------------------------------------------------------
-- Output functions
-------------------------------------------------------------------------------------------------

outputOptAlignment s1 s2 = do
  putStr $ "There are " ++ (show $ length a) ++ " optimal alignments: \n"
  printStrings a
  putStr $ "There are " ++ (show $ length a) ++ " optimal alignments \n"
  where a = optimalAlignments' s1 s2

printStrings [] = putChar '\n'
printStrings (s:ss) = do
  putChar '\n'
  putStr $ intersperse ' ' $ fst s 
  putChar '\n'
  putStr $ intersperse ' ' $ snd s
  putStr "\n\n"
  printStrings ss

-------------------------------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------------------------------

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs =  [a | a <- xs, valueFcn a == g]
  where g = maximum $ map valueFcn xs

max3 :: Ord a => (a, a, a) -> a
max3 (x, y, z) = max x $ max y z  

score :: Char -> Char -> Int
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y 
  | x == y = scoreMatch
  | x /= y = scoreMismatch  

stringScore :: (String, String) -> Int
stringScore ([], _) = 0
stringScore (_, []) = 0
stringScore ((x:xs), (y:ys)) = score x y + stringScore (xs, ys)
