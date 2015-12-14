
data Direction = L | R deriving (Show)
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
data Crumb a =  LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Directions = [Direction]
type Breadcrums a = [Crumb a]
type Zipper a = (Tree a, Breadcrums a)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

modify :: (a-> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

goLeft :: (Tree a, Breadcrums a) -> (Tree a, Breadcrums a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight :: (Tree a, Breadcrums a) -> (Tree a, Breadcrums a)
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: (Tree a, Breadcrums a) -> (Tree a, Breadcrums a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t,bs)

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
             (Node 'L'  
                  (Node 'N' Empty Empty)  
                  (Node 'T' Empty Empty)  
              )  
             (Node 'Y'  
                  (Node 'S' Empty Empty)  
                  (Node 'A' Empty Empty)  
              )  
         )  
         (Node 'L'  
             (Node 'W'  
                  (Node 'C' Empty Empty)  
                  (Node 'R' Empty Empty)  
              )  
             (Node 'A'  
                  (Node 'A' Empty Empty)  
                  (Node 'C' Empty Empty)  
              )  
         )  


applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)


