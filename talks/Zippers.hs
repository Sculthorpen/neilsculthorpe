module Zippers where

-----------------------------------------------

modifySuffix :: Int -> ([a] -> [a]) -> [a] -> [a]
modifySuffix n f as = let (bs,cs) = splitAt n as
                      in bs ++ f cs

-----------------------------------------------

type ListContext a = [a]

type ListZipper a = (ListContext a, [a])

forward :: ListZipper a -> ListZipper a
forward (ctx, a:as) = (a:ctx, as)

backward :: ListZipper a -> ListZipper a
backward (a:ctx, as) = (ctx, a:as)

modify :: ([a] -> [a]) -> ListZipper a -> ListZipper a
modify f (ctx, as) = (ctx, f as)

-----------------------------------------------

data Tree a = Branch (Tree a) (Tree a) | Leaf a
              deriving (Eq,Show)

data Direction = L | R
                 deriving (Eq,Show)

type TreeContext a = [(Direction, Tree a)]

type TreeZipper a = (TreeContext a, Tree a)

up :: TreeZipper a -> TreeZipper a
up ((L, r):ctx, l)  = (ctx, Branch l r)
up ((R, l):ctx, r)  = (ctx, Branch l r)

left :: TreeZipper a -> TreeZipper a
left (ctx, Branch l r) = ((L,r):ctx, l)

right :: TreeZipper a -> TreeZipper a
right (ctx, Branch l r) = ((R,l):ctx, r)

modifyTree :: (Tree a -> Tree a) -> TreeZipper a -> TreeZipper a
modifyTree f (ctx, t) = (ctx, f t)

tree1 :: Tree Int
tree1 = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))

treeZipper1 :: TreeZipper Int
treeZipper1 = ([], Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3)))

treeZipper2 :: TreeZipper Int
treeZipper2 = ([(R,Leaf 1)], Branch (Leaf 2) (Leaf 3))

treeZipper3 :: TreeZipper Int
treeZipper3 = ([(L,Leaf 3),(R,Leaf 1)], Leaf 2)

treeZipper4 :: TreeZipper Int
treeZipper4 = ([(L,Branch (Leaf 2) (Leaf 3))], Leaf 1)

-----------------------------------------------

data BTree a = Node (BTree a) a (BTree a) | Empty
               deriving (Eq,Show)

-- type BTreeZipper = ...

-- ancestors :: BTreeZipper a -> [a]

-- top :: BTreeZipper a -> BTreeZipper a

-- zipperToTree :: BTreeZipper a -> BTree a

-----------------------------------------------
