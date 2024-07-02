module Strictness where

import Prelude hiding (fst,snd)

----------------------------------------------

err0 :: Int
err0 = 5 `div` 0

ok0 :: Int
ok0 = let x = 5 `div` 0
       in 4

seqErr0 :: Int
seqErr0 = let x = 5 `div` 0
           in x `seq` 4

----------------------------------------------

fst :: (a,b) -> a
fst (a,b) = a

snd :: (a,b) -> b
snd (a,b) = b


pair1 :: (Int,Int)
pair1 = (3, 5 `div` 0)


ok1 :: Int
ok1 = fst pair1

err1 :: Int
err1 = snd pair1


seqOk1 :: Int
seqOk1 = pair1 `seq` 7

----------------------------------------------

pair3 :: (Int,Int)
pair3 = (5 `div` 0, error "BANG!")


fun3 :: (Int,Int) -> Char
fun3 (x,y) = 'A'


ok3 :: Char
ok3 = fun3 pair3

ok3' :: Char
ok3' = pair3 `seq` 'B'


funSeq3 :: (Int,Int) -> Char
funSeq3 (x,y) = y `seq` 'C'

err3 :: Char
err3 = funSeq3 pair3

----------------------------------------------

data Pair x y = Pair x !y deriving Show


pfst :: Pair x y -> x
pfst (Pair x y) = x

psnd :: Pair x y -> y
psnd (Pair x y) = y


errFst :: Pair Int Int
errFst = Pair (5 `div` 0) 3

errSnd :: Pair Int Int
errSnd = Pair 2 (5 `div` 0)


ok2 :: Int
ok2 = psnd errFst

err2 :: Int
err2 = pfst errSnd


fun2 :: Pair Int Int -> Char
fun2 p = 'A'

ok2' :: Char
ok2' = fun2 errSnd

----------------------------------------------

data Box a = Box a deriving Show


bombBox :: Box a
bombBox = Box (error "Bomb!")

fakeBox :: Box a
fakeBox = error "Fake!"


openBox1 :: Box a -> String
openBox1 (Box a) = "Box opened safely."

openBox2 :: Box a -> String
openBox2 b = b `seq` "Box opened safely."


openBomb1 :: String
openBomb1 = openBox1 bombBox

openBomb2 :: String
openBomb2 = openBox2 bombBox

openFake1 :: String
openFake1 = openBox1 fakeBox

openFake2 :: String
openFake2 = openBox2 fakeBox

----------------------------------------------

newtype NewBox a = NewBox a deriving Show


bombBox' :: NewBox a
bombBox' = NewBox (error "Bomb!")

fakeBox' :: NewBox a
fakeBox' = error "Fake!"


openBox1' :: NewBox a -> String
openBox1' (NewBox a) = "Box opened safely."

openBox2' :: NewBox a -> String
openBox2' b = b `seq` "Box opened safely."


-- same
openBomb1' :: String
openBomb1' = openBox1' bombBox'

-- different!
openBomb2' :: String
openBomb2' = openBox2' bombBox'

-- different!
openFake1' :: String
openFake1' = openBox1' fakeBox'

-- same
openFake2' :: String
openFake2' = openBox2' fakeBox'

----------------------------------------------
