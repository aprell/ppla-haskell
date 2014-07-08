module PPLA where

import Prelude hiding (head, last, tail, init, (!!), length, reverse, map, filter, lookup)
import Data.Char
import Test.QuickCheck

----- Let's get started

double :: Int -> Int
double x = x + x

----- Recursion

fact :: Integer -> Integer
fact 0 = 1
fact n | n > 0 = n * fact (n-1)
       | otherwise = error "Factorial undefined for negative values"

safeFact :: Integer -> Maybe Integer
safeFact n | n < 0 = Nothing
           | otherwise = Just (fact n)

testFact = do
    putStr "factorial "
    f <- readLn
    case safeFact f of
        Just n  -> putStrLn (show n) >> testFact
        Nothing -> putStrLn "Undefined"

----- Pattern matching

head :: [a] -> a
head (x:xs) = x
head _ = error "empty list"

last :: [a] -> a
last (x:[]) = x
last (x:xs) = last xs
last _ = error "empty list"

tail :: [a] -> [a]
tail (x:xs) = xs
tail _ = error "empty list"

init :: [a] -> [a]
init (x:[]) = []
init (x:xs) = x : init xs
init _ = error "empty list"

(!!) :: [a] -> Int -> a
xs     !! n | n < 0 = error "negative index"
[]     !! n = error "index too large"
(x:_)  !! 0 = x
(_:xs) !! n = xs !! (n-1)

length :: Num a => [t] -> a
length [] = 0
length (x:xs) = 1 + length xs

length' :: Num a => [t] -> a
length' xs = go 0 xs
    where go acc [] = acc
          go acc (x:xs) = go (acc+1) xs

-- quickCheck (\xs -> length xs == length' xs)

-- makePair x y = (x, y)
-- makePair x y = (,) x y
makePair = (,)

reversePair :: (a, b) -> (b, a)
reversePair (x, y) = (y, x)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

reverse' :: [a] -> [a]
reverse' xs = go [] xs
    where go acc [] = acc
          go acc (x:xs) = go (x:acc) xs

-- Type of property determines which test data is generated
-- quickCheck ((\xs -> reverse xs == reverse' xs) :: [Int] -> Bool)

prop_reverse :: Eq a => [a] -> [a] -> Bool
prop_reverse xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

-- quickCheck (prop_reverse :: [Int] -> [Int] -> Bool)

{- Performance difference between appending and prepending to a list
ghci> :set +s
ghci> reverse [1..1000] `seq` ()
()
(0.01 secs, 2677232 bytes)
ghci> reverse [1..10^6] `seq` ()
()
(1.45 secs, 302035480 bytes)
ghci> reverse [1..10^7] `seq` ()
()
(17.36 secs, 2988083376 bytes)
ghci> reverse' [1..1000] `seq` ()
()
(0.01 secs, 2647256 bytes)
ghci> reverse' [1..10^6] `seq` ()
()
(0.69 secs, 211333720 bytes)
ghci> reverse' [1..10^7] `seq` ()
()
(8.59 secs, 2083184040 bytes)
-}

palindrome :: Eq a => [a] -> Bool
palindrome p = reverse p == p

diff :: String -> String -> String
diff (x:xs) (y:ys)
    | x == y = x : diff xs ys
    | otherwise = '-' : diff xs ys
diff _ _ = []

----- Function composition

isEven n = n `mod` 2 == 0

-- isOdd n = not (isEven n)
isOdd = not . isEven
-- ghci> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c

twice f = f . f
-- twice (+2) 4 == ((+2) . (+2)) 4 == ((+2) ((+2) 4)) == 8
-- twice reverse "Haskell"

times n f | n > 0 = f . times (n-1) f
times _ _ = id
-- 2 `times` (+2) $ 4
-- 5 `times` (++ [42]) $ []

----- List comprehensions and infinite lists

squares :: Int -> [Int]
squares n = [x^2 | x <- [1..n], isEven x]

squaresInf :: [Int]
squaresInf = [x^2 | x <- [1..]]

primes = [p | p <- [2..], divisors p == [1, p]]
    where divisors n = [d | d <- [1..n], n `mod` d == 0]

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = ins x (isort xs)
    where ins x [] = [x]
          ins x (y:ys) | x <= y = x : y : ys
                       | otherwise = y : ins x ys

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = [a | a <- xs, a <= x]
          rhs = [a | a <- xs, a > x]

sorted :: Ord a => [a] -> Bool
sorted []  = True
sorted [x] = True
sorted (x:y:xs) = x <= y && sorted (y:xs)

prop_sorted :: Ord a => [a] -> Bool
prop_sorted = sorted . qsort

-- quickCheck (prop_sorted :: [Float] -> Bool)

----- Higher-order functions

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
-- map (*2) [1..10]
-- map isEven [1..10]
-- map ord "Haskell"
-- map (++ "!") ["foo", "bar", "baz"]

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs
-- filter (< 5) [1..10]
-- filter isEven [1..10]
-- filter palindrome ["racecar", "Otto", "LOL"]

reduce :: (a -> b -> a) -> a -> [b] -> a
reduce _ acc [] = acc
reduce f acc (x:xs) = reduce f (f acc x) xs
-- reduce (+) 0 [1..10]
-- reduce (*) 1 [1..10]
-- reduce min 3 [3,1,4,1,5,9,2,6]
-- reduce max 3 [3,1,4,1,5,9,2,6]

reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 f (x:xs) = reduce f x xs
reduce1 _ [] = error "reduce1 must be applied to non-empty lists"
-- reduce1 (+) [1..10]
-- reduce1 (*) [1..10]
-- reduce1 min [3,1,4,1,5,9,2,6]
-- reduce1 max [3,1,4,1,5,9,2,6]
-- reduce1 undefined []

reverse'' :: [a] -> [a]
reverse'' xs = reduce f [] xs
    where f = \a x -> x:a

----- foldl vs foldr

testFold = do
    let test fold = fold (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..10])
    putStrLn "foldl (+) 0 [1..10]"
    putStrLn $ "  " ++ test foldl -- "((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)"
    putStrLn "foldr (+) 0 [1..10]"
    putStrLn $ "  " ++ test foldr -- "(1+(2+(3+(4+(5+(6+(7+(8+(9+(10+0))))))))))"

----- Lazy evaluation

{- Two simple examples demonstrating Haskell's laziness
ghci> let x = 1+2
ghci> :sprint x
x = _
ghci> let y = x*2
ghci> :sprint y
y = _
ghci> y
6
ghci> :sprint y
y = 6
ghci> :sprint x
x = 3

ghci> let xs = map (+1) [1..5]
ghci> :sprint xs
xs = _
ghci> length xs
5
ghci> :sprint xs
xs = [_,_,_,_,_] -- because length evaluates to WHNF
ghci> xs !! 0
2
ghci> :sprint xs
xs = [2,_,_,_,_]
ghci> xs !! 3
5
ghci> :sprint xs
xs = [2,_,_,5,_]
ghci> sum xs
20
ghci> :sprint xs
xs = [2,3,4,5,6]
-}

myIf :: Bool -> a -> a -> a
myIf True  t _ = t
myIf False _ e = e

{-
ghci> myIf True "true" undefined
"true"
ghci> myIf False "true" undefined
*** Exception: Prelude.undefined
ghci> myIf True "true" $! undefined
*** Exception: Prelude.undefined -- ($!) forces evaluation
-}

----- QuickCheck

runQuickCheck = do
    putStrLn "Testing length"
    quickCheck ((\xs -> length xs == length' xs) :: String -> Bool)

    putStrLn "Testing reverse"
    quickCheck ((\xs -> reverse xs == reverse' xs) :: [Int] -> Bool)

    putStrLn "Testing prop_reverse"
    quickCheck (prop_reverse :: [Int] -> [Int] -> Bool)

    putStrLn "Testing prop_sorted"
    quickCheck (prop_sorted :: [Float] -> Bool)

----- Data types

-- String is a type synonym for [Char]
-- type String = [Char]

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

distanceFromOrigin :: Point -> Double
distanceFromOrigin = distance (0, 0)

type SymbolTable = [(String, Double)]

numbers :: SymbolTable
numbers = [("pi", pi), ("e", exp 1), ("root 2", sqrt 2)]

lookup :: String -> SymbolTable -> Maybe Double
lookup _ [] = Nothing
lookup s ((k, v):xs)
    | s == k = Just v
    | otherwise = lookup s xs
-- lookup "pi" numbers
-- lookup "root 2" numbers
-- lookup "root 3" numbers

data Answer = Yes | No deriving (Show, Read)

testAnswer = do
    putStr "Quit? "
    a <- getLine
    case reads a :: [(Answer, String)] of
        [(Yes, _)] -> return ()
        [(No,  _)] -> putStrLn "Try again" >> testAnswer
        _          -> putStrLn "Invalid input" >> testAnswer

-- data Movie = Movie String Int Float deriving (Show, Eq, Ord)
data Movie = Movie
    { movieTitle :: String,
      movieYear  :: Int,
      movieScore :: Float
    } deriving (Show, Eq, Ord)

imdbTop5 :: [Movie]
imdbTop5 =
    [ Movie
      { movieTitle = "The Shawshank Redemption",
        movieYear  = 1994,
        movieScore = 9.2
      },
      Movie
      { movieTitle = "The Godfather",
        movieYear  = 1972,
        movieScore = 9.2
      },
      Movie
      { movieTitle = "The Godfather: Part II",
        movieYear  = 1974,
        movieScore = 9.0
      },
      Movie
      { movieTitle = "The Dark Knight",
        movieYear  = 2008,
        movieScore = 8.9
      },
      Movie
      { movieTitle = "Pulp Fiction",
        movieYear  = 1994,
        movieScore = 8.9
      }
    ]
-- movieScore (head imdbTop5)
-- map movieTitle imdbTop5
-- qsort imdbTop5

----- Recursive data types

data AExpr a = Literal a
             | Add (AExpr a) (AExpr a)
             | Sub (AExpr a) (AExpr a)
             deriving Show

eval :: AExpr Double -> Double
eval (Literal a) = a
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
-- eval $ Add (Literal 1) (Sub (Literal 3) (Literal 2))
-- eval $ Literal 1 `Add` (Literal 3 `Sub` Literal 2)

data List a = Null | Cons a (List a) deriving Show

infixr 5 `Cons` -- right-associative, precedence level 5 (same as (:))

append :: List a -> List a -> List a
Null      `append` ys = ys
Cons x xs `append` ys = x `Cons` (xs `append` ys)
-- 1 `Cons` Null `append` (2 `Cons` 3 `Cons` Null)
