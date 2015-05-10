module PPLA where

import           Data.Char
import           Prelude         hiding (drop, filter, head, init, last, length,
                                  lookup, map, reverse, tail, take, (!!))
import           Test.QuickCheck

----- Let's get started

double :: Int -> Int
double x = x + x

----- Recursion

-- |
--
-- >>> fact 5
-- 120
--
-- >>> fact (-1)
-- *** Exception: Factorial undefined for negative values
--
fact :: Integer -> Integer
fact 0 = 1
fact n | n > 0 = n * fact (n-1)
       | otherwise = error "Factorial undefined for negative values"

-- |
--
-- >>> safeFact 5
-- Just 120
--
-- >>> safeFact (-1)
-- Nothing
--
safeFact :: Integer -> Maybe Integer
safeFact n | n < 0 = Nothing
           | otherwise = Just (fact n)

testFact = do
    putStr "factorial "
    f <- readLn
    case safeFact f of
        Just n  -> print n >> testFact
        Nothing -> putStrLn "Undefined"

----- Pattern matching

head :: [a] -> a
head (x:xs) = x
head _ = error "empty list"

last :: [a] -> a
last [x] = x
last (x:xs) = last xs
last _ = error "empty list"

tail :: [a] -> [a]
tail (x:xs) = xs
tail _ = error "empty list"

init :: [a] -> [a]
init [x] = []
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

-- | prop> length xs == length' xs
length' :: Num a => [t] -> a
length' xs = go 0 xs
    where go acc [] = acc
          go acc (x:xs) = go (acc+1) xs

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (x:xs) = drop (n-1) xs

-- makePair x y = (x, y)
-- makePair x y = (,) x y
makePair = (,)

reversePair :: (a, b) -> (b, a)
reversePair (x, y) = (y, x)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-- Type of property determines which test data is generated

-- | prop> reverse xs == reverse' (xs :: [Int])
reverse' :: [a] -> [a]
reverse' xs = go [] xs
    where go acc [] = acc
          go acc (x:xs) = go (x:acc) xs

-- | prop> prop_reverse :: [Int] -> [Int] -> Bool
prop_reverse :: Eq a => [a] -> [a] -> Bool
prop_reverse xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

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

-- |
--
-- >>> twice (+2) 4
-- 8
--
-- >>> twice reverse "Haskell"
-- "Haskell"
--
twice f = f . f
-- ^ twice (+2) 4 == ((+2) . (+2)) 4 == ((+2) ((+2) 4)) == 8

-- |
--
-- >>> 2 `times` (+2) $ 4
-- 8
--
-- >>> 5 `times` (++ [42]) $ []
-- [42,42,42,42,42]
--
times n f | n > 0 = f . times (n-1) f
times _ _ = id

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

-- | prop> prop_sorted :: [Float] -> Bool
prop_sorted :: Ord a => [a] -> Bool
prop_sorted = sorted . qsort

-- |
--
-- >>> qsortBy compare [3,1,4,1,5,9,2,6,5]
-- [1,1,2,3,4,5,5,6,9]
--
-- >>> qsortBy (flip compare) [3,1,4,1,5,9,2,6,5]
-- [9,6,5,5,4,3,2,1,1]
--
qsortBy :: (a -> a -> Ordering) -> [a] -> [a]
qsortBy _ [] = []
qsortBy cmp (x:xs) = qsortBy cmp lhs ++ [x] ++ qsortBy cmp rhs
    where lhs = [a | a <- xs, a `cmp` x == LT || a `cmp` x == EQ]
          rhs = [a | a <- xs, a `cmp` x == GT]

-- | prop> prop_qsortBy :: [Float] -> Bool
prop_qsortBy :: Ord a => [a] -> Bool
prop_qsortBy xs = (reverse . qsortBy compare) xs == qsortBy (flip compare) xs

----- Higher-order functions

-- | Map
--
-- >>> map (*2) [1..10]
-- [2,4,6,8,10,12,14,16,18,20]
--
-- >>> map isEven [1..10]
-- [False,True,False,True,False,True,False,True,False,True]
--
-- >>> map ord "Haskell"
-- [72,97,115,107,101,108,108]
--
-- >>> map (++ "!") ["foo", "bar", "baz"]
-- ["foo!","bar!","baz!"]
--
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- | Filter
--
-- >>> filter (< 5) [1..10]
-- [1,2,3,4]
--
-- >>> filter isEven [1..10]
-- [2,4,6,8,10]
--
-- >>> filter palindrome ["racecar", "Otto", "LOL"]
-- ["racecar","LOL"]
--
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs

-- | Reduce
--
-- >>> reduce (+) 0 [1..10]
-- 55
--
-- >>> reduce (*) 1 [1..10]
-- 3628800
--
-- >>> reduce min 3 [3,1,4,1,5,9,2,6]
-- 1
--
-- >>> reduce max 3 [3,1,4,1,5,9,2,6]
-- 9
--
reduce :: (a -> b -> a) -> a -> [b] -> a
reduce _ acc [] = acc
reduce f acc (x:xs) = reduce f (f acc x) xs

-- | Reduce1
--
-- >>> reduce1 (+) [1..10]
-- 55
--
-- >>> reduce1 (*) [1..10]
-- 3628800
--
-- >>> reduce1 min [3,1,4,1,5,9,2,6]
-- 1
--
-- >>> reduce1 max [3,1,4,1,5,9,2,6]
-- 9
--
reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 f (x:xs) = reduce f x xs
reduce1 _ [] = error "reduce1 must be applied to non-empty lists"

reverse'' :: [a] -> [a]
reverse'' xs = reduce f [] xs
    where f = flip (:)

----- foldl vs foldr

testFold = do
    let test fold = fold (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..10])
    putStrLn "foldl (+) 0 [1..10]"
    putStrLn $ "  " ++ test foldl -- "((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)"
    putStrLn "foldr (+) 0 [1..10]"
    putStrLn $ "  " ++ test foldr -- "(1+(2+(3+(4+(5+(6+(7+(8+(9+(10+0))))))))))"

----- Pipelining

-- |
--
-- >>> [1..10] |> filter isEven |> map (^2)
-- [4,16,36,64,100]
--
-- >>> "Haskell" |> map ord |> sum
-- 708
--
(|>) :: a -> (a -> b) -> b
x |> f = f x
-- ^ x |> f |> g == g (f x) == (g . f) x

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

    putStrLn "Testing prop_qsortBy"
    quickCheck (prop_qsortBy :: [Float] -> Bool)

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

-- |
--
-- >>> lookup "pi" numbers
-- Just 3.141592653589793
--
-- >>> lookup "root 2" numbers
-- Just 1.4142135623730951
--
-- >>> lookup "root 3" numbers
-- Nothing
--
lookup :: String -> SymbolTable -> Maybe Double
lookup _ [] = Nothing
lookup s ((k, v):xs)
    | s == k = Just v
    | otherwise = lookup s xs

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

-- |
--
-- >>> eval $ Add (Literal 1) (Sub (Literal 3) (Literal 2))
-- 2.0
--
-- >>> eval $ Literal 1 `Add` (Literal 3 `Sub` Literal 2)
-- 2.0
--
eval :: AExpr Double -> Double
eval (Literal a) = a
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b

data List a = Null | Cons a (List a) deriving Show

infixr 5 `Cons` -- right-associative, precedence level 5 (same as (:))

-- |
--
-- >>> 1 `Cons` Null `append` (2 `Cons` 3 `Cons` Null)
-- Cons 1 (Cons 2 (Cons 3 Null))
--
append :: List a -> List a -> List a
Null      `append` ys = ys
Cons x xs `append` ys = x `Cons` (xs `append` ys)
