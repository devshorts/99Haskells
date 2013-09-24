module NineNineProblems where
import System.Random
import Data.List
import Control.Arrow

myLast :: [a] -> a
myLast = last

myButLast :: [a] -> a
myButLast [] = error "No elements"
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Integer -> a
elementAt [] _ = error "No elements"
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1) 

myLength :: [a] -> Integer
myLength a = findLength a 0 
    where 
        findLength [] n = n
        findLength (_:xs) n = findLength xs (n + 1) 
 
isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom a = a == reverse a

data NestedList a = Elem a | List [NestedList a]

flattenList :: NestedList a -> [a]
flattenList (Elem a) = [a]
flattenList (List a) = foldr ((++) . flattenList) [] a

compress :: Eq a => [a] -> [a]
compress a = map head . group $ a

pack :: Eq a => [a] -> [[a]]
pack = group

encode :: Eq a => [a] -> [(Int, a)]
encode a = map (length &&& head) . group $ a       

data Encoding a = Single a | Many Int a deriving(Show, Eq)

encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified a = map translate (encode a)
	where 
		translate (1, n) = Single n
		translate (a, n) = Many a n

decodeModified :: [(Int, a)] -> [a]
decodeModified xs = concatMap decodeHelper xs
	where
		decodeHelper (n, value) = replicate n value

dupli :: [b] -> [b]
dupli xs =  xs >>= replicate 2

repli :: Int -> [b] -> [b]
repli n xs = xs >>= replicate n

split n xs = fst $ splitAt n $ xs


rotate xs a = take len . drop (a `mod` len) . cycle $ xs
	where len = length xs


removeAt :: [t] -> Int -> (t, [t])
removeAt xs n = 
	case splitData of
		([], xs) -> error "Not enough elements"
		(xs, []) -> error "Not enough elements"
		(front, x:back) -> (x, front++back)

	where 
		splitData = splitAt n xs

insertAt :: a -> [a] -> Int -> [a]
insertAt elem list n = first ++ [elem] ++ end	
	where 
		(first, end) = splitAt (n-1) list


range :: (Enum t) => t -> t -> [t]
range start end = [start..end]


rnd_select :: [a] -> Int -> IO [a]
rnd_select list num =
    do
        gen <- newStdGen
        return $
            let infiniteList = randomRs (0, length list - 1) gen
                randos = [ list!!idx | idx <- infiniteList]
            in take num randos