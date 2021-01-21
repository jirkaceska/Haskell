fm :: (a -> b) -> (a -> Bool) -> [a] -> [b]
fm f p xs = ( map f . filter p ) xs


fm007 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
fm007 f p xs = [ y | x <- xs, p x, let y = f x ]

fm42 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
fm42 f p xs = [ y | x <- xs, let y = f x, p x ] 

myMap :: Eq a => [(a, a)] -> a -> a
myMap [] x = x
myMap ((a,b):s) x = if x == a
                    then b
                    else myMap s x

encrypt :: Eq a => [a] -> [(a, a)] -> [a]
encrypt [] _ = []
encrypt xs [] = xs
encrypt xs crypt = map (myMap crypt) xs

data Filesystem = Folder String [Filesystem]
                | File String
                  deriving (Show, Eq)

appendSlash :: String -> String
appendSlash = (++ "/")

relPaths :: Filesystem -> [String]
relPaths (File name) = [name]
relPaths (Folder name []) = [appendSlash name]
relPaths (Folder name fs) = [appendSlash name] 
                            ++ [appendSlash name ++ substr | sub <- fs, substr <- relPaths sub]

data BinTree a = Node a (BinTree a) (BinTree a)
               | Empty
               deriving (Show, Eq)

treeFold :: (a -> b -> b -> b) -> b -> BinTree a -> b
treeFold n e (Node v l r) = n v (treeFold n e l) (treeFold n e r)
treeFold n  e Empty          = e

treeFilter :: (a -> Bool) -> BinTree a -> BinTree a
treeFilter p = treeFold (f p) b

f :: (a -> Bool) -> a -> BinTree a -> BinTree a -> BinTree a
f filterP v tL tR = if filterP v
                    then Node v tL tR
                    else Empty

b :: BinTree a
b = Empty




data NTree a = NNode a [NTree a] deriving (Show, Eq)

ntreeFold :: (a -> [b] -> b) -> NTree a -> b
ntreeFold f (NNode v ts) = f v (map (ntreeFold f) ts)

maybeApply :: Ord a => (a -> a) -> NTree a -> NTree a
maybeApply f t = ntreeFold (g f) t


g :: Ord a => (a -> a) -> a -> [NTree a] -> NTree a
g fcToApply v trees = NNode (min v (fcToApply v)) trees