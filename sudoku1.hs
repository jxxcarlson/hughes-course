type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char 

rows :: Matrix a -> Matrix a
rows = id  

col :: Int -> Matrix a -> [a]
col k = fmap ( !! k)

cols :: Matrix a -> Matrix a
cols m = 
    fmap (\k -> col k m) [0..n] 
       where n = length m - 1

rowSlice :: Int -> Int -> Matrix a -> Matrix a
rowSlice i j m = 
   [m !! k | k <- [i..j]]

colSlice :: Int -> Int -> Matrix a -> Matrix a
colSlice i j m = 
   [col k m | k <- [i..j]]

subSquare :: Int -> Int -> Matrix a -> Matrix a
subSquare r c m = 
    colSlice (3*c) (3*c + 2) $ rowSlice (3*r) (3*r + 2) m


nines = ["123", "456", "789"]

m27 = ["123456789", "234567891", "345678912"
        , "456789123", "567891234", "678912345"
        , "789123456", "891234567", "912345678"]
