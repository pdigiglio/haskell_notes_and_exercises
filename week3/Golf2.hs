
import Data.List

data_ :: [Integer] 
data_ = [1,1,1,3,5,8,9,9] 

main :: IO()
main =
    do
        print $ counts data_
        putStr $ histo data_


--minVal, maxVal :: Integer
--minVal = 0
--maxVal = 9

--countIf :: (a -> Bool) -> [a] -> Int
--countIf p = length . filter p

counts :: [Integer] -> [Int]
--counts = zipWith countIf [0..9] . repeat
--counts l = [countIf (==k) l | k <- [0..9]] 
counts l = [length $ filter (==k) l | k <- [0..9]] 
--counts = map (length . tail) . group . sort . (++ [0..9])

toRow :: Int -> [Int] -> String
toRow k = map (\x -> if (x >= k) then '*' else ' ')

histo :: [Integer] -> String
histo l =
    concatMap (++ "\n")
    $ reverse
    $ (["==========\n0123456789"] ++)
    $ zipWith toRow [1..m] (repeat cs)
    where
        cs = counts l
        m = maximum cs
    

