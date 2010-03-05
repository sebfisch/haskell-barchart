import Criterion.Main

main = defaultMain [bgroup "fac" [bench (show n) (nf fac n) | n <- ns]]
 where ns = [10^4 * k | k <- [1,2,3]] :: [Integer]

fac 0 = 1
fac n = n * fac (n-1)

