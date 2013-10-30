import Test.QuickCheck

{- 
	Part 1 
 	k-1 steps
-}

-- Part 2
power1 :: Integer -> Integer -> Integer
power1 n 0 = 1
power1 n k = product [n | x <- [1..k]]

-- Part 3
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power2: negative argument!"
power2 n 0 = 1
power2 n 1 = n
power2 n k | even k    = power2 (n*n) (div k 2)
           | otherwise = n * power2 n (k-1)

-- Taken from lecture
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Part 4
{-
    Since our functions are really only definied for positive
    numbers, we will only test those. To give a good test, we will vary 
    between large and small values.
-}

prop_power :: Integer -> Integer -> Bool
prop_power n k = (power n k == power1 n k)
              && (power n k == power2 n k)

prop_power' :: Integer -> Integer -> Bool
prop_power' n k = let k' = abs k in
                  (power n k' == power1 n k') &&
                  (power n k' == power2 n k')

--test:: Bool
test =      prop_power 1 0 &&
            prop_power 1 2 &&
            prop_power 5 5

test' = quickCheck prop_power'
