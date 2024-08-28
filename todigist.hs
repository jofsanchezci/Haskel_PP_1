toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = map (read . (:[])) (show n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)
