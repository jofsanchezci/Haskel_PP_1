-- Función para sumar los cuadrados de los números impares
sumOfOddSquares :: [Int] -> Int
sumOfOddSquares xs = sum [x^2 | x <- xs, odd x]
