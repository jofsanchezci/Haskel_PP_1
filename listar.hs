-- Función recursiva para contar el número de elementos en una lista
countElements :: [a] -> Int
countElements [] = 0
countElements (_:xs) = 1 + countElements xs
