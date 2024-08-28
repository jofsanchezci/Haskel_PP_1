-- Palindrome.hs

-- Verificación de palíndromo
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Función main para probar la verificación de palíndromo
main :: IO ()
main = do
    putStrLn "Ingrese una cadena para verificar si es un palíndromo:"
    input <- getLine
    if isPalindrome input
        then putStrLn "Es un palíndromo."
        else putStrLn "No es un palíndromo."
