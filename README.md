
# Guía para Iniciar en la Programación con Haskell

Haskell es un lenguaje de programación funcional puro, conocido por su enfoque en la inmutabilidad y las funciones como ciudadanos de primera clase. A continuación, te proporciono una guía paso a paso para comenzar a programar en Haskell.

## 1. Instalación de Haskell

Antes de comenzar a programar, necesitas instalar el compilador de Haskell. La forma más sencilla de instalar Haskell es mediante el uso de la herramienta `GHCup`, que administra las versiones de GHC (el compilador de Haskell) y otras herramientas relacionadas.

- **En macOS/Linux:**
  ```bash
  curl -sSL https://get.haskellstack.org/ | sh
  ```
  o usa:
  ```bash
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
  ```

- **En Windows:**
  Visita [https://www.haskell.org/platform/](https://www.haskell.org/platform/) y sigue las instrucciones para instalar Haskell Platform.

## 2. Primer Programa en Haskell

Una vez que tengas Haskell instalado, puedes crear tu primer programa. Tradicionalmente, esto se hace con un simple "Hello, World!".

- Crea un archivo llamado `Hello.hs`:

  ```haskell
  main :: IO ()
  main = putStrLn "Hello, World!"
  ```

- **Compilar y ejecutar:**

  ```bash
  ghc -o Hello Hello.hs
  ./Hello
  ```

Esto debería mostrar "Hello, World!" en la terminal.

## 3. Entorno Interactivo: GHCi

GHCi es el intérprete interactivo de Haskell que te permite probar expresiones, funciones, y módulos sin necesidad de compilar un programa completo.

- **Iniciar GHCi:**
  ```bash
  ghci
  ```

- **Probar funciones:**
  ```haskell
  Prelude> 2 + 2
  4
  ```

GHCi es muy útil para experimentar con pequeñas partes de código y para aprendizaje interactivo.

## 4. Estructura Básica de un Programa

Los programas en Haskell consisten principalmente en funciones. Una función básica en Haskell se define como:

```haskell
square :: Int -> Int
square x = x * x
```

En este caso, `square` es una función que toma un entero (`Int`) y devuelve su cuadrado.

## 5. Tipos de Datos Básicos

Haskell tiene varios tipos de datos básicos:

- **Int**: Enteros.
- **Integer**: Enteros de precisión arbitraria.
- **Float**: Números de punto flotante.
- **Double**: Números de punto flotante de doble precisión.
- **Char**: Un solo carácter.
- **Bool**: Valores booleanos (`True` o `False`).
- **String**: Una lista de caracteres.

Puedes crear listas en Haskell como:

```haskell
numbers = [1, 2, 3, 4, 5]
```

Y acceder a elementos de la lista:

```haskell
firstNumber = head numbers
```

## 6. Funciones y Recursión

Las funciones son fundamentales en Haskell. La recursión es una técnica clave debido a la naturaleza inmutable del lenguaje.

Por ejemplo, una función recursiva para calcular el factorial de un número:

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

## 7. Manejo de Listas

Las listas son fundamentales en Haskell. Aquí tienes algunas operaciones comunes:

- **Concatenación:**
  ```haskell
  [1, 2, 3] ++ [4, 5, 6]  -- Resultado: [1, 2, 3, 4, 5, 6]
  ```

- **Agregar al inicio:**
  ```haskell
  0 : [1, 2, 3]  -- Resultado: [0, 1, 2, 3]
  ```

- **Mapeo:**
  ```haskell
  map (*2) [1, 2, 3]  -- Resultado: [2, 4, 6]
  ```

## 8. Comprensión de Listas

Haskell tiene una característica poderosa llamada comprensión de listas, que es similar a los generadores de Python:

```haskell
squares = [x^2 | x <- [1..10]]
```

Esto genera una lista de los cuadrados de los números del 1 al 10.

## 9. Módulos y Librerías

Haskell tiene una rica colección de módulos y librerías. Para utilizar módulos adicionales, como `Data.List`:

```haskell
import Data.List
```

Esto te permite acceder a funciones adicionales, como `sort`:

```haskell
sortedList = sort [3, 2, 1]
```

## 10. Ejercicios de Práctica

1. **Factorial Recursivo:** Implementa una función que calcule el factorial de un número.
2. **Listas:** Define una función que tome una lista de enteros y devuelva la suma de los cuadrados de los números impares.
3. **Recursión en Listas:** Implementa una función que cuente el número de elementos en una lista.

## 11. Documentación y Recursos

- **Learn You a Haskell for Great Good!**: Un libro interactivo y tutorial en línea. https://learnyouahaskell.com/

- **Haskell Programming from First Principles**: Un libro más profundo sobre Haskell. https://www.goodreads.com/book/show/25587599-haskell-programming-from-first-principles

- **Hackage**: Repositorio de paquetes Haskell. https://hackage.haskell.org/

# Ejercicios de Programación en Haskell

Este repositorio contiene una serie de ejercicios diseñados para ayudarte a aprender y practicar la programación en Haskell. Cada ejercicio cubre diferentes aspectos del lenguaje y te permitirá familiarizarte con sus características.

## Ejercicios

### 1. Reversa de una Lista

**Descripción:** Implementa una función que tome una lista y devuelva una nueva lista con los elementos en orden inverso.

**Código:**
```haskell
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
```

### 2. Verificación de Palíndromo

**Descripción:** Implementa una función que verifique si una cadena de texto es un palíndromo (se lee igual de izquierda a derecha que de derecha a izquierda).

**Código:**
```haskell
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs
```

### 3. Suma de una Lista

**Descripción:** Implementa una función recursiva que sume todos los elementos de una lista.

**Código:**
```haskell
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs
```

### 4. Máximo de una Lista

**Descripción:** Implementa una función recursiva que encuentre el máximo elemento en una lista de números enteros.

**Código:**
```haskell
maxList :: [Int] -> Int
maxList [] = error "Lista vacía"
maxList [x] = x
maxList (x:xs) = max x (maxList xs)
```

### 5. Número Fibonacci

**Descripción:** Implementa una función que devuelva el n-ésimo número de la secuencia de Fibonacci. La secuencia comienza con 0 y 1, y cada número subsiguiente es la suma de los dos anteriores.

**Código:**
```haskell
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
```

### 6. Factorial usando `foldl`

**Descripción:** Reimplementa la función factorial utilizando `foldl` (una función de plegado que aplica una función a una lista de izquierda a derecha).

**Código:**
```haskell
factorialFoldl :: Int -> Int
factorialFoldl n = foldl (*) 1 [1..n]
```

### 7. Filtro de Números Pares

**Descripción:** Implementa una función que filtre y devuelva solo los números pares de una lista.

**Código:**
```haskell
filterEvens :: [Int] -> [Int]
filterEvens xs = filter even xs
```

### 8. Concatenación de Listas

**Descripción:** Implementa una función que tome dos listas y las concatene.

**Código:**
```haskell
concatLists :: [a] -> [a] -> [a]
concatLists [] ys = ys
concatLists (x:xs) ys = x : concatLists xs ys
```

### 9. Inversión de Pares en una Lista de Tuplas

**Descripción:** Implementa una función que tome una lista de tuplas y devuelva una lista con los elementos de cada tupla invertidos.

**Código:**
```haskell
invertTuples :: [(a, b)] -> [(b, a)]
invertTuples xs = [(y, x) | (x, y) <- xs]
```

### 10. Mapa de una Función sobre una Lista

**Descripción:** Implementa una función que aplique una función dada a cada elemento de una lista.

**Código:**
```haskell
applyFunction :: (a -> b) -> [a] -> [b]
applyFunction f xs = map f xs
```

### 11. Producto de Todos los Elementos en una Lista

**Descripción:** Implementa una función que calcule el producto de todos los elementos de una lista.

**Código:**
```haskell
productList :: [Int] -> Int
productList [] = 1
productList (x:xs) = x * productList xs
```

### 12. Eliminación de Duplicados en una Lista

**Descripción:** Implementa una función que elimine duplicados en una lista.

**Código:**
```haskell
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs
```

### 13. Cálculo de la Media de una Lista

**Descripción:** Implementa una función que calcule la media (promedio) de una lista de números.

**Código:**
```haskell
average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)
```

### 14. Merge de Dos Listas Ordenadas

**Descripción:** Implementa una función que mezcle dos listas ordenadas en una sola lista ordenada.

**Código:**
```haskell
mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted [] ys = ys
mergeSorted xs [] = xs
mergeSorted (x:xs) (y:ys)
    | x <= y    = x : mergeSorted xs (y:ys)
    | otherwise = y : mergeSorted (x:xs) ys
```

### 15. Eliminación de Números Negativos

**Descripción:** Implementa una función que elimine todos los números negativos de una lista.

**Código:**
```haskell
removeNegatives :: [Int] -> [Int]
removeNegatives xs = filter (>= 0) xs
```
