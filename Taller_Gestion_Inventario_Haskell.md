
# Taller: Resolviendo un Problema de Negocio con Haskell

## Objetivo:
Este taller tiene como objetivo aplicar los conceptos de Haskell para resolver un problema práctico. A lo largo del taller, definirás el problema, diseñarás una solución utilizando Haskell y escribirás el código necesario para implementarla.

## Problema:
**Gestión de Inventario en una Tienda**

Una tienda necesita un sistema básico de gestión de inventario. La tienda vende varios productos, y cada producto tiene un nombre, un precio y una cantidad en stock. El sistema debe permitir:

1. **Agregar un nuevo producto al inventario.**
2. **Actualizar la cantidad de un producto existente.**
3. **Eliminar un producto del inventario.**
4. **Consultar el inventario para ver el total de productos en stock y su valor total.**

## Especificaciones:

### Estructura de Datos:

- Utiliza una lista de tuplas para representar el inventario. Cada tupla debe contener el nombre del producto (String), el precio (Double) y la cantidad en stock (Int).

### Funciones Requeridas:

- `addProduct :: [(String, Double, Int)] -> String -> Double -> Int -> [(String, Double, Int)]`
  - Esta función debe agregar un nuevo producto al inventario.

- `updateQuantity :: [(String, Double, Int)] -> String -> Int -> [(String, Double, Int)]`
  - Esta función debe actualizar la cantidad en stock de un producto existente.

- `removeProduct :: [(String, Double, Int)] -> String -> [(String, Double, Int)]`
  - Esta función debe eliminar un producto del inventario.

- `inventorySummary :: [(String, Double, Int)] -> (Int, Double)`
  - Esta función debe devolver el número total de productos en stock y el valor total del inventario.

## Ejemplo de Implementación:

```haskell
-- Agregar un nuevo producto al inventario
addProduct :: [(String, Double, Int)] -> String -> Double -> Int -> [(String, Double, Int)]
addProduct inventory name price quantity = inventory ++ [(name, price, quantity)]

-- Actualizar la cantidad de un producto existente
updateQuantity :: [(String, Double, Int)] -> String -> Int -> [(String, Double, Int)]
updateQuantity [] _ _ = []
updateQuantity ((n, p, q):xs) name newQuantity
    | n == name = (n, p, newQuantity) : xs
    | otherwise = (n, p, q) : updateQuantity xs name newQuantity

-- Eliminar un producto del inventario
removeProduct :: [(String, Double, Int)] -> String -> [(String, Double, Int)]
removeProduct inventory name = filter (\(n, _, _) -> n /= name) inventory

-- Resumen del inventario: total de productos y valor total
inventorySummary :: [(String, Double, Int)] -> (Int, Double)
inventorySummary inventory = (totalQuantity, totalValue)
  where
    totalQuantity = sum [q | (_, _, q) <- inventory]
    totalValue = sum [p * fromIntegral q | (_, p, q) <- inventory]
```

## Ejemplo de Prueba:

```haskell
main :: IO ()
main = do
    let inventory = []
    let inventory1 = addProduct inventory "Manzanas" 0.5 100
    let inventory2 = addProduct inventory1 "Platanos" 0.3 150
    let inventory3 = updateQuantity inventory2 "Manzanas" 120
    let inventory4 = removeProduct inventory3 "Platanos"
    let (totalQty, totalValue) = inventorySummary inventory4
    
    putStrLn $ "Inventario Final: " ++ show inventory4
    putStrLn $ "Total de productos en stock: " ++ show totalQty
    putStrLn $ "Valor total del inventario: " ++ show totalValue
```

## Expansión:

Como ejercicio adicional, puedes expandir la funcionalidad del sistema:

1. **Buscar un Producto:** Implementa una función que permita buscar un producto por su nombre y devolver su precio y cantidad en stock.
2. **Aplicar Descuentos:** Implementa una función que aplique un descuento a todos los productos del inventario.

---

Este taller te permitirá aplicar tus conocimientos de Haskell a un problema práctico y desarrollar un sistema básico de gestión de inventario.
