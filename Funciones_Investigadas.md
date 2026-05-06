# Funciones por investigar (Haskell)

Este documento resume los conceptos básicos que se usan en este proyecto y que normalmente se piden investigar para implementar funcionalidades en Haskell.

---

## 1) Manejo de entrada y salida (I/O) en Haskell

En Haskell, los programas “puros” no hacen efectos (como leer teclado o escribir en pantalla).  
Los efectos se manejan con el tipo `IO a`, que significa: “una acción que, al ejecutarse, produce un valor de tipo `a` y puede hacer efectos”.

### Funciones comunes

- `putStrLn :: String -> IO ()`  
  Imprime una línea.
- `getLine :: IO String`  
  Lee una línea del teclado.
- `print :: Show a => a -> IO ()`  
  Imprime cualquier cosa “mostrable” (`Show`).

### Ejemplo básico

```haskell
main :: IO ()
main = do
  putStrLn "¿Cómo te llamás?"
  nombre <- getLine
  putStrLn ("Hola, " ++ nombre)
```

Notas:
- El `do` permite “secuenciar” acciones `IO`.
- El operador `<-` extrae el valor producido por una acción `IO`.

---

## 2) Manejo de archivos en Haskell

Leer y escribir archivos también son efectos, así que usan `IO`.

### Funciones comunes

- `readFile :: FilePath -> IO String`  
  Lee el archivo completo como texto.
- `writeFile :: FilePath -> String -> IO ()`  
  Sobrescribe el archivo con ese texto.
- `appendFile :: FilePath -> String -> IO ()`  
  Agrega texto al final.

### Ejemplo (guardar y cargar)

```haskell
-- Guardar
guardarTexto :: FilePath -> String -> IO ()
guardarTexto ruta contenido = writeFile ruta contenido

-- Cargar
cargarTexto :: FilePath -> IO String
cargarTexto ruta = readFile ruta
```

En este proyecto se usa persistencia simple con `show`/`read`:

```haskell
-- Guardar una lista de registros:
writeFile "records.txt" (show registros)

-- Cargarla:
contenido <- readFile "records.txt"
let registros = read contenido :: [FinancialRecord]
```

Recomendación de curso: si se usa `read`, hay que tener cuidado con errores si el archivo está corrupto o vacío.

---

## 3) Tipos de datos algebraicos (`data`)

Los **tipos algebraicos** permiten modelar la información de forma clara y segura.

### a) Tipo enumeración (varias opciones)

```haskell
data RecordType = Income | Expense | Saving | Investment
  deriving (Show, Read, Eq)
```

Esto representa un valor que puede ser **una** de esas opciones.

### b) Tipo “registro” (con campos)

```haskell
data FinancialRecord = FinancialRecord
  { recordType  :: RecordType
  , amount      :: Double
  , category    :: String
  , date        :: String
  , description :: String
  , tags        :: [String]
  } deriving (Show, Read)
```

Beneficios:
- Campos con nombre (más legible).
- Validación de lógica usando `case` sobre constructores.

### Pattern matching (muy usado)

```haskell
contribucion :: FinancialRecord -> Double
contribucion r =
  case recordType r of
    Income  -> amount r
    Expense -> -amount r
    _       -> amount r
```

---

## 4) Funciones de orden superior (`map`, `filter`, `fold`)

Una función de orden superior recibe otra función como parámetro o devuelve una función.

### `map`
Aplica una función a cada elemento de una lista.

```haskell
map (+1) [1,2,3]  -- [2,3,4]
```

Ejemplo en finanzas: extraer montos:

```haskell
map amount registros
```

### `filter`
Filtra una lista dejando solo los elementos que cumplen una condición.

```haskell
filter even [1,2,3,4]  -- [2,4]
```

Ejemplo: solo gastos:

```haskell
filter (\r -> recordType r == Expense) registros
```

### `fold` (reducción/acumulación)
Convierte una lista en un solo valor acumulando.

Hay dos variantes típicas:
- `foldl` / `foldl'` (izquierda)
- `foldr` (derecha)

Ejemplo: sumar montos:

```haskell
sum (map amount registros)
```

o con `foldl`:

```haskell
foldl (\acc r -> acc + amount r) 0 registros
```

En proyectos de curso, muchas veces `sum`, `maximum`, `minimum` y `length` ya resuelven lo común sin necesidad de `fold` manual.

---

## 5) Manejo de listas y tuplas

### Listas

- Tipo: `[a]` (lista de elementos tipo `a`)
- Ejemplos: `[Int]`, `[String]`, `[FinancialRecord]`

Operaciones comunes:

```haskell
head [1,2,3]   -- 1  (ojo: falla si está vacía)
tail [1,2,3]   -- [2,3] (ojo: falla si está vacía)
null []        -- True
length [1,2,3] -- 3
```

Comprensión de listas (muy usada en el proyecto):

```haskell
[ amount r | r <- registros, recordType r == Expense ]
```

### Tuplas

Sirven para agrupar valores de distintos tipos sin crear un `data` nuevo.

```haskell
("2026-05", 120000.0, 80000.0, 40000.0)
-- (mes, ingresos, gastos, neto)
```

Acceso:
- `fst (a,b)` devuelve `a`
- `snd (a,b)` devuelve `b`

Pattern matching:

```haskell
let (mes, ing, gas, neto) = fila
```

---

## Relación con el proyecto

En este proyecto se usan estos conceptos para:

- **I/O**: menús en consola (`getLine`, `putStrLn`), validación de entradas.
- **Archivos**: persistencia de registros/presupuestos/reglas (`readFile`, `writeFile`).
- **Tipos algebraicos**: modelar registros financieros y reglas (`data`).
- **Orden superior**: filtrar y sumar por categorías/meses (`map`, `filter`, `sum`, comprensiones).
- **Listas/tuplas**: representar tablas de resultados (por ejemplo, flujo mensual).

