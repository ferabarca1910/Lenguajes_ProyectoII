module Main where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Text.Read (readMaybe)

import Types
import Storage
import Logic

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy c s = go s []
  where
    go [] acc = [reverse acc]
    go (x : xs) acc
      | x == c = reverse acc : go xs []
      | otherwise = go xs (x : acc)

parseMonto :: String -> Maybe Double
parseMonto s =
  case reads (trim s) of
    [(x, rest)] | all isSpace rest, x > 0 -> Just x
    _ -> Nothing

diasEnMes :: Int -> Int -> Int
diasEnMes y m = case m of
  1 -> 31
  2 -> if bisiesto y then 29 else 28
  3 -> 31
  4 -> 30
  5 -> 31
  6 -> 30
  7 -> 31
  8 -> 31
  9 -> 30
  10 -> 31
  11 -> 30
  12 -> 31
  _ -> 0

bisiesto :: Int -> Bool
bisiesto y =
  (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0)

parseFecha :: String -> Maybe String
parseFecha raw =
  case mapM readMaybe (splitBy '-' (trim raw)) :: Maybe [Int] of
    Just [y, mo, d]
      | y >= 1900
          && y <= 2100
          && mo >= 1
          && mo <= 12
          && d >= 1
          && d <= diasEnMes y mo ->
          Just (printfFecha y mo d)
    _ -> Nothing

printfFecha :: Int -> Int -> Int -> String
printfFecha y mo d =
  pad 4 y ++ "-" ++ pad 2 mo ++ "-" ++ pad 2 d
  where
    pad w n = replicate (w - length s) '0' ++ s
      where
        s = show n

parseTags :: String -> [String]
parseTags = filter (not . null) . map trim . splitBy ','

leerMontoPositivo :: String -> IO Double
leerMontoPositivo promptLine = do
  putStrLn promptLine
  line <- getLine
  case parseMonto line of
    Just x -> return x
    Nothing -> do
      putStrLn "Monto inválido: debe ser un número mayor que cero."
      leerMontoPositivo promptLine

leerMontoValido :: IO Double
leerMontoValido =
  leerMontoPositivo "Monto (número positivo, ej: 1500 o 99.5):"

leerCategoria :: IO String
leerCategoria = do
  putStrLn "Categoría (texto no vacío):"
  line <- getLine
  case trim line of
    "" -> do
      putStrLn "La categoría no puede estar vacía."
      leerCategoria
    c -> return c

leerFechaValida :: IO String
leerFechaValida = do
  putStrLn "Fecha en formato AAAA-MM-DD (ej: 2026-05-12):"
  line <- getLine
  case parseFecha line of
    Just f -> return f
    Nothing -> do
      putStrLn "Fecha inválida: revise año, mes, día y el formato."
      leerFechaValida

leerTags :: IO [String]
leerTags = do
  putStrLn "Etiquetas separadas por coma (Enter = ninguna, ej: fijo, variable):"
  line <- getLine
  return (parseTags line)

-- Función principal que inicia el programa
main :: IO ()
main = do
    registros <- loadRecords
    presupuestos <- loadBudgets
    menu registros presupuestos

-- Menú principal del sistema
menu :: [FinancialRecord] -> [Budget] -> IO ()
menu registros presupuestos = do
    putStrLn "\n--- Sistema de Finanzas ---"
    putStrLn "1. Agregar ingreso"
    putStrLn "2. Agregar gasto"
    putStrLn "3. Agregar ahorro"
    putStrLn "4. Agregar inversión"
    putStrLn "5. Ver registros"
    putStrLn "6. Ver balance"
    putStrLn "7. Definir o actualizar presupuesto por categoría"
    putStrLn "8. Comparar presupuesto vs gastos reales (alertas)"
    putStrLn "9. Guardar y salir"
    opcion <- getLine

    case opcion of
        "1" -> agregarRegistro Income registros >>= \rs -> menu rs presupuestos
        "2" -> agregarRegistro Expense registros >>= \rs -> menu rs presupuestos
        "3" -> agregarRegistro Saving registros >>= \rs -> menu rs presupuestos
        "4" -> agregarRegistro Investment registros >>= \rs -> menu rs presupuestos
        "5" -> do
            mostrarRegistros registros
            menu registros presupuestos
        "6" -> do
            putStrLn ("Balance: " ++ show (calcularBalance registros))
            menu registros presupuestos
        "7" -> do
            ps <- agregarPresupuesto presupuestos
            menu registros ps
        "8" -> do
            compararPresupuestosVsGastos presupuestos registros
            menu registros presupuestos
        "9" -> do
            saveRecords registros
            saveBudgets presupuestos
            putStrLn "Datos guardados (registros y presupuestos). Chao mae"
        _ -> do
            putStrLn "Opción inválida"
            menu registros presupuestos

-- Crea un nuevo registro financiero (monto, categoría, fecha y tags validados)
agregarRegistro :: RecordType -> [FinancialRecord] -> IO [FinancialRecord]
agregarRegistro tipo registros = do
    monto <- leerMontoValido
    cat <- leerCategoria
    fecha <- leerFechaValida
    putStrLn "Descripción:"
    desc <- getLine
    tgs <- leerTags

    let nuevo = FinancialRecord tipo monto cat fecha (trim desc) tgs

    return (registros ++ [nuevo])

-- Define o actualiza el tope de gasto para una categoría (solo compara contra registros tipo Gasto)
agregarPresupuesto :: [Budget] -> IO [Budget]
agregarPresupuesto bs = do
    putStrLn "Categoría a presupuestar (debe ser la misma que usás en gastos, sin importar mayúsculas):"
    cat <- leerCategoria
    lim <-
        leerMontoPositivo
            "Tope máximo de gasto para esa categoría (número positivo, ej: 50000):"
    let b = Budget cat lim
    putStrLn ("Listo. Presupuesto para \"" ++ cat ++ "\": " ++ show lim)
    return (upsertBudget b bs)

-- Tabla real vs presupuesto y líneas ALERTA si hay exceso
compararPresupuestosVsGastos :: [Budget] -> [FinancialRecord] -> IO ()
compararPresupuestosVsGastos [] _ =
    putStrLn "No hay presupuestos definidos. Use la opción 7 primero."
compararPresupuestosVsGastos bs regs = do
    putStrLn "\n--- Presupuesto vs gastos reales (solo Expense) ---"
    mapM_ (fila regs) bs
    let als = alertasPresupuesto bs regs
    if null als
        then putStrLn "\nSin alertas: ningún presupuesto superado."
        else do
            putStrLn "\n--- Alertas ---"
            mapM_ putStrLn als
  where
    fila rs b = do
        let real = gastoRealEnCategoria (budgetCategory b) rs
            tope = budgetLimit b
            restante = tope - real
        putStrLn "------------------------"
        putStrLn ("Categoría: " ++ budgetCategory b)
        putStrLn ("Presupuesto (tope): " ++ show tope)
        putStrLn ("Gasto real acumulado: " ++ show real)
        putStrLn ("Diferencia (tope - gasto): " ++ show restante)
        if real > tope
            then putStrLn "Estado: EXCEDIDO"
            else putStrLn "Estado: dentro del presupuesto"

-- Muestra todos los registros de forma más legible
mostrarRegistros :: [FinancialRecord] -> IO ()
mostrarRegistros [] = putStrLn "No hay registros"
mostrarRegistros regs = mapM_ mostrarUno regs

-- Muestra un solo registro con formato
mostrarUno :: FinancialRecord -> IO ()
mostrarUno r = do
    putStrLn "------------------------"
    putStrLn ("Tipo: " ++ show (recordType r))
    putStrLn ("Monto: " ++ show (amount r))
    putStrLn ("Categoría: " ++ category r)
    putStrLn ("Fecha: " ++ date r)
    putStrLn ("Descripción: " ++ description r)
    putStrLn ("Tags: " ++ show (tags r))
