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

-- Umbral >= 0 (para mínimos de ahorro u otros límites no estrictamente positivos)
parseMontoNoNeg :: String -> Maybe Double
parseMontoNoNeg s =
  case reads (trim s) of
    [(x, rest)] | all isSpace rest, x >= 0 -> Just x
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

leerMontoNoNegativo :: String -> IO Double
leerMontoNoNegativo promptLine = do
  putStrLn promptLine
  line <- getLine
  case parseMontoNoNeg line of
    Just x -> return x
    Nothing -> do
      putStrLn "Valor inválido: debe ser un número mayor o igual a cero."
      leerMontoNoNegativo promptLine

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
    reglas <- loadRules
    menu registros presupuestos reglas

-- Menú principal del sistema
menu :: [FinancialRecord] -> [Budget] -> [Rule] -> IO ()
menu registros presupuestos reglas = do
    putStrLn "\n--- Sistema de Finanzas ---"
    putStrLn "1. Agregar ingreso"
    putStrLn "2. Agregar gasto"
    putStrLn "3. Agregar ahorro"
    putStrLn "4. Agregar inversión"
    putStrLn "5. Ver registros"
    putStrLn "6. Ver balance"
    putStrLn "7. Definir o actualizar presupuesto por categoría"
    putStrLn "8. Comparar presupuesto vs gastos reales (alertas)"
    putStrLn "9. Agregar regla del sistema"
    putStrLn "10. Evaluar reglas (alertas y advertencias)"
    putStrLn "11. Guardar y salir"
    opcion <- getLine

    case opcion of
        "1" -> agregarRegistro Income registros >>= \rs -> menu rs presupuestos reglas
        "2" -> agregarRegistro Expense registros >>= \rs -> menu rs presupuestos reglas
        "3" -> agregarRegistro Saving registros >>= \rs -> menu rs presupuestos reglas
        "4" -> agregarRegistro Investment registros >>= \rs -> menu rs presupuestos reglas
        "5" -> do
            mostrarRegistros registros
            menu registros presupuestos reglas
        "6" -> do
            putStrLn ("Balance: " ++ show (calcularBalance registros))
            menu registros presupuestos reglas
        "7" -> do
            ps <- agregarPresupuesto presupuestos
            menu registros ps reglas
        "8" -> do
            compararPresupuestosVsGastos presupuestos registros
            menu registros presupuestos reglas
        "9" -> do
            rs <- agregarRegla reglas
            menu registros presupuestos rs
        "10" -> do
            evaluarReglasEnPantalla reglas registros
            menu registros presupuestos reglas
        "11" -> do
            saveRecords registros
            saveBudgets presupuestos
            saveRules reglas
            putStrLn "Datos guardados (registros, presupuestos y reglas). Chao mae"
        _ -> do
            putStrLn "Opción inválida"
            menu registros presupuestos reglas

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

-- Alta de reglas configurables (se acumulan en memoria hasta guardar con 11)
agregarRegla :: [Rule] -> IO [Rule]
agregarRegla rs = do
    putStrLn "\n--- Agregar regla ---"
    putStrLn "1. Si gasto en categoría supera un monto → alerta"
    putStrLn "2. Si ahorro total (suma de Saving) es menor a un mínimo → advertencia"
    putStrLn "0. Volver sin cambios"
    sub <- getLine
    case trim sub of
        "0" -> return rs
        "1" -> do
            putStrLn "Categoría a vigilar (igual que en gastos; sin importar mayúsculas al evaluar):"
            cat <- leerCategoria
            lim <-
                leerMontoPositivo
                    "Umbral: se dispara la alerta si la suma de gastos en esa categoría es mayor a:"
            putStrLn "Regla agregada (recuerde guardar con la opción 11)."
            return (rs ++ [RuleGastoEnCategoriaMayor cat lim])
        "2" -> do
            minimo <-
                leerMontoNoNegativo
                    "Mínimo de ahorro total deseado (>= 0). Advertencia si la suma de registros Saving queda por debajo:"
            putStrLn "Regla agregada (recuerde guardar con la opción 11)."
            return (rs ++ [RuleAhorroTotalMenor minimo])
        _ -> do
            putStrLn "Opción inválida."
            agregarRegla rs

evaluarReglasEnPantalla :: [Rule] -> [FinancialRecord] -> IO ()
evaluarReglasEnPantalla [] _ =
    putStrLn "No hay reglas definidas. Use la opción 9."
evaluarReglasEnPantalla rs regs = do
    putStrLn "\n--- Reglas definidas ---"
    mapM_
        ( \(i, r) ->
            putStrLn (show (i :: Int) ++ ". " ++ describirRegla r)
        )
        (zip [1 ..] rs)
    putStrLn "\n--- Resultado de la evaluación ---"
    case evaluarReglas rs regs of
        [] -> putStrLn "Ninguna regla disparada (todo OK según los datos actuales)."
        msgs -> mapM_ putStrLn msgs

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
