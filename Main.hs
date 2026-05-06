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

esMesValido :: String -> Bool
esMesValido s =
  length s == 7
    && s !! 4 == '-'
    && all (`elem` ['0' .. '9']) [s !! 0, s !! 1, s !! 2, s !! 3, s !! 5, s !! 6]
    && let mo = read [s !! 5, s !! 6] :: Int
        in mo >= 1 && mo <= 12

leerMesValido :: String -> IO String
leerMesValido promptLine = do
  putStrLn promptLine
  line <- getLine
  let m = trim line
  if esMesValido m
    then return m
    else do
      putStrLn "Mes inválido. Use formato YYYY-MM (ej: 2026-05)."
      leerMesValido promptLine

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
    putStrLn "11. Análisis financiero avanzado (2.3)"
    putStrLn "12. Simulación financiera (2.4)"
    putStrLn "13. Reportes (2.7)"
    putStrLn "14. Guardar y salir"
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
            mostrarAnalisisAvanzado registros
            menu registros presupuestos reglas
        "12" -> do
            mostrarSimulacionFinanciera registros
            menu registros presupuestos reglas
        "13" -> do
            mostrarReportes registros
            menu registros presupuestos reglas
        "14" -> do
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

mostrarAnalisisAvanzado :: [FinancialRecord] -> IO ()
mostrarAnalisisAvanzado regs = do
    putStrLn "\n=== Análisis financiero avanzado (2.3) ==="
    putStrLn "\n--- Flujo de caja mensual ---"
    let flujo = resumenMensual regs
    if null flujo
        then putStrLn "No hay datos suficientes para flujo mensual."
        else
            mapM_
                ( \(m, ing, gas, neto) ->
                    putStrLn
                        ( m
                            ++ " | ingresos: "
                            ++ show ing
                            ++ " | gastos: "
                            ++ show gas
                            ++ " | neto: "
                            ++ show neto
                        )
                )
                flujo

    putStrLn "\n--- Tendencia de gasto mensual ---"
    let tend = tendenciaGastoMensual regs
    if null tend
        then putStrLn "No hay suficientes meses con gasto para calcular tendencia."
        else
            mapM_
                ( \(m, act, prev, var) ->
                    putStrLn
                        ( m
                            ++ " | gasto anterior: "
                            ++ show prev
                            ++ " | gasto actual: "
                            ++ show act
                            ++ " | variación: "
                            ++ show var
                        )
                )
                tend

    putStrLn "\n--- Proyección de gasto próximo mes ---"
    case proyeccionGastoSiguienteMes regs of
        Nothing -> putStrLn "No hay histórico de gastos para proyectar."
        Just p -> putStrLn ("Proyección estimada (promedio histórico): " ++ show p)

    putStrLn "\n--- Top 5 categorías con mayor gasto ---"
    let top5 = topCategoriasGasto 5 regs
    if null top5
        then putStrLn "No hay gastos registrados por categoría."
        else
            mapM_
                ( \(c, v) ->
                    putStrLn ("Categoría: " ++ c ++ " | gasto acumulado: " ++ show v)
                )
                top5

mostrarSimulacionFinanciera :: [FinancialRecord] -> IO ()
mostrarSimulacionFinanciera regs = do
    putStrLn "\n=== Simulación financiera (2.4) ==="
    putStrLn "1. Simular reducción de gastos (%)"
    putStrLn "2. Proyección de ahorro en el tiempo (meses)"
    putStrLn "0. Volver"
    sub <- getLine
    case trim sub of
        "0" -> return ()
        "1" -> do
            porcentaje <-
                leerMontoNoNegativo
                    "Porcentaje de reducción de gastos (0 a 100, ej: 10):"
            let (gActual, gReducido, ahorro, bActual, bSimulado) =
                    simularReduccionGastos porcentaje regs
            putStrLn "\n--- Resultado de simulación ---"
            putStrLn ("Gasto actual total: " ++ show gActual)
            putStrLn ("Gasto total simulado: " ++ show gReducido)
            putStrLn ("Ahorro estimado por reducción: " ++ show ahorro)
            putStrLn ("Balance actual: " ++ show bActual)
            putStrLn ("Balance simulado: " ++ show bSimulado)
        "2" -> do
            mesesD <-
                leerMontoPositivo
                    "Cantidad de meses a proyectar (número entero positivo, ej: 6):"
            let meses = floor mesesD
            putStrLn ("\nSe proyectará para " ++ show meses ++ " meses.")
            case proyeccionAhorroEnMeses meses regs of
                Nothing -> putStrLn "No hay datos suficientes para proyectar ahorro."
                Just filas -> do
                    putStrLn "\n--- Proyección de ahorro acumulado ---"
                    mapM_
                        ( \(m, a) ->
                            putStrLn ("Mes " ++ show m ++ ": ahorro acumulado estimado = " ++ show a)
                        )
                        filas
        _ -> do
            putStrLn "Opción inválida."
            mostrarSimulacionFinanciera regs

mostrarReportes :: [FinancialRecord] -> IO ()
mostrarReportes regs = do
    putStrLn "\n=== Reportes (2.7) ==="
    putStrLn "1. Resumen mensual"
    putStrLn "2. Comparación entre periodos (mes vs mes)"
    putStrLn "3. Categorías con mayor gasto"
    putStrLn "0. Volver"
    sub <- getLine
    case trim sub of
        "0" -> return ()
        "1" -> do
            putStrLn "\n--- Resumen mensual ---"
            let flujo = resumenMensual regs
            if null flujo
                then putStrLn "No hay datos para generar el resumen mensual."
                else
                    mapM_
                        ( \(m, ing, gas, neto) ->
                            putStrLn
                                ( m
                                    ++ " | ingresos: "
                                    ++ show ing
                                    ++ " | gastos: "
                                    ++ show gas
                                    ++ " | neto: "
                                    ++ show neto
                                )
                        )
                        flujo
        "2" -> do
            putStrLn "\n--- Comparación entre periodos ---"
            m1 <- leerMesValido "Primer periodo (YYYY-MM):"
            m2 <- leerMesValido "Segundo periodo (YYYY-MM):"
            case compararPeriodos m1 m2 regs of
                Nothing ->
                    putStrLn
                        "No se pudo comparar: revise que ambos meses existan en los registros."
                Just (ing1, ing2, vIng, gas1, gas2, vGas, neto1, neto2, vNeto) -> do
                    putStrLn ("Ingresos: " ++ m1 ++ "=" ++ show ing1 ++ " | " ++ m2 ++ "=" ++ show ing2 ++ " | variación=" ++ show vIng)
                    putStrLn ("Gastos:   " ++ m1 ++ "=" ++ show gas1 ++ " | " ++ m2 ++ "=" ++ show gas2 ++ " | variación=" ++ show vGas)
                    putStrLn ("Neto:     " ++ m1 ++ "=" ++ show neto1 ++ " | " ++ m2 ++ "=" ++ show neto2 ++ " | variación=" ++ show vNeto)
        "3" -> do
            putStrLn "\n--- Categorías con mayor gasto ---"
            let tops = topCategoriasGasto 5 regs
            if null tops
                then putStrLn "No hay gastos registrados por categoría."
                else
                    mapM_
                        ( \(c, v) ->
                            putStrLn ("Categoría: " ++ c ++ " | gasto acumulado: " ++ show v)
                        )
                        tops
        _ -> do
            putStrLn "Opción inválida."
            mostrarReportes regs
