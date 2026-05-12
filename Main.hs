{- | Punto de entrada y capa de IO: menú de consola, validación de entradas y llamadas a 'Storage' y 'Logic'.
-}
module Main where

import Control.Exception (IOException, try)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Text.Read (readMaybe)

import Types
import Storage
import Logic

-- | Intenta escribir @records.txt@; no aborta el programa si el archivo está bloqueado.
intentarSaveRecords :: String -> [FinancialRecord] -> IO ()
intentarSaveRecords msgExito rs = do
  r <- (try $ saveRecords rs) :: IO (Either IOException ())
  case r of
    Left e -> do
      putStrLn "No se pudo guardar records.txt."
      putStrLn "  Causa habitual: el archivo está abierto en el editor (cerrá la pestaña records.txt)."
      putStrLn ("  " ++ show e)
    Right () -> putStrLn msgExito

-- | Elimina espacios iniciales y finales.
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- | Prefijo numérico del menú (acepta @7.@, @ 8 @, etc.).
opcMenu :: String -> String
opcMenu = takeWhile (`elem` ['0' .. '9']) . trim

-- | Parte la cadena en tokens por un delimitador (no incluye vacíos consecutivos como segmentos vacíos al inicio: ver implementación).
splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy c s = go s []
  where
    go [] acc = [reverse acc]
    go (x : xs) acc
      | x == c = reverse acc : go xs []
      | otherwise = go xs (x : acc)

-- | Parsea un número estrictamente positivo (resto de la línea solo espacios).
parseMonto :: String -> Maybe Double
parseMonto s =
  case reads (trim s) of
    [(x, rest)] | all isSpace rest, x > 0 -> Just x
    _ -> Nothing

-- | Como 'parseMonto' pero admite cero (@>= 0@).
parseMontoNoNeg :: String -> Maybe Double
parseMontoNoNeg s =
  case reads (trim s) of
    [(x, rest)] | all isSpace rest, x >= 0 -> Just x
    _ -> Nothing

-- | Cantidad de días del mes @m@ en año @y@ (1–12; otro mes devuelve 0).
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

-- | Año bisiesto gregoriano.
bisiesto :: Int -> Bool
bisiesto y =
  (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0)

-- | Valida @AAAA-MM-DD@ y devuelve la misma fecha normalizada con ceros a la izquierda.
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

-- | Formato @YYYY-MM-DD@ con ancho fijo de dígitos.
printfFecha :: Int -> Int -> Int -> String
printfFecha y mo d =
  pad 4 y ++ "-" ++ pad 2 mo ++ "-" ++ pad 2 d
  where
    pad w n = replicate (w - length s) '0' ++ s
      where
        s = show n

-- | Etiquetas separadas por comas; se recortan y se omiten vacías.
parseTags :: String -> [String]
parseTags = filter (not . null) . map trim . splitBy ','

-- | @True@ si la cadena tiene forma @YYYY-MM@ con mes 01–12.
esMesValido :: String -> Bool
esMesValido s =
  length s == 7
    && s !! 4 == '-'
    && all (`elem` ['0' .. '9']) [s !! 0, s !! 1, s !! 2, s !! 3, s !! 5, s !! 6]
    && let mo = read [s !! 5, s !! 6] :: Int
        in mo >= 1 && mo <= 12

-- | Pide un mes @YYYY-MM@ hasta obtener uno válido.
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

-- | Pide un monto > 0 hasta parsear correctamente.
leerMontoPositivo :: String -> IO Double
leerMontoPositivo promptLine = do
  putStrLn promptLine
  line <- getLine
  case parseMonto line of
    Just x -> return x
    Nothing -> do
      putStrLn "Monto inválido: debe ser un número mayor que cero."
      leerMontoPositivo promptLine

-- | Atajo: monto positivo con prompt fijo del flujo de registro.
leerMontoValido :: IO Double
leerMontoValido =
  leerMontoPositivo "Monto (número positivo, ej: 1500 o 99.5):"

-- | Pide un número @>= 0@.
leerMontoNoNegativo :: String -> IO Double
leerMontoNoNegativo promptLine = do
  putStrLn promptLine
  line <- getLine
  case parseMontoNoNeg line of
    Just x -> return x
    Nothing -> do
      putStrLn "Valor inválido: debe ser un número mayor o igual a cero."
      leerMontoNoNegativo promptLine

-- | Categoría no vacía (tras trim).
leerCategoria :: IO String
leerCategoria = do
  putStrLn "Categoría (texto no vacío):"
  line <- getLine
  case trim line of
    "" -> do
      putStrLn "La categoría no puede estar vacía."
      leerCategoria
    c -> return c

-- | Fecha válida en consola (@AAAA-MM-DD@).
leerFechaValida :: IO String
leerFechaValida = do
  putStrLn "Fecha en formato AAAA-MM-DD (ej: 2026-05-12):"
  line <- getLine
  case parseFecha line of
    Just f -> return f
    Nothing -> do
      putStrLn "Fecha inválida: revise año, mes, día y el formato."
      leerFechaValida

-- | Lista de etiquetas desde una línea (puede ser vacía).
leerTags :: IO [String]
leerTags = do
  putStrLn "Etiquetas separadas por coma (Enter = ninguna, ej: fijo, variable):"
  line <- getLine
  return (parseTags line)

-- | Carga datos, entra al bucle del menú principal.
main :: IO ()
main = do
    registros <- loadRecords
    presupuestos <- loadBudgets
    reglas <- loadRules
    menu registros presupuestos reglas

-- | Menú interactivo: la opción 14 guarda solo registros en disco y termina.
menu :: [FinancialRecord] -> [Budget] -> [Rule] -> IO ()
menu registros presupuestos reglas = do
    putStrLn "\n--- Sistema de Finanzas ---"
    putStrLn "1. Agregar ingreso"
    putStrLn "2. Agregar gasto"
    putStrLn "3. Agregar ahorro"
    putStrLn "4. Agregar inversión"
    putStrLn "5. Ver registros"
    putStrLn "6. Ver balance"
    putStrLn "7. [2.2] Definir o actualizar presupuesto por categoría"
    putStrLn "8. [2.2] Comparar gastos reales vs presupuesto (incluye alertas si hay exceso)"
    putStrLn "9. [2.5] Agregar regla del sistema (gasto en categoria / ahorro minimo)"
    putStrLn "10. [2.5] Evaluar reglas (alertas y advertencias con datos actuales)"
    putStrLn "11. [2.3] Analisis financiero avanzado"
    putStrLn "12. Simulación financiera"
    putStrLn "13. Reportes"
    putStrLn "14. Guardar registros (records.txt) y salir"
    opcion <- getLine

    case opcMenu opcion of
        "1" ->
            agregarRegistro Income registros >>= \rs -> do
                intentarSaveRecords "Registro guardado en records.txt." rs
                menu rs presupuestos reglas
        "2" ->
            agregarRegistro Expense registros >>= \rs -> do
                intentarSaveRecords "Registro guardado en records.txt." rs
                menu rs presupuestos reglas
        "3" ->
            agregarRegistro Saving registros >>= \rs -> do
                intentarSaveRecords "Registro guardado en records.txt." rs
                menu rs presupuestos reglas
        "4" ->
            agregarRegistro Investment registros >>= \rs -> do
                intentarSaveRecords "Registro guardado en records.txt." rs
                menu rs presupuestos reglas
        "5" -> do
            mostrarRegistros registros
            menu registros presupuestos reglas
        "6" -> do
            putStrLn ("Balance: " ++ show (calcularBalance registros))
            menu registros presupuestos reglas
        "7" -> do
            ps <- agregarPresupuesto presupuestos
            mostrarAlertasPresupuesto2_2 ps registros
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
            intentarSaveRecords "Registros guardados en records.txt." registros
            putStrLn "Nota: presupuestos y reglas de esta sesión no se guardan en archivo. Chao mae"
        _ -> do
            putStrLn "Opción inválida"
            menu registros presupuestos reglas

-- | Alta de un movimiento del tipo indicado; devuelve la lista extendida.
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

-- | Tras definir un presupuesto: aviso si ya hay gastos (Expense) que superan el tope (requisito 2.2).
mostrarAlertasPresupuesto2_2 :: [Budget] -> [FinancialRecord] -> IO ()
mostrarAlertasPresupuesto2_2 bs regs = do
    putStrLn ""
    putStrLn "--- 2.2 Alertas por exceso de presupuesto (con datos actuales) ---"
    let als = alertasPresupuesto bs regs
    if null als
        then putStrLn "Sin exceso: ningun tope superado por los gastos registrados hasta ahora."
        else mapM_ putStrLn als

-- | Define o actualiza tope de gasto por categoría ('Logic.upsertBudget').
agregarPresupuesto :: [Budget] -> IO [Budget]
agregarPresupuesto bs = do
    putStrLn "Categoria a presupuestar (debe ser la misma que usas en gastos, sin importar mayusculas):"
    cat <- leerCategoria
    lim <-
        leerMontoPositivo
            "Tope maximo de gasto para esa categoria (numero positivo, ej: 50000):"
    let b = Budget cat lim
    putStrLn ("Listo. Presupuesto para \"" ++ cat ++ "\": " ++ show lim)
    return (upsertBudget b bs)

-- | Tabla presupuesto vs gasto real y alertas por exceso.
compararPresupuestosVsGastos :: [Budget] -> [FinancialRecord] -> IO ()
compararPresupuestosVsGastos [] _ =
    putStrLn "No hay presupuestos definidos. Use la opcion 7 primero."
compararPresupuestosVsGastos bs regs = do
    putStrLn "\n=== 2.2 Presupuestos: comparación real vs tope y alertas ==="
    putStrLn "(Solo se suman gastos tipo Expense por categoria, misma logica que al definir el presupuesto.)\n"
    mapM_ (fila regs) bs
    let als = alertasPresupuesto bs regs
    if null als
        then putStrLn "\nSin alertas: ningun presupuesto superado."
        else do
            putStrLn "\n--- Alertas ---"
            mapM_ putStrLn als
  where
    fila rs b = do
        let real = gastoRealEnCategoria (budgetCategory b) rs
            tope = budgetLimit b
            restante = tope - real
        putStrLn "------------------------"
        putStrLn ("Categoria: " ++ budgetCategory b)
        putStrLn ("Presupuesto (tope): " ++ show tope)
        putStrLn ("Gasto real acumulado: " ++ show real)
        putStrLn ("Diferencia (tope - gasto): " ++ show restante)
        if real > tope
            then putStrLn "Estado: EXCEDIDO"
            else putStrLn "Estado: dentro del presupuesto"

-- | Alta de reglas (solo en memoria; no se persisten en archivo).
agregarRegla :: [Rule] -> IO [Rule]
agregarRegla rs = do
    putStrLn "\n--- Agregar regla ---"
    putStrLn "1. Si gasto en categoria supera un monto -> alerta"
    putStrLn "2. Si ahorro total (suma de Saving) es menor a un minimo -> advertencia"
    putStrLn "0. Volver sin cambios"
    sub <- getLine
    case trim sub of
        "0" -> return rs
        "1" -> do
            putStrLn "Categoria a vigilar (igual que en gastos; sin importar mayusculas al evaluar):"
            cat <- leerCategoria
            lim <-
                leerMontoPositivo
                    "Umbral: se dispara la alerta si la suma de gastos en esa categoria es mayor a:"
            putStrLn "Regla agregada (solo en memoria hasta cerrar el programa)."
            return (rs ++ [RuleGastoEnCategoriaMayor cat lim])
        "2" -> do
            minimo <-
                leerMontoNoNegativo
                    "Minimo de ahorro total deseado (>= 0). Advertencia si la suma de registros Saving queda por debajo:"
            putStrLn "Regla agregada (solo en memoria hasta cerrar el programa)."
            return (rs ++ [RuleAhorroTotalMenor minimo])
        _ -> do
            putStrLn "Opcion invalida."
            agregarRegla rs

-- | Lista reglas y muestra el resultado de 'Logic.evaluarReglas'.
evaluarReglasEnPantalla :: [Rule] -> [FinancialRecord] -> IO ()
evaluarReglasEnPantalla [] _ =
    putStrLn "No hay reglas definidas. Use la opcion 9."
evaluarReglasEnPantalla rs regs = do
    putStrLn "\n--- Reglas definidas ---"
    mapM_
        ( \(i, r) ->
            putStrLn (show (i :: Int) ++ ". " ++ describirRegla r)
        )
        (zip [1 ..] rs)
    putStrLn "\n--- Resultado de la evaluacion ---"
    case evaluarReglas rs regs of
        [] -> putStrLn "Ninguna regla disparada (todo OK segun los datos actuales)."
        msgs -> mapM_ putStrLn msgs

-- | Imprime todos los registros (o aviso si no hay).
mostrarRegistros :: [FinancialRecord] -> IO ()
mostrarRegistros [] = putStrLn "No hay registros"
mostrarRegistros regs = mapM_ mostrarUno regs

-- | Detalle formateado de un 'FinancialRecord'.
mostrarUno :: FinancialRecord -> IO ()
mostrarUno r = do
    putStrLn "------------------------"
    putStrLn ("Tipo: " ++ show (recordType r))
    putStrLn ("Monto: " ++ show (amount r))
    putStrLn ("Categoría: " ++ category r)
    putStrLn ("Fecha: " ++ date r)
    putStrLn ("Descripción: " ++ description r)
    putStrLn ("Tags: " ++ show (tags r))

-- | Flujo mensual, tendencia de gasto, proyeccion y top categorias (requisito 2.3).
mostrarAnalisisAvanzado :: [FinancialRecord] -> IO ()
mostrarAnalisisAvanzado regs = do
    putStrLn "\n=== Analisis financiero avanzado (2.3) ==="
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
                            ++ " | variacion: "
                            ++ show var
                        )
                )
                tend

    putStrLn "\n--- Proyeccion de gasto proximo mes ---"
    case proyeccionGastoSiguienteMes regs of
        Nothing -> putStrLn "No hay historico de gastos para proyectar."
        Just p -> putStrLn ("Proyeccion estimada (promedio historico de gastos mensuales): " ++ show p)

    putStrLn "\n--- Top 5 categorias con mayor gasto (impacto en egresos) ---"
    let top5 = topCategoriasGasto 5 regs
    if null top5
        then putStrLn "No hay gastos registrados por categoria."
        else
            mapM_
                ( \(c, v) ->
                    putStrLn ("Categoria: " ++ c ++ " | gasto acumulado: " ++ show v)
                )
                top5

-- | Submenú: simulación de recorte de gastos y proyección de ahorro en meses (2.4).
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

-- | Submenú: resumen mensual, comparación mes a mes y top gastos (2.7).
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
