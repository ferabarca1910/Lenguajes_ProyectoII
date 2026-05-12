{- | Lógica pura: normalización de categorías, balance, presupuestos, reglas,
     análisis por mes, simulación y reportes. Sin efectos de IO.
-}
module Logic where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, nub, sort)

import Types

-- | Quita espacios al inicio y al final (útil para nombres de categoría).
trimCat :: String -> String
trimCat = dropWhile isSpace . dropWhileEnd isSpace

-- | Normaliza categoría: trim + minúsculas, para comparar presupuestos y gastos.
categoriaNorm :: String -> String
categoriaNorm = map toLower . trimCat

-- | Suma los 'Expense' cuya categoría normalizada coincide con la referencia.
gastoRealEnCategoria :: String -> [FinancialRecord] -> Double
gastoRealEnCategoria catPresupuesto regs =
  sum
    [ amount r
    | r <- regs
    , recordType r == Expense
    , categoriaNorm (category r) == ref
    ]
  where
    ref = categoriaNorm catPresupuesto

-- | 'True' si el gasto real en la categoría del presupuesto supera el tope.
excedePresupuesto :: Budget -> [FinancialRecord] -> Bool
excedePresupuesto b regs = gastoRealEnCategoria (budgetCategory b) regs > budgetLimit b

-- | Mensajes de alerta por cada presupuesto cuyo gasto real excede el límite.
alertasPresupuesto :: [Budget] -> [FinancialRecord] -> [String]
alertasPresupuesto bs regs =
  [ msg b
  | b <- bs
  , excedePresupuesto b regs
  ]
  where
    msg b =
      "ALERTA: categoría \""
        ++ budgetCategory b
        ++ "\" — gasto real "
        ++ show (gastoRealEnCategoria (budgetCategory b) regs)
        ++ " > presupuesto "
        ++ show (budgetLimit b)

-- | Balance neto: ingresos, ahorros e inversiones suman; los gastos restan.
calcularBalance :: [FinancialRecord] -> Double
calcularBalance = sum . map contribucion
  where
    contribucion r = case recordType r of
        Income     -> amount r
        Expense    -> -amount r
        Saving     -> amount r
        Investment -> amount r

-- | Inserta un presupuesto o reemplaza el existente con la misma categoría normalizada.
upsertBudget :: Budget -> [Budget] -> [Budget]
upsertBudget b [] = [b]
upsertBudget b (x : xs)
  | categoriaNorm (budgetCategory b) == categoriaNorm (budgetCategory x) =
      b : xs
  | otherwise = x : upsertBudget b xs

-- | Suma de montos de todos los registros tipo 'Saving'.
totalAhorroRegistrado :: [FinancialRecord] -> Double
totalAhorroRegistrado regs =
  sum [amount r | r <- regs, recordType r == Saving]

-- | Evalúa una regla; 'Just' contiene el mensaje si la condición se cumple.
evaluarRegla :: Rule -> [FinancialRecord] -> Maybe String
evaluarRegla (RuleGastoEnCategoriaMayor cat lim) regs =
  let g = gastoRealEnCategoria cat regs
   in if g > lim
        then
          Just
            ( "[ALERTA] Gastos en categoría \""
                ++ cat
                ++ "\" suman "
                ++ show g
                ++ " (supera el umbral "
                ++ show lim
                ++ ")"
            )
        else Nothing
evaluarRegla (RuleAhorroTotalMenor minimo) regs =
  let a = totalAhorroRegistrado regs
   in if a < minimo
        then
          Just
            ( "[ADVERTENCIA] Ahorro total registrado "
                ++ show a
                ++ " es menor al mínimo "
                ++ show minimo
            )
        else Nothing

-- | Evalúa todas las reglas y concatena los mensajes de las que disparan.
evaluarReglas :: [Rule] -> [FinancialRecord] -> [String]
evaluarReglas rs regs = concatMap f rs
  where
    f r = case evaluarRegla r regs of
      Nothing -> []
      Just m -> [m]

-- | Descripción legible de la regla (para listados en consola).
describirRegla :: Rule -> String
describirRegla (RuleGastoEnCategoriaMayor c x) =
  "Alerta si gastos (Expense) en categoría \"" ++ c ++ "\" superan " ++ show x
describirRegla (RuleAhorroTotalMenor m) =
  "Advertencia si la suma de ahorros (Saving) es menor a " ++ show m

-- | Prefijo @YYYY-MM@ de una fecha @YYYY-MM-DD@ (si es corta, devuelve la cadena tal cual).
mesDeFecha :: String -> String
mesDeFecha f
  | length f >= 7 = take 7 f
  | otherwise = f

-- | Meses @YYYY-MM@ distintos presentes en los registros, ordenados lexicográficamente.
mesesUnicosOrdenados :: [FinancialRecord] -> [String]
mesesUnicosOrdenados regs = sort (nub [mesDeFecha (date r) | r <- regs])

-- | Por cada mes: @(mes, ingresos, gastos, neto)@ con neto = ingresos − gastos.
resumenMensual :: [FinancialRecord] -> [(String, Double, Double, Double)]
resumenMensual regs =
  [ (mes, ingresos, gastos, ingresos - gastos)
  | mes <- meses
  , let ingresos = sumaIncome mes
  , let gastos = sumaExpense mes
  ]
  where
    meses = mesesUnicosOrdenados regs
    sumaIncome m =
      sum [amount r | r <- regs, mesDeFecha (date r) == m, recordType r == Income]
    sumaExpense m =
      sum [amount r | r <- regs, mesDeFecha (date r) == m, recordType r == Expense]

-- | Suma de 'Expense' por mes @YYYY-MM@.
gastosPorMes :: [FinancialRecord] -> [(String, Double)]
gastosPorMes regs =
  [ (m, sum [amount r | r <- regs, recordType r == Expense, mesDeFecha (date r) == m])
  | m <- mesesUnicosOrdenados regs
  ]

-- | Por cada mes (salvo el primero): gasto del mes anterior, actual, y variación (actual − anterior).
tendenciaGastoMensual :: [FinancialRecord] -> [(String, Double, Double, Double)]
tendenciaGastoMensual regs =
  [ (mesActual, gastoActual, gastoPrevio, gastoActual - gastoPrevio)
  | ((_, gastoPrevio), (mesActual, gastoActual)) <- zip gs (drop 1 gs)
  ]
  where
    gs = gastosPorMes regs

-- | Promedio de gastos mensuales históricos; 'Nothing' si no hay datos.
proyeccionGastoSiguienteMes :: [FinancialRecord] -> Maybe Double
proyeccionGastoSiguienteMes regs =
  case map snd (gastosPorMes regs) of
    [] -> Nothing
    xs -> Just (sum xs / fromIntegral (length xs))

-- | Gasto acumulado por categoría (nombre exacto como en el registro, solo 'Expense').
gastoPorCategoria :: [FinancialRecord] -> [(String, Double)]
gastoPorCategoria regs =
  [ (c, sum [amount r | r <- regs, recordType r == Expense, category r == c])
  | c <- categoriasUnicasGasto regs
  ]

-- | Hasta @n@ categorías con mayor gasto acumulado (orden descendente por monto).
topCategoriasGasto :: Int -> [FinancialRecord] -> [(String, Double)]
topCategoriasGasto n regs =
  take n $
    sortByGastoDesc
      (gastoPorCategoria regs)

-- | Categorías que aparecen en al menos un 'Expense', sin duplicados (orden no garantizado).
categoriasUnicasGasto :: [FinancialRecord] -> [String]
categoriasUnicasGasto regs =
  nub [category r | r <- regs, recordType r == Expense]

-- | Ordena @(categoría, monto)@ por monto descendente (ordenamiento por partición, no estable).
sortByGastoDesc :: [(String, Double)] -> [(String, Double)]
sortByGastoDesc [] = []
sortByGastoDesc (x : xs) =
  sortByGastoDesc mayores ++ [x] ++ sortByGastoDesc menores
  where
    mayores = [p | p@(_, v) <- xs, v > snd x]
    menores = [p | p@(_, v) <- xs, v <= snd x]

-- | Suma de todos los montos 'Expense'.
totalGastos :: [FinancialRecord] -> Double
totalGastos regs = sum [amount r | r <- regs, recordType r == Expense]

-- | Simula bajar gastos en un porcentaje (0–100). Tupla: gasto actual, gasto tras recorte,
--   ahorro estimado por el recorte, balance actual y balance si se aplicara ese ahorro al balance.
simularReduccionGastos :: Double -> [FinancialRecord] -> (Double, Double, Double, Double, Double)
simularReduccionGastos porcentaje regs =
  (gastoActual, gastoReducido, ahorroEstimado, balanceActual, balanceSimulado)
  where
    gastoActual = totalGastos regs
    factor = max 0 (min 100 porcentaje) / 100
    ahorroEstimado = gastoActual * factor
    gastoReducido = gastoActual - ahorroEstimado
    balanceActual = calcularBalance regs
    balanceSimulado = balanceActual + ahorroEstimado

-- | Promedio del neto mensual (ingresos − gastos) sobre los meses con datos en 'resumenMensual'.
ahorroPromedioMensual :: [FinancialRecord] -> Maybe Double
ahorroPromedioMensual regs =
  case resumenMensual regs of
    [] -> Nothing
    xs -> Just (sum [neto | (_, _, _, neto) <- xs] / fromIntegral (length xs))

-- | Proyección lineal: para cada mes @1..n@, ahorro acumulado = promedio neto × mes. 'Nothing' si @n ≤ 0@ o sin datos.
proyeccionAhorroEnMeses :: Int -> [FinancialRecord] -> Maybe [(Int, Double)]
proyeccionAhorroEnMeses meses regs
  | meses <= 0 = Nothing
  | otherwise =
      case ahorroPromedioMensual regs of
        Nothing -> Nothing
        Just prom ->
          Just [(m, prom * fromIntegral m) | m <- [1 .. meses]]

-- | @(ingresos, gastos, neto)@ para un mes @YYYY-MM@, si existe en 'resumenMensual'.
resumenDeMes :: String -> [FinancialRecord] -> Maybe (Double, Double, Double)
resumenDeMes mes regs =
  case [ (ing, gas, neto) | (m, ing, gas, neto) <- resumenMensual regs, m == mes ] of
    [] -> Nothing
    (x : _) -> Just x

-- | Compara dos meses: ingresos, gastos y neto de cada uno más variaciones (mes2 − mes1).
--   Tupla: @(ing1, ing2, Δing, gas1, gas2, Δgas, neto1, neto2, Δneto)@.
compararPeriodos ::
  String ->
  String ->
  [FinancialRecord] ->
  Maybe (Double, Double, Double, Double, Double, Double, Double, Double, Double)
compararPeriodos mes1 mes2 regs = do
  (ing1, gas1, neto1) <- resumenDeMes mes1 regs
  (ing2, gas2, neto2) <- resumenDeMes mes2 regs
  let varIng = ing2 - ing1
      varGas = gas2 - gas1
      varNeto = neto2 - neto1
  Just (ing1, ing2, varIng, gas1, gas2, varGas, neto1, neto2, varNeto)