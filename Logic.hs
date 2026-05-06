module Logic where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, nub, sort)

import Types

-- Quita espacios al inicio y al final de una categoría.
trimCat :: String -> String
trimCat = dropWhile isSpace . dropWhileEnd isSpace

-- Normaliza categoría para comparar sin diferencias de mayúsculas/minúsculas.
categoriaNorm :: String -> String
categoriaNorm = map toLower . trimCat

-- Suma los gastos (Expense) de una categoría dada.
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

-- Indica si una categoría se pasó de su presupuesto.
excedePresupuesto :: Budget -> [FinancialRecord] -> Bool
excedePresupuesto b regs = gastoRealEnCategoria (budgetCategory b) regs > budgetLimit b

-- Genera mensajes de alerta para los presupuestos excedidos.
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

-- Balance general: ingreso/ahorro/inversión suman y gasto resta.
calcularBalance :: [FinancialRecord] -> Double
calcularBalance = sum . map contribucion
  where
    contribucion r = case recordType r of
        Income     -> amount r
        Expense    -> -amount r
        Saving     -> amount r
        Investment -> amount r

-- Inserta o reemplaza presupuesto de una categoría.
upsertBudget :: Budget -> [Budget] -> [Budget]
upsertBudget b [] = [b]
upsertBudget b (x : xs)
  | categoriaNorm (budgetCategory b) == categoriaNorm (budgetCategory x) =
      b : xs
  | otherwise = x : upsertBudget b xs

-- Suma todo lo registrado como ahorro (Saving).
totalAhorroRegistrado :: [FinancialRecord] -> Double
totalAhorroRegistrado regs =
  sum [amount r | r <- regs, recordType r == Saving]

-- Evalúa una regla individual y retorna mensaje si se activa.
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

-- Evalúa todas las reglas y junta solo los mensajes disparados.
evaluarReglas :: [Rule] -> [FinancialRecord] -> [String]
evaluarReglas rs regs = concatMap f rs
  where
    f r = case evaluarRegla r regs of
      Nothing -> []
      Just m -> [m]

-- Convierte una regla a texto legible para mostrar en pantalla.
describirRegla :: Rule -> String
describirRegla (RuleGastoEnCategoriaMayor c x) =
  "Alerta si gastos (Expense) en categoría \"" ++ c ++ "\" superan " ++ show x
describirRegla (RuleAhorroTotalMenor m) =
  "Advertencia si la suma de ahorros (Saving) es menor a " ++ show m

-- Extrae el mes en formato YYYY-MM desde YYYY-MM-DD.
mesDeFecha :: String -> String
mesDeFecha f
  | length f >= 7 = take 7 f
  | otherwise = f

-- Lista de meses únicos y ordenados presentes en los registros.
mesesUnicosOrdenados :: [FinancialRecord] -> [String]
mesesUnicosOrdenados regs = sort (nub [mesDeFecha (date r) | r <- regs])

-- Flujo por mes: ingresos, gastos y neto.
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

-- Gasto total por mes (solo registros Expense).
gastosPorMes :: [FinancialRecord] -> [(String, Double)]
gastosPorMes regs =
  [ (m, sum [amount r | r <- regs, recordType r == Expense, mesDeFecha (date r) == m])
  | m <- mesesUnicosOrdenados regs
  ]

-- Compara gasto de cada mes contra el mes anterior.
tendenciaGastoMensual :: [FinancialRecord] -> [(String, Double, Double, Double)]
tendenciaGastoMensual regs =
  [ (mesActual, gastoActual, gastoPrevio, gastoActual - gastoPrevio)
  | ((_, gastoPrevio), (mesActual, gastoActual)) <- zip gs (drop 1 gs)
  ]
  where
    gs = gastosPorMes regs

-- Proyección simple: promedio de gastos mensuales históricos.
proyeccionGastoSiguienteMes :: [FinancialRecord] -> Maybe Double
proyeccionGastoSiguienteMes regs =
  case map snd (gastosPorMes regs) of
    [] -> Nothing
    xs -> Just (sum xs / fromIntegral (length xs))

-- Suma gasto por categoría para identificar impacto.
gastoPorCategoria :: [FinancialRecord] -> [(String, Double)]
gastoPorCategoria regs =
  [ (c, sum [amount r | r <- regs, recordType r == Expense, category r == c])
  | c <- categoriasUnicasGasto regs
  ]

-- Top N categorías con mayor gasto acumulado.
topCategoriasGasto :: Int -> [FinancialRecord] -> [(String, Double)]
topCategoriasGasto n regs =
  take n $
    sortByGastoDesc
      (gastoPorCategoria regs)

-- Lista categorías (de gastos) sin repetir.
categoriasUnicasGasto :: [FinancialRecord] -> [String]
categoriasUnicasGasto regs =
  nub [category r | r <- regs, recordType r == Expense]

-- Ordena pares (categoría, monto) de mayor a menor monto.
sortByGastoDesc :: [(String, Double)] -> [(String, Double)]
sortByGastoDesc [] = []
sortByGastoDesc (x : xs) =
  sortByGastoDesc mayores ++ [x] ++ sortByGastoDesc menores
  where
    mayores = [p | p@(_, v) <- xs, v > snd x]
    menores = [p | p@(_, v) <- xs, v <= snd x]

-- Total de gastos registrados (Expense).
totalGastos :: [FinancialRecord] -> Double
totalGastos regs = sum [amount r | r <- regs, recordType r == Expense]

-- Simula reducir gastos en un porcentaje y retorna:
-- (gastoActual, gastoReducido, ahorroEstimado, balanceActual, balanceSimulado).
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

-- Ahorro neto promedio por mes: ingreso - gasto.
ahorroPromedioMensual :: [FinancialRecord] -> Maybe Double
ahorroPromedioMensual regs =
  case resumenMensual regs of
    [] -> Nothing
    xs -> Just (sum [neto | (_, _, _, neto) <- xs] / fromIntegral (length xs))

-- Proyecta ahorro acumulado para N meses usando el promedio neto mensual.
proyeccionAhorroEnMeses :: Int -> [FinancialRecord] -> Maybe [(Int, Double)]
proyeccionAhorroEnMeses meses regs
  | meses <= 0 = Nothing
  | otherwise =
      case ahorroPromedioMensual regs of
        Nothing -> Nothing
        Just prom ->
          Just [(m, prom * fromIntegral m) | m <- [1 .. meses]]

-- Busca el resumen de un mes específico (YYYY-MM) dentro del flujo mensual.
resumenDeMes :: String -> [FinancialRecord] -> Maybe (Double, Double, Double)
resumenDeMes mes regs =
  case [ (ing, gas, neto) | (m, ing, gas, neto) <- resumenMensual regs, m == mes ] of
    [] -> Nothing
    (x : _) -> Just x

-- Compara dos periodos (mes vs mes) y devuelve variaciones.
-- (ingMes1, ingMes2, varIng, gasMes1, gasMes2, varGas, netoMes1, netoMes2, varNeto)
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