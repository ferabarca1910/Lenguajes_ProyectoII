module Logic where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, foldl', sortBy)

import Types

trimCat :: String -> String
trimCat = dropWhile isSpace . dropWhileEnd isSpace

-- Comparación insensible a mayúsculas para emparejar categoría de gasto con presupuesto
categoriaNorm :: String -> String
categoriaNorm = map toLower . trimCat

-- Suma solo gastos (Expense) cuya categoría coincide con la del presupuesto (normalizada)
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

-- True si el gasto acumulado en esa categoría supera el tope
excedePresupuesto :: Budget -> [FinancialRecord] -> Bool
excedePresupuesto b regs = gastoRealEnCategoria (budgetCategory b) regs > budgetLimit b

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

-- Balance simple: ingresos, ahorros e inversiones suman; gastos restan
calcularBalance :: [FinancialRecord] -> Double
calcularBalance = sum . map contribucion
  where
    contribucion r = case recordType r of
        Income     -> amount r
        Expense    -> -amount r
        Saving     -> amount r
        Investment -> amount r

-- Si ya hay presupuesto para la misma categoría (normalizada), lo reemplaza
upsertBudget :: Budget -> [Budget] -> [Budget]
upsertBudget b [] = [b]
upsertBudget b (x : xs)
  | categoriaNorm (budgetCategory b) == categoriaNorm (budgetCategory x) =
      b : xs
  | otherwise = x : upsertBudget b xs

-- Suma de montos registrados como ahorro (Saving)
totalAhorroRegistrado :: [FinancialRecord] -> Double
totalAhorroRegistrado regs =
  sum [amount r | r <- regs, recordType r == Saving]

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

evaluarReglas :: [Rule] -> [FinancialRecord] -> [String]
evaluarReglas rs regs = concatMap f rs
  where
    f r = case evaluarRegla r regs of
      Nothing -> []
      Just m -> [m]

describirRegla :: Rule -> String
describirRegla (RuleGastoEnCategoriaMayor c x) =
  "Alerta si gastos (Expense) en categoría \"" ++ c ++ "\" superan " ++ show x
describirRegla (RuleAhorroTotalMenor m) =
  "Advertencia si la suma de ahorros (Saving) es menor a " ++ show m

mesDeFecha :: String -> String
mesDeFecha f
  | length f >= 7 = take 7 f
  | otherwise = f

acumularPorClave :: String -> Double -> [(String, Double)] -> [(String, Double)]
acumularPorClave k v [] = [(k, v)]
acumularPorClave k v ((k0, v0) : xs)
  | k == k0 = (k0, v0 + v) : xs
  | otherwise = (k0, v0) : acumularPorClave k v xs

ordenarPorMesAsc :: [(String, Double)] -> [(String, Double)]
ordenarPorMesAsc = sortBy (\(m1, _) (m2, _) -> compare m1 m2)

resumenMensual :: [FinancialRecord] -> [(String, Double, Double, Double)]
resumenMensual regs =
  [ (mes, ingresos, gastos, ingresos - gastos)
  | mes <- meses
  , let ingresos = sumaIncome mes
  , let gastos = sumaExpense mes
  ]
  where
    meses = uniqueSortedMonths regs
    sumaIncome m =
      sum [amount r | r <- regs, mesDeFecha (date r) == m, recordType r == Income]
    sumaExpense m =
      sum [amount r | r <- regs, mesDeFecha (date r) == m, recordType r == Expense]

gastosPorMes :: [FinancialRecord] -> [(String, Double)]
gastosPorMes regs =
  ordenarPorMesAsc $
    foldl'
      (\acc r -> if recordType r == Expense then acumularPorClave (mesDeFecha (date r)) (amount r) acc else acc)
      []
      regs

tendenciaGastoMensual :: [FinancialRecord] -> [(String, Double, Double, Double)]
tendenciaGastoMensual regs =
  [ (mesActual, gastoActual, gastoPrevio, gastoActual - gastoPrevio)
  | ((_, gastoPrevio), (mesActual, gastoActual)) <- zip gs (drop 1 gs)
  ]
  where
    gs = gastosPorMes regs

proyeccionGastoSiguienteMes :: [FinancialRecord] -> Maybe Double
proyeccionGastoSiguienteMes regs =
  case map snd (gastosPorMes regs) of
    [] -> Nothing
    xs -> Just (sum xs / fromIntegral (length xs))

gastoPorCategoria :: [FinancialRecord] -> [(String, Double)]
gastoPorCategoria regs =
  foldl'
    (\acc r -> if recordType r == Expense then acumularPorClave (category r) (amount r) acc else acc)
    []
    regs

topCategoriasGasto :: Int -> [FinancialRecord] -> [(String, Double)]
topCategoriasGasto n regs =
  take n $
    sortBy
      (\(c1, v1) (c2, v2) -> compare v2 v1 <> compare c1 c2)
      (gastoPorCategoria regs)

uniqueSortedMonths :: [FinancialRecord] -> [String]
uniqueSortedMonths regs = go (sortBy compare [mesDeFecha (date r) | r <- regs]) []
  where
    go [] acc = reverse acc
    go (x : xs) [] = go xs [x]
    go (x : xs) (y : ys)
      | x == y = go xs (y : ys)
      | otherwise = go xs (x : y : ys)