module Logic where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd)

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