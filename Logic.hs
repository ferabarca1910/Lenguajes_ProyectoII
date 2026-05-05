module Logic where

import Types

-- Balance simple: ingresos, ahorros e inversiones suman; gastos restan
calcularBalance :: [FinancialRecord] -> Double
calcularBalance = sum . map contribucion
  where
    contribucion r = case recordType r of
        Income     -> amount r
        Expense    -> -amount r
        Saving     -> amount r
        Investment -> amount r
