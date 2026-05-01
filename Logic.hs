module Logic where

import Types

-- Calcula el balance total del sistema
-- Income suma, Expense resta, los demás se ignoran por ahora
calcularBalance :: [FinancialRecord] -> Double
calcularBalance records =
    sum [ if recordType r == Income then amount r else - amount r | r <- records ]
