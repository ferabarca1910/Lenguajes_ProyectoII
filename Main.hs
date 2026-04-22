module Main where

import Types
import Storage

--Main usa IO es un tiro de retorno tipo print
main :: IO ()
main = do
  putStrLn "Guardando registro de prueba..."

  let r1 = FinancialRecord Income 1000 "Salario" "2026-04-21" "Pago mensual" ["fijo"]

  saveRecords [r1]

  putStrLn "Cargando registros..."

  records <- loadRecords

  print records
