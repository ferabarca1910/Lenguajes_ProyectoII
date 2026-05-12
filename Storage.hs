{- | Persistencia en archivos de texto del directorio de trabajo: @records.txt@, @budgets.txt@, @rules.txt@.
     Formato: 'show' / 'read' de Haskell; no conviene editar a mano.
-}
module Storage where

import Types
import System.Directory (doesFileExist)

-- | Serializa y escribe la lista de 'FinancialRecord' en @records.txt@.
saveRecords :: [FinancialRecord] -> IO ()
saveRecords records = writeFile "records.txt" (show records)

-- | Escribe presupuestos en @budgets.txt@.
saveBudgets :: [Budget] -> IO ()
saveBudgets bs = writeFile "budgets.txt" (show bs)

-- | Lee @budgets.txt@ o lista vacía si no existe.
loadBudgets :: IO [Budget]
loadBudgets = do
  exists <- doesFileExist "budgets.txt"
  if not exists
    then return []
    else do
      content <- readFile "budgets.txt"
      return (read content)

-- | Lee @records.txt@ o @[]@ si el archivo no existe.
loadRecords :: IO [FinancialRecord]
loadRecords = do
  exists <- doesFileExist "records.txt"

  if not exists
    then return []
    else do
      content <- readFile "records.txt"
      return (read content)

-- | Escribe reglas en @rules.txt@.
saveRules :: [Rule] -> IO ()
saveRules rs = writeFile "rules.txt" (show rs)

-- | Lee @rules.txt@ o @[]@ si no existe.
loadRules :: IO [Rule]
loadRules = do
  exists <- doesFileExist "rules.txt"
  if not exists
    then return []
    else do
      content <- readFile "rules.txt"
      return (read content)
