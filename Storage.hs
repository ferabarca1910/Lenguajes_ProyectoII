module Storage where

import Types
import System.Directory (doesFileExist)

-- Guarda la lista de registros en un archivo de texto
-- Se usa show para serializar la lista
saveRecords :: [FinancialRecord] -> IO ()
saveRecords records = writeFile "records.txt" (show records)

-- Carga los registros desde el archivo
-- Si el archivo no existe, retorna lista vacía para evitar errores
loadRecords :: IO [FinancialRecord]
loadRecords = do
  exists <- doesFileExist "records.txt"

  if not exists
    then return []
    else do
      content <- readFile "records.txt"
      return (read content)
