module Storage where

import Types

--Recibe una lista de FinanciaRecord y devuelve una acción tipo IO ()
saveRecords :: [FinancialRecord] -> IO ()
saveRecords records = writeFile "records.txt" (show records)

loadRecords :: IO [FinancialRecord]
loadRecords = do
  content <- readFile "records.txt"
  return (read content)
