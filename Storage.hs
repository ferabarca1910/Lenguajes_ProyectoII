{- | Persistencia solo de registros financieros en @records.txt@.
     Formato: un @FinancialRecord@ por línea (@show@ de Haskell por registro).
     Si el archivo antiguo empieza con @[@, se lee como lista única con @read@.
     Presupuestos y reglas solo en memoria (no hay @budgets.txt@ ni @rules.txt@).
     La lectura usa 'slurpFile' para cerrar el handle de inmediato (evita bloqueos en Windows).
-}
module Storage
  ( saveRecords,
    loadRecords,
    saveBudgets,
    loadBudgets,
    saveRules,
    loadRules,
  )
where

import Control.Exception (evaluate)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

import Types

-- | Lee el archivo entero de forma estricta y libera el handle antes de seguir (importante en Windows).
slurpFile :: FilePath -> IO String
slurpFile path = do
  s <- readFile path
  _ <- evaluate (length s)
  return s

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Serializa: una línea por registro (@show@ de cada 'FinancialRecord').
saveRecords :: [FinancialRecord] -> IO ()
saveRecords records =
  writeFile "records.txt" (unlines (map show records))

-- | Sin archivo: los presupuestos solo existen en memoria mientras corre el programa.
saveBudgets :: [Budget] -> IO ()
saveBudgets _ = return ()

-- | Sin archivo: siempre lista vacía al iniciar.
loadBudgets :: IO [Budget]
loadBudgets = return []

-- | Lee @records.txt@: formato línea por línea, o legado de una sola lista que empieza con @[@.
loadRecords :: IO [FinancialRecord]
loadRecords = do
  exists <- doesFileExist "records.txt"
  if not exists
    then return []
    else do
      content <- slurpFile "records.txt"
      let s = trim content
      if null s
        then return []
        else
          if head s == '['
            then return (fromMaybe [] (readMaybe s :: Maybe [FinancialRecord]))
            else return (parseRecordLines content)

-- | Cada línea no vacía se interpreta como un solo @FinancialRecord@.
parseRecordLines :: String -> [FinancialRecord]
parseRecordLines =
  mapMaybe (\ln -> readMaybe (trim ln) :: Maybe FinancialRecord)
    . filter (not . null . trim)
    . lines

-- | Sin archivo: las reglas solo existen en memoria.
saveRules :: [Rule] -> IO ()
saveRules _ = return ()

-- | Sin archivo: siempre lista vacía al iniciar.
loadRules :: IO [Rule]
loadRules = return []
