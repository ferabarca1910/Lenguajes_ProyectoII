module Main where

import Types
import Storage
import Logic

-- Función principal que inicia el programa
main :: IO ()
main = do
    registros <- loadRecords
    menu registros

-- Menú principal del sistema
menu :: [FinancialRecord] -> IO ()
menu registros = do
    putStrLn "\n--- Sistema de Finanzas ---"
    putStrLn "1. Agregar ingreso"
    putStrLn "2. Agregar gasto"
    putStrLn "3. Ver registros"
    putStrLn "4. Ver balance"
    putStrLn "5. Guardar y salir"
    opcion <- getLine

    case opcion of
        "1" -> agregarRegistro Income registros >>= menu
        "2" -> agregarRegistro Expense registros >>= menu
        "3" -> do
            mostrarRegistros registros
            menu registros
        "4" -> do
            putStrLn ("Balance: " ++ show (calcularBalance registros))
            menu registros
        "5" -> do
            saveRecords registros
            putStrLn "Datos guardados. Chao mae"
        _   -> do
            putStrLn "Opción inválida"
            menu registros

-- Crea un nuevo registro financiero
-- De momento se usan valores por defecto para category, date y tags
agregarRegistro :: RecordType -> [FinancialRecord] -> IO [FinancialRecord]
agregarRegistro tipo registros = do
    putStrLn "Monto:"
    m <- getLine
    putStrLn "Descripción:"
    desc <- getLine

    let nuevo = FinancialRecord
            tipo
            (read m)
            "General"
            "2026-04-20"
            desc
            []

    return (registros ++ [nuevo])

-- Muestra todos los registros de forma más legible
mostrarRegistros :: [FinancialRecord] -> IO ()
mostrarRegistros [] = putStrLn "No hay registros"
mostrarRegistros regs = mapM_ mostrarUno regs

-- Muestra un solo registro con formato
mostrarUno :: FinancialRecord -> IO ()
mostrarUno r = do
    putStrLn "------------------------"
    putStrLn ("Tipo: " ++ show (recordType r))
    putStrLn ("Monto: " ++ show (amount r))
    putStrLn ("Categoría: " ++ category r)
    putStrLn ("Fecha: " ++ date r)
    putStrLn ("Descripción: " ++ description r)
    putStrLn ("Tags: " ++ show (tags r))
