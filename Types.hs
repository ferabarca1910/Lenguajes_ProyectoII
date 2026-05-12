{- | Tipos de dominio: movimientos financieros, presupuestos por categoría y reglas evaluables.
     Convención de balance: 'Income', 'Saving' e 'Investment' suman; 'Expense' resta (véase 'Logic.calcularBalance').
-}
module Types where

-- | Clasificación de un movimiento en el libro de registros.
data RecordType = Income | Expense | Saving | Investment
  deriving (Show, Read, Eq)

-- | Un movimiento con monto, categoría, fecha, descripción y etiquetas opcionales.
data FinancialRecord = FinancialRecord
  { recordType  :: RecordType
  , amount      :: Double
  , category    :: String
  , date        :: String
  , description :: String
  , tags        :: [String]
  } deriving (Show, Read)

-- | Tope de gastos ('Expense') para una categoría; al comparar se normaliza el nombre (trim + minúsculas).
data Budget = Budget
  { budgetCategory :: String
  , budgetLimit    :: Double
  } deriving (Show, Read, Eq)

-- | Reglas configurables; se evalúan sobre la lista de 'FinancialRecord' en memoria.
data Rule
  = -- | Dispara si la suma de 'Expense' en la categoría (normalizada) supera el umbral.
    RuleGastoEnCategoriaMayor String Double
  | -- | Dispara si la suma de montos 'Saving' es menor al mínimo indicado.
    RuleAhorroTotalMenor Double
  deriving (Show, Read, Eq)
