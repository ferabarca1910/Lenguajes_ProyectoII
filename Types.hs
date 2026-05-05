module Types where
--RecordTypes va a ser igual a ese tipo de valores(puede variar)
data RecordType = Income | Expense | Saving | Investment
  deriving (Show, Read, Eq)

data FinancialRecord = FinancialRecord
  { recordType  :: RecordType
  , amount      :: Double
  , category    :: String
  , date        :: String
  , description :: String
  , tags        :: [String]
  } deriving (Show, Read)

-- Presupuesto máximo de gastos (Expense) para una categoría (nombre libre, se compara sin distinguir mayúsculas)
data Budget = Budget
  { budgetCategory :: String
  , budgetLimit    :: Double
  } deriving (Show, Read, Eq)

-- Reglas configurables evaluadas sobre los registros cargados en memoria
data Rule
  = -- Suma de gastos (Expense) en la categoría (normalizada) supera el umbral
    RuleGastoEnCategoriaMayor String Double
  | -- Suma de montos de ahorro (Saving) por debajo del mínimo deseado
    RuleAhorroTotalMenor Double
  deriving (Show, Read, Eq)
