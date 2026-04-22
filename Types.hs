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
