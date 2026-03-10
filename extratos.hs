import Test.HUnit
import Test.QuickCheck

data Movimento = Credito Float
               | Debito Float
               deriving Show

data Data = D Int Int Int
          deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
              deriving Show

data Extractos = Extractos [Extracto]
               deriving Show

-- Exercicio 1

extValor :: Extracto -> Float -> [Movimento]

