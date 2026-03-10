import Test.HUnit
import Test.QuickCheck
import Data.Foldable (Foldable(sum))

data Movimento = Credito Float
               | Debito Float
               deriving Show

data Data = D Int Int Int
          deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
              deriving Show

data Extractos = Extractos [Extracto]
               deriving Show

ext1 :: Extracto
ext1 = Ext 100
       [ (D 1 1 2024, "Salário", Credito 1200)
       , (D 2 1 2024, "Supermercado", Debito 50)
       , (D 3 1 2024, "Renda", Debito 400)
       , (D 4 1 2024, "Restaurante", Debito 30)
       ]


-- Exercicio 1

valorMov :: Movimento -> Float
valorMov (Credito v) = v
valorMov (Debito v) = v

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) v = []
extValor (Ext si ((_,_,mov):t)) v = if valorMov mov > v then mov : extValor (Ext si t) v else extValor (Ext si t) v

-- Exercicio 2

verificaString :: String -> [String] -> Bool
verificaString s [] = False
verificaString s (s1:s2) = (s == s1) || verificaString s s2


filtro :: Extracto -> [String] -> [(Data, Movimento)]
filtro (Ext _ []) strings = []
filtro (Ext si ((d, s, mov):t)) strings = if verificaString s strings then (d, mov) : filtro (Ext si t) strings else filtro (Ext si t) strings

-- Exercicio 3

isCredito :: Movimento -> Bool
Credito m = True
Debito m = False

isDebito :: Movimento -> Bool
Credito m = False
Debito m = True

sumCred :: [Movimento] -> Float
sumCred [] = 0
sumCred (Debito m : movs) = 0 + sumCred movs
sumCred (Credito m : movs) = m + sumCred movs

sumDeb :: [Movimento] -> Float
sumCred 
sumDeb (Credito m : movs) = m + sumDeb movs
sumDeb (Debito m : movs) = 0 + sumDeb movs

creDeb :: Extracto -> (Float, Float)
creDeb (Ext si ((_,_,mov):t)) = (sumCred mov, sumCred)
