module Functions where

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

round2 :: Float -> Float
round2 x = fromIntegral (round (x * 100)) / 100

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
isCredito (Credito m) = True
isCredito _ = False

isDebito :: Movimento -> Bool
isDebito (Debito m) = True
isDebito _ = False

creDeb :: Extracto -> (Float, Float)
creDeb (Ext _ movs) = (totalCred, totalDeb)
       where
              totalCred = sum [valorMov m | (_,_,m) <- movs, isCredito m]
              totalDeb = sum [valorMov m | (_,_,m) <- movs, isDebito m]

-- Exercicio 4

saldo :: Extracto -> Float
saldo (Ext s movs) = round2 (s + totalCred - totalDeb)
  where
    (totalCred,totalDeb) = creDeb (Ext s movs)
