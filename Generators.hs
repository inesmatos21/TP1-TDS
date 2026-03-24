module Generators where

import Test.QuickCheck
import Functions

genDebito :: Gen (String, Float)
genDebito = do
  (nome, val) <- frequency
    [ (2,  do v <- choose (10.0, 20.0); return ("Restaurante", v))
    , (5,  do v <- choose (5.0, 100.0); return ("Supermercado", v))
    , (1,  do v <- choose (10.0, 40.0); return ("Telefone", v))
    , (1,  do v <- choose (10.0, 50.0); return ("Internet", v))
    , (1,  do v <- choose (40.0, 100.0); return ("Eletricidade", v))
    , (1,  do v <- choose (20.0, 60.0); return ("Agua", v))
    , (4,  do v <- choose (10.0, 80.0); return ("Combustivel", v))
    , (2,  do v <- choose (5.0, 30.0); return ("Farmacia", v))
    , (2,  do v <- choose (5.0, 10.0); return ("Cinema", v))
    , (3,  do v <- choose (2.0, 100.0); return ("Roupa", v))
    , (2,  do v <- choose (5.0, 20.0); return ("Livraria", v))
    , (15, do v <- choose (0.1, 5.0); return ("Cafe", v))
    , (2,  do v <- choose (2.5, 20.0); return ("Uber", v))
    , (1,  do v <- choose (20.0, 30.0); return ("Ginasio", v))
    , (1,  do v <- choose (20.0, 90.0); return ("Seguro", v))
    ]
  return (nome, round2 val)

genCredito :: Gen (String, Float)
genCredito = do
  (nome, val) <- frequency
    [ (2,  do v <- choose (10.0,50.0); return ("Betclic", v))
    , (2,  do v <- choose (15.0, 50.0); return ("Reembolso", v))
    , (5,  do v <- choose (10.0, 40.0); return ("MBWay Recebido", v))
    , (1,  do v <- choose (80.0, 150.0); return ("Freelance", v))
    , (3,  do v <- choose (10.0,200.0); return ("Transferencia", v))
    ]
  return (nome, round2 val)

genSalario :: Gen Float
genSalario = round2 <$> choose (700.0, 1200)

genMovimento :: Gen (String,Movimento)
genMovimento =  frequency [
  (99, do (s,v) <- genDebito
          return (s, Debito v)),
  (1, do (s,v) <- genCredito
         return (s, Credito v))]


genData :: Gen Data
genData = do
  m <- choose (1,12)
  a <- choose (2000,2026)
  d <- genDataDia m a
  return $ D d m a

pertence :: Int -> [Int] -> Bool
pertence _ [] = False
pertence n (h:t) | n == h = True
                 | otherwise = pertence n t

genDataDia :: Int -> Int -> Gen Int
genDataDia m a
  | m == 2 = choose (1,28)
  | pertence m [1,3,5,7,8,10,12] = choose (1,31)
  | otherwise = choose (1,30)

getDiaData :: Data -> Int
getDiaData (D d m a) = d

getMesData :: Data -> Int
getMesData (D d m a) = m

getAnoData :: Data -> Int
getAnoData (D d m a) = a

-- Gera movimentos num dia em especifico
genMovsDia :: Int -> Float -> Data -> Gen (Float, [(Data, String, Movimento)])
genMovsDia 0 s _ = return (s, [])
genMovsDia n s d = do
  (desc, mov) <- genMovimento
  let v = valorMov mov
  
  if v > s
    then 
      genMovsDia (n-1) s d
    else do
      let novoSaldo = s - v
      (saldoFinal, rest) <- genMovsDia (n-1) novoSaldo d
      return (saldoFinal, (d, desc, mov) : rest)

-- Vê quantos dias até ao fim do mês
diasRestantes :: Data -> Int
diasRestantes (D d m a) = diasNoMes m - d + 1
  where
    diasNoMes 2 = 28
    diasNoMes m
      | pertence m [1,3,5,7,8,10,12] = 31
      | otherwise = 30

-- Passa para o próximo dia
nextDia :: Data -> Data
nextDia (D d m a) = D (d + 1) m a

-- Gera movimentos num mes
genMovs :: Int -> Int -> Float -> Data -> Gen (Float, [(Data, String, Movimento)])
genMovs 0 _ s _ = return (s, [])
genMovs n diaSalario s d = do
  if getDiaData d == diaSalario then do
    credito <- genSalario
    let mov = (d, "Salario", Credito credito)
    let novoSaldo = s + credito
    
    (saldoFinal, rest) <- genMovs (n - 1) diaSalario novoSaldo (nextDia d)
    return (saldoFinal, mov : rest)
  else do
    k <- choose(0,5)
    (saldoAposDia, diaMovs) <- genMovsDia k s d
    (saldoFinal, rest) <- genMovs (n - 1) diaSalario saldoAposDia (nextDia d)
    return (saldoFinal, diaMovs ++ rest)

genExtracto :: Float -> Data -> Gen Extracto
genExtracto s d = do
  let n = diasRestantes d
  diaSalario <- choose (1, 8)
  (_, movs) <- genMovs n diaSalario s d
  return (Ext s movs)

-- Passa para o próximo mês
nextMonth :: Data -> Data
nextMonth (D d m a) | m == 12 = D 1 1 (a + 1)
                    | otherwise = D 1 (m + 1) a

-- Add this after your existing Arbitrary Extractos instance
instance Arbitrary Extracto where
  arbitrary = do
    d <- genData
    s <- round2 <$> choose (500.0, 1000.0)
    genExtracto s d

instance Arbitrary Extractos where
  arbitrary = sized $ genExtractos

genExtractos n = do
  d <- genData
  s <- round2 <$> choose (500.0, 1000.0)
  Extractos <$> genListExtractos n s d

genListExtractos :: Int -> Float -> Data -> Gen [Extracto]
genListExtractos 0 _ _ = return []
genListExtractos n s d = do
  anterior <- genExtracto s d
  rest <- genListExtractos (n-1) (saldo anterior) (nextMonth d)
  return (anterior : rest)
