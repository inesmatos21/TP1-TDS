import Test.HUnit
import Test.QuickCheck
import Data.Foldable (Foldable(sum))
import GHC.Read (list)

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

-- Geradores

round2 :: Float -> Float
round2 x = fromIntegral (round (x * 100)) / 100

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

genSalario :: Gen Float
genSalario = round2 <$> choose (700.0, 1200)

genCredito :: Gen Float
genCredito = round2 <$> choose (80.0, 200)


genMovimento :: Gen (String,Movimento)
genMovimento =  frequency [
  (99, do (s,v) <- genDebito
          return (s, Debito v)),
  (1, do v <- genCredito
         return ("Betclic", Credito v))]


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



-- Geradores

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
    credito <- genCredito
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

-- Mostrar Data
showData :: Data -> String
showData (D d m a) = show d ++ "/" ++ show m ++ "/" ++ show a

-- Mostrar movimento
showMov :: (Data, String, Movimento) -> String
showMov (d, desc, Credito v) = showData d ++ " | " ++ desc ++ " | +" ++ show (round2 v) ++ " €"
showMov (d, desc, Debito v)  = showData d ++ " | " ++ desc ++ " | -" ++ show (round2 v) ++ " €"

-- Imprimir um Ext
printExtracto :: Extracto -> IO ()
printExtracto ext@(Ext s movs) = do
    putStrLn "\n========================================="
    putStrLn "             EXTRATO MENSAL              "
    putStrLn "========================================="
    putStrLn $ " Saldo Inicial: " ++ show (round2 s) ++ " €"
    putStrLn "-----------------------------------------"
    mapM_ (putStrLn . showMov) movs
    putStrLn "-----------------------------------------"
    putStrLn $ " Saldo Final:   " ++ show (round2 $ saldo ext) ++ " €"
    putStrLn "=========================================\n"

-- Imprimir todos os Extractos
printExtractos :: Extractos -> IO ()
printExtractos (Extractos exts) = mapM_ printExtracto exts

ext1 = Extractos [Ext 530.0 [(D 8 7 2008,"Salario",Credito 124.55),(D 9 7 2008,"Roupa",Debito 34.96),(D 9 7 2008,"Combustivel",Debito 77.8),(D 9 7 2008,"Eletricidade",Debito 67.45),(D 9 7 2008,"Cafe",Debito 3.91),(D 9 7 2008,"Cafe",Debito 4.8),(D 12 7 2008,"Cafe",Debito 1.27),(D 13 7 2008,"Uber",Debito 9.93),(D 13 7 2008,"Roupa",Debito 48.67),(D 13 7 2008,"Cafe",Debito 0.12),(D 14 7 2008,"Supermercado",Debito 18.25),(D 14 7 2008,"Cafe",Debito 2.68),(D 14 7 2008,"Cinema",Debito 9.35),(D 15 7 2008,"Farmacia",Debito 19.62),(D 16 7 2008,"Combustivel",Debito 54.24),(D 17 7 2008,"Combustivel",Debito 53.26),(D 18 7 2008,"Supermercado",Debito 66.87),(D 18 7 2008,"Internet",Debito 37.63),(D 18 7 2008,"Farmacia",Debito 17.53),(D 19 7 2008,"Eletricidade",Debito 79.74),(D 19 7 2008,"Cafe",Debito 3.29),(D 19 7 2008,"Internet",Debito 32.29),(D 19 7 2008,"Uber",Debito 4.19),(D 21 7 2008,"Cafe",Debito 0.13),(D 22 7 2008,"Cafe",Debito 0.99),(D 27 7 2008,"Roupa",Debito 3.6),(D 30 7 2008,"Cafe",Debito 0.38),(D 30 7 2008,"Cafe",Debito 0.37)],Ext 1.23 [(D 1 8 2008,"Salario",Credito 171.66),(D 2 8 2008,"Cafe",Debito 0.52),(D 3 8 2008,"Livraria",Debito 16.31),(D 3 8 2008,"Cafe",Debito 4.17),(D 3 8 2008,"Combustivel",Debito 78.97),(D 3 8 2008,"Cafe",Debito 3.67),(D 6 8 2008,"Livraria",Debito 19.36),(D 6 8 2008,"Telefone",Debito 12.9),(D 6 8 2008,"Farmacia",Debito 5.8),(D 6 8 2008,"Cafe",Debito 2.58),(D 7 8 2008,"Cafe",Debito 2.98),(D 7 8 2008,"Farmacia",Debito 18.01),(D 8 8 2008,"Cafe",Debito 3.86),(D 8 8 2008,"Cafe",Debito 2.04),(D 15 8 2008,"Cafe",Debito 0.85),(D 28 8 2008,"Cafe",Debito 0.3),(D 29 8 2008,"Cafe",Debito 0.31)],Ext 0.26 [(D 2 9 2008,"Salario",Credito 107.03),(D 4 9 2008,"Cafe",Debito 1.2),(D 4 9 2008,"Combustivel",Debito 63.6),(D 4 9 2008,"Combustivel",Debito 12.88),(D 4 9 2008,"Cafe",Debito 3.39),(D 4 9 2008,"Cafe",Debito 2.04),(D 5 9 2008,"Cinema",Debito 9.02),(D 6 9 2008,"Cafe",Debito 2.43),(D 6 9 2008,"Farmacia",Debito 12.16),(D 25 9 2008,"Cafe",Debito 0.24)],Ext 0.33 [(D 7 10 2008,"Salario",Credito 116.15),(D 9 10 2008,"Agua",Debito 39.16),(D 9 10 2008,"Farmacia",Debito 6.36),(D 9 10 2008,"Supermercado",Debito 28.48),(D 10 10 2008,"Cafe",Debito 2.36),(D 11 10 2008,"Cinema",Debito 8.35),(D 11 10 2008,"Cafe",Debito 0.78),(D 11 10 2008,"Cafe",Debito 4.85),(D 11 10 2008,"Cafe",Debito 1.31),(D 11 10 2008,"Cafe",Debito 1.12),(D 12 10 2008,"Cafe",Debito 0.11),(D 13 10 2008,"Supermercado",Debito 11.04),(D 15 10 2008,"Cafe",Debito 2.47),(D 16 10 2008,"Cinema",Debito 8.02),(D 16 10 2008,"Cafe",Debito 0.17),(D 19 10 2008,"Cafe",Debito 1.1),(D 24 10 2008,"Cafe",Debito 0.27),(D 31 10 2008,"Cafe",Debito 0.14)],Ext 0.39 [(D 3 11 2008,"Salario",Credito 107.64),(D 4 11 2008,"Cafe",Debito 2.99),(D 5 11 2008,"Eletricidade",Debito 73.65),(D 5 11 2008,"Restaurante",Debito 16.4),(D 6 11 2008,"Cafe",Debito 3.17),(D 6 11 2008,"Cafe",Debito 2.05),(D 6 11 2008,"Cafe",Debito 4.99),(D 8 11 2008,"Cafe",Debito 1.85),(D 14 11 2008,"Cafe",Debito 2.36),(D 22 11 2008,"Cafe",Debito 0.56)],Ext 1.0e-2 [(D 7 12 2008,"Salario",Credito 145.74),(D 8 12 2008,"Supermercado",Debito 90.32),(D 8 12 2008,"Supermercado",Debito 26.79),(D 9 12 2008,"Cafe",Debito 3.23),(D 9 12 2008,"Cafe",Debito 1.08),(D 11 12 2008,"Cafe",Debito 0.47),(D 11 12 2008,"Cafe",Debito 4.8),(D 11 12 2008,"Cafe",Debito 3.31),(D 12 12 2008,"Cafe",Debito 2.06),(D 12 12 2008,"Farmacia",Debito 12.54),(D 13 12 2008,"Cafe",Debito 0.81)],Ext 0.34 [(D 5 1 2009,"Salario",Credito 151.67),(D 6 1 2009,"Cafe",Debito 2.8),(D 6 1 2009,"Telefone",Debito 14.68),(D 9 1 2009,"Supermercado",Debito 23.0),(D 9 1 2009,"Supermercado",Debito 52.97),(D 10 1 2009,"Farmacia",Debito 6.8),(D 10 1 2009,"Agua",Debito 21.28),(D 11 1 2009,"Supermercado",Debito 14.98),(D 12 1 2009,"Cafe",Debito 4.88),(D 12 1 2009,"Cafe",Debito 4.17),(D 13 1 2009,"Cafe",Debito 3.29),(D 16 1 2009,"Cafe",Debito 2.12),(D 19 1 2009,"Cafe",Debito 0.39),(D 19 1 2009,"Cafe",Debito 0.14)],Ext 0.51 [(D 3 2 2009,"Salario",Credito 116.58),(D 4 2 2009,"Uber",Debito 4.96),(D 4 2 2009,"Restaurante",Debito 12.46),(D 4 2 2009,"Roupa",Debito 61.64),(D 4 2 2009,"Cafe",Debito 3.45),(D 4 2 2009,"Cafe",Debito 4.71),(D 5 2 2009,"Cafe",Debito 4.0),(D 5 2 2009,"Uber",Debito 14.95),(D 6 2 2009,"Cafe",Debito 2.26),(D 8 2 2009,"Cafe",Debito 3.35),(D 10 2 2009,"Cafe",Debito 4.79),(D 24 2 2009,"Cafe",Debito 0.28)],Ext 0.24 [(D 4 3 2009,"Salario",Credito 131.21),(D 5 3 2009,"Uber",Debito 3.44),(D 5 3 2009,"Farmacia",Debito 10.19),(D 5 3 2009,"Cafe",Debito 1.88),(D 5 3 2009,"Ginasio",Debito 24.62),(D 6 3 2009,"Supermercado",Debito 81.28),(D 6 3 2009,"Cafe",Debito 4.37),(D 7 3 2009,"Cafe",Debito 4.8),(D 18 3 2009,"Cafe",Debito 0.75)],Ext 0.12 [(D 6 4 2009,"Salario",Credito 102.49),(D 7 4 2009,"Uber",Debito 7.06),(D 7 4 2009,"Uber",Debito 8.57),(D 7 4 2009,"Cafe",Debito 4.34),(D 7 4 2009,"Uber",Debito 19.36),(D 9 4 2009,"Farmacia",Debito 26.77),(D 9 4 2009,"Cafe",Debito 0.41),(D 9 4 2009,"Cafe",Debito 3.11),(D 10 4 2009,"Cafe",Debito 1.53),(D 10 4 2009,"Cafe",Debito 2.87),(D 10 4 2009,"Roupa",Debito 14.71),(D 11 4 2009,"Cafe",Debito 2.37),(D 11 4 2009,"Cafe",Debito 2.81),(D 12 4 2009,"Cafe",Debito 0.91),(D 16 4 2009,"Cafe",Debito 0.14),(D 17 4 2009,"Livraria",Debito 7.13),(D 29 4 2009,"Cafe",Debito 0.32)],Ext 0.2 [(D 6 5 2009,"Salario",Credito 140.06),(D 7 5 2009,"Cafe",Debito 4.66),(D 7 5 2009,"Telefone",Debito 24.11),(D 7 5 2009,"Supermercado",Debito 9.19),(D 7 5 2009,"Supermercado",Debito 31.0),(D 8 5 2009,"Livraria",Debito 10.63),(D 8 5 2009,"Restaurante",Debito 16.23),(D 8 5 2009,"Cafe",Debito 4.71),(D 8 5 2009,"Internet",Debito 34.78),(D 10 5 2009,"Cafe",Debito 1.64),(D 14 5 2009,"Cafe",Debito 2.22),(D 15 5 2009,"Cafe",Debito 0.86),(D 18 5 2009,"Cafe",Debito 0.21)],Ext 2.0e-2 [(D 5 6 2009,"Salario",Credito 194.82),(D 6 6 2009,"Eletricidade",Debito 46.85),(D 6 6 2009,"Cinema",Debito 6.61),(D 7 6 2009,"Combustivel",Debito 58.09),(D 7 6 2009,"Uber",Debito 3.36),(D 7 6 2009,"Cinema",Debito 5.69),(D 8 6 2009,"Combustivel",Debito 47.71),(D 8 6 2009,"Cafe",Debito 3.86),(D 8 6 2009,"Livraria",Debito 13.7),(D 13 6 2009,"Cafe",Debito 1.87),(D 14 6 2009,"Cafe",Debito 2.84),(D 14 6 2009,"Cafe",Debito 3.49),(D 17 6 2009,"Cafe",Debito 0.68)],Ext 9.0e-2 [(D 1 7 2009,"Salario",Credito 185.99),(D 2 7 2009,"Cafe",Debito 1.68),(D 2 7 2009,"Cafe",Debito 2.27),(D 2 7 2009,"Cinema",Debito 7.5),(D 3 7 2009,"Supermercado",Debito 20.29),(D 4 7 2009,"Cinema",Debito 7.54),(D 7 7 2009,"Cafe",Debito 3.24),(D 7 7 2009,"Cinema",Debito 5.32),(D 7 7 2009,"Livraria",Debito 13.26),(D 7 7 2009,"Cafe",Debito 0.66),(D 8 7 2009,"Restaurante",Debito 13.06),(D 8 7 2009,"Cafe",Debito 3.25),(D 8 7 2009,"Cafe",Debito 3.53),(D 8 7 2009,"Livraria",Debito 10.66),(D 8 7 2009,"Farmacia",Debito 9.38),(D 10 7 2009,"Restaurante",Debito 17.63),(D 10 7 2009,"Combustivel",Debito 18.13),(D 10 7 2009,"Farmacia",Debito 23.67),(D 11 7 2009,"Supermercado",Debito 9.02),(D 11 7 2009,"Cafe",Debito 1.91),(D 11 7 2009,"Cafe",Debito 4.54),(D 13 7 2009,"Cafe",Debito 1.54),(D 13 7 2009,"Cafe",Debito 2.27),(D 14 7 2009,"Cinema",Debito 5.01),(D 19 7 2009,"Cafe",Debito 0.35),(D 30 7 2009,"Cafe",Debito 0.34)],Ext 3.0e-2 [(D 2 8 2009,"Salario",Credito 139.5),(D 3 8 2009,"Agua",Debito 39.26),(D 3 8 2009,"Cafe",Debito 0.55),(D 3 8 2009,"Livraria",Debito 10.04),(D 3 8 2009,"Telefone",Debito 24.31),(D 3 8 2009,"Cafe",Debito 1.41),(D 4 8 2009,"Farmacia",Debito 12.57),(D 4 8 2009,"Cinema",Debito 9.89),(D 5 8 2009,"Supermercado",Debito 30.76),(D 5 8 2009,"Cafe",Debito 1.77),(D 6 8 2009,"Cafe",Debito 0.38),(D 7 8 2009,"Cafe",Debito 4.82),(D 7 8 2009,"Cafe",Debito 2.59),(D 12 8 2009,"Cafe",Debito 0.82),(D 23 8 2009,"Cafe",Debito 0.36)],Ext 0.0 [(D 7 9 2009,"Salario",Credito 171.79),(D 8 9 2009,"Cafe",Debito 3.79),(D 8 9 2009,"Livraria",Debito 18.3),(D 8 9 2009,"Roupa",Debito 21.0),(D 9 9 2009,"Cafe",Debito 2.58),(D 10 9 2009,"Farmacia",Debito 29.55),(D 10 9 2009,"Cafe",Debito 1.6),(D 10 9 2009,"Cafe",Debito 0.92),(D 10 9 2009,"Cafe",Debito 4.35),(D 10 9 2009,"Supermercado",Debito 22.85),(D 11 9 2009,"Livraria",Debito 18.76),(D 12 9 2009,"Cafe",Debito 2.27),(D 13 9 2009,"Livraria",Debito 12.33),(D 14 9 2009,"Restaurante",Debito 14.93),(D 15 9 2009,"Livraria",Debito 14.56),(D 18 9 2009,"Cafe",Debito 3.68)],Ext 0.32 [(D 4 10 2009,"Salario",Credito 105.48),(D 5 10 2009,"Uber",Debito 14.9),(D 5 10 2009,"Eletricidade",Debito 83.84),(D 9 10 2009,"Cafe",Debito 4.43),(D 11 10 2009,"Cafe",Debito 2.63)],Ext 0.0 [(D 5 11 2009,"Salario",Credito 117.72),(D 8 11 2009,"Uber",Debito 12.77),(D 8 11 2009,"Combustivel",Debito 27.53),(D 8 11 2009,"Supermercado",Debito 21.08),(D 9 11 2009,"Cafe",Debito 1.16),(D 9 11 2009,"Cinema",Debito 7.8),(D 9 11 2009,"Cafe",Debito 3.66),(D 9 11 2009,"Ginasio",Debito 20.39),(D 10 11 2009,"Cafe",Debito 4.37),(D 10 11 2009,"Uber",Debito 6.03),(D 12 11 2009,"Cafe",Debito 2.67),(D 13 11 2009,"Cafe",Debito 2.04),(D 14 11 2009,"Cafe",Debito 4.61),(D 15 11 2009,"Cafe",Debito 3.32)],Ext 0.29 [(D 5 12 2009,"Salario",Credito 99.66),(D 8 12 2009,"Internet",Debito 38.95),(D 8 12 2009,"Cafe",Debito 4.3),(D 8 12 2009,"Cafe",Debito 4.1),(D 8 12 2009,"Restaurante",Debito 14.46),(D 10 12 2009,"Cafe",Debito 4.75),(D 10 12 2009,"Roupa",Debito 25.87),(D 11 12 2009,"Cafe",Debito 0.84),(D 12 12 2009,"Cafe",Debito 3.42),(D 13 12 2009,"Cafe",Debito 1.45),(D 17 12 2009,"Cafe",Debito 1.56)],Ext 0.25 [(D 1 1 2010,"Salario",Credito 194.19),(D 2 1 2010,"Ginasio",Debito 25.49),(D 2 1 2010,"Cafe",Debito 2.98),(D 2 1 2010,"Agua",Debito 28.61),(D 3 1 2010,"Restaurante",Debito 19.64),(D 3 1 2010,"Seguro",Debito 27.03),(D 3 1 2010,"Combustivel",Debito 74.91),(D 5 1 2010,"Supermercado",Debito 15.48)],Ext 0.3 [(D 4 2 2010,"Salario",Credito 114.77),(D 5 2 2010,"Combustivel",Debito 33.6),(D 5 2 2010,"Cafe",Debito 1.04),(D 5 2 2010,"Cafe",Debito 2.34),(D 6 2 2010,"Cafe",Debito 0.93),(D 6 2 2010,"Seguro",Debito 69.81),(D 10 2 2010,"Cafe",Debito 3.76),(D 11 2 2010,"Cafe",Debito 1.39),(D 11 2 2010,"Cafe",Debito 1.16),(D 11 2 2010,"Cafe",Debito 0.76)]]