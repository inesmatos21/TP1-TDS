import Test.HUnit
import Test.QuickCheck
import Data.Foldable (Foldable(sum))
import Data.Bifunctor

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
saldo (Ext s movs) = s + totalCred - totalDeb
  where
    (totalCred,totalDeb) = creDeb (Ext s movs)

-- Geradores

round2 :: Float -> Float
round2 x = fromIntegral (round (x * 100)) / 100

genDebito :: Gen (String, Float)
genDebito = second round2 <$> frequency
  [ (2,  (,) "Restaurante"  <$> choose (10.0, 20.0))
  , (5,  (,) "Supermercado" <$> choose (5.0,  100.0))
  , (1,  (,) "Telefone"     <$> choose (10.0, 40.0))
  , (1,  (,) "Internet"     <$> choose (10.0, 50.0))
  , (1,  (,) "Eletricidade" <$> choose (40.0, 100.0))
  , (1,  (,) "Agua"         <$> choose (20.0, 60.0))
  , (4,  (,) "Combustivel"  <$> choose (10.0, 80.0))
  , (2,  (,) "Farmacia"     <$> choose (5.0,  30.0))
  , (2,  (,) "Cinema"       <$> choose (5.0,  10.0))
  , (3,  (,) "Roupa"        <$> choose (2.0,  100.0))
  , (2,  (,) "Livraria"     <$> choose (5.0,  20.0))
  , (15, (,) "Cafe"         <$> choose (0.1,  5.0))
  , (2,  (,) "Uber"         <$> choose (2.5,  20.0))
  , (1,  (,) "Ginasio"      <$> choose (20.0,  30.0))
  , (1,  (,) "Seguro"       <$> choose (20.0, 90.0))
  ]

debitosObrigatorios :: [(String, (Float, Float))]
debitosObrigatorios =
  [ ("Agua",         (20.0,  60.0))
  , ("Eletricidade", (40.0,  100.0))
  , ("Internet",     (10.0,  50.0))
  , ("Telefone",     (10.0,  40.0))
  , ("Seguro",       (20.0,  90.0))
  ]

gerarDebitosObrigatorios :: Gen [(String, Movimento)]
gerarDebitosObrigatorios = mapM genOne debitosObrigatorios
  where
    genOne (nome, (lo, hi)) = do
      val <- round2 <$> choose (lo, hi)
      return (nome, Debito val)

genCredito :: Gen Float
genCredito = round2 <$> choose (700.0, 1200)

genMovimento :: Gen (String, Movimento)
genMovimento = frequency
  [ (95, do
      (nome, val) <- genDebito
      return (nome, Debito val))
  , (5, do
      val <- genCredito
      return ("Salario", Credito val))
  ]


genData :: Gen Data
genData = do
       dia <- elements[1..31]
       mes <- if dia == 31 then elements[1,3,5,7,8,10,12] else elements[1..12]
       ano <- elements[2000..2026]
       return $ D dia mes ano

listOfDebits :: [String]
listOfDebits = ["Restaurante", "Supermercado", "Telefone", "Internet",
                "Eletricidade", "Agua", "Combustivel", "Farmacia",
                "Cinema", "Roupa", "Livraria", "Cafe",
                "Uber", "Ginasio", "Seguro"]

genString :: Movimento -> Gen String
genString (Credito x) = return "Salario"
genString (Debito x) = elements listOfDebits