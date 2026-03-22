module Properties where 

import Test.QuickCheck
import Functions
import Generators
import Functions (Extracto, Movimento, valorMov)

-- Propriedade que verifica se o saldo inicial de um extrato é o saldo inicial do anterior mais os movimentos

saldoInicial :: Extracto -> Float
saldoInicial (Ext s _) = s

proximoSaldo :: [Extracto] -> Bool
proximoSaldo [] = True
proximoSaldo [_] = True
proximoSaldo (e1:e2:rest) = saldo e1 == saldoInicial e2 && proximoSaldo (e2:rest)

prop_saldoCorreto :: Extractos -> Bool
prop_saldoCorreto (Extractos exts) = proximoSaldo exts

-- Propriedade que verifica se num extrato todos os movimentos são do mesmo mês e ano

prop_verDataExtracto :: Extracto -> Bool
prop_verDataExtracto (Ext _ []) = True
prop_verDataExtracto (Ext _ ((D _ m a, _, _):movs)) = mesmoMesAno m a movs

mesmoMesAno :: Int -> Int -> [(Data, String, Movimento)] -> Bool
mesmoMesAno _ _ [] = True
mesmoMesAno m a ((D _ m2 a2, _, _):xs) = m == m2 && a == a2 && mesmoMesAno m a xs

-- Propriedade que verifica que o saldo nunca fica negativo

prop_saldoNaoNegativo :: Extracto -> Bool
prop_saldoNaoNegativo (Ext s movs) = verificaSaldos s movs

verificaSaldos :: Float -> [(Data, String, Movimento)] -> Bool
verificaSaldos s [] = True
verificaSaldos s ((_,_,mov) : rest) = (novoSaldo >= 0.00) && verificaSaldos novoSaldo rest
    where
        novoSaldo = adicionaMovimento s mov

adicionaMovimento :: Float -> Movimento -> Float
adicionaMovimento s mov = if isCredito mov then s + valorMov mov else s - valorMov mov

prop_saldoNaoNegativoExtractos :: Extractos -> Bool
prop_saldoNaoNegativoExtractos (Extractos exts) = all prop_saldoNaoNegativo exts
