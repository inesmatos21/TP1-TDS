module Properties where 

import Test.QuickCheck
import Functions
import Generators

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

prop_verDataExtractos :: Extractos -> Bool
prop_verDataExtractos (Extractos exts) = 
  all prop_verDataExtracto exts

-- Propriedade que verifica se os movimentos num extrato estão ordenados cronologicamente 

prop_movimentosOrdenados :: Extracto -> Bool
prop_movimentosOrdenados (Ext _ movs) = diasOrdenados movs

diasOrdenados :: [(Data, String, Movimento)] -> Bool
diasOrdenados [] = True
diasOrdenados [_] = True
diasOrdenados ((D d1 m1 a1, str1, mov1) : (D d2 m2 a2, str2, mov2) : rest) = 
    d1 <= d2 && m1 <= m2 && a1 <= a2 && diasOrdenados ((D d2 m2 a2, str2, mov2) : rest)

prop_movimentosOrdenadosExtractos :: Extractos -> Bool
prop_movimentosOrdenadosExtractos (Extractos exts) = verificaTodosOrdenados exts

verificaTodosOrdenados :: [Extracto] -> Bool
verificaTodosOrdenados [] = True
verificaTodosOrdenados (extrato : resto) = prop_movimentosOrdenados extrato && verificaTodosOrdenados resto


-- Propriedade que verifica se os extratos estão por ordem cronológica

prop_extratosCronologicos :: Extractos -> Bool
prop_extratosCronologicos (Extractos exts) = extratosOrdenados exts

extratosOrdenados :: [Extracto] -> Bool
extratosOrdenados [] = True
extratosOrdenados [_] = True
extratosOrdenados (e1 : e2 : rest) = verificaOrdem e1 e2 && extratosOrdenados (e2 : rest)

verificaOrdem :: Extracto -> Extracto -> Bool
verificaOrdem (Ext _ []) _ = True
verificaOrdem _ (Ext _ []) = True
verificaOrdem (Ext _ ((D _ m1 a1, _, _) : _)) (Ext _ ((D _ m2 a2, _, _) : _)) = 
    a1 < a2 || (a1 == a2 && m1 <= m2)


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

-- Propriedade que verifica se não há movimentos com valores negativos ou zero
prop_movimentosPositivos :: Extracto -> Bool
prop_movimentosPositivos (Ext _ movs) = valoresPositivos movs

valoresPositivos :: [(Data, String, Movimento)] -> Bool
valoresPositivos [] = True
valoresPositivos ((_, _, mov):rest) = valorMov mov > 0.00 && valoresPositivos rest
