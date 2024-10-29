module Testes (
    prop_mao_inicial,
    prop_creditos_pos_ronda,
    prop_final_vez_casa,
    prop_jogador_nao_perde_dinheiro,
    prop_jogador_nao_ganha_dinheiro,
    prop_distribuicao_nao_perde,
    CartaValida(..),
    converte,
    baralho1
)
where 
import BlackJack
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

data CartaValida = CV {carta :: String} deriving (Show)

baralho1::[CartaValida]
baralho1 = [CV [x , y]| y <-ys, x<-xs]
   where xs ="A23456789TJQK"
         ys ="SHDC"

converte:: [CartaValida] -> Baralho
converte xs = Baralho [x | (CV x) <- xs]

instance Arbitrary CartaValida where
    arbitrary = elements baralho1

prop_mao_inicial :: [CartaValida] -> Property
prop_mao_inicial xs = length xs >= 2 ==> pontos [carta $ head xs, carta $ last (take 2 xs)] <= 21

prop_creditos_pos_ronda :: [CartaValida] -> Property
prop_creditos_pos_ronda xs  = length xs > 10 ==> cre == 100 + 5 || cre == 100 - 5 || cre == 100  
                                                                where cre = creditos $ vitorioso (casa $ hit $ distribuir (EstadoJogo (converte xs) (Baralho []) (Baralho []) 100)) 5

-- 11 cards to cover all possible maximum cases without exceeding 1000 cases tested in QuickCheck
-- we are assuming a play where the player stands after receiving the cards
prop_final_vez_casa :: [CartaValida] -> Property
prop_final_vez_casa xs = length xs > 10 ==> pontos (getBaralho $ baralhoMesa $ casa $ distribuir (EstadoJogo (converte xs) (Baralho []) (Baralho []) 100)) >= 17

-- if the player has 21 points, they never lose money
-- this avoids creating a game state to get the player's card points
prop_jogador_nao_perde_dinheiro :: [CartaValida] -> Property
prop_jogador_nao_perde_dinheiro xs = length xs > 9 
                                     ==> creditos (vitorioso (casa $ distribuir (EstadoJogo (converte ((CV "AS"):(CV "TS"):xs)) (Baralho []) (Baralho []) 100)) 5) >= 100

-- The minimum possible points in a player's hand is 4 points
-- This is because in the lowest combination they will have two 2_ cards
prop_jogador_nao_ganha_dinheiro :: [CartaValida] -> Property
prop_jogador_nao_ganha_dinheiro xs = length xs > 3 ==> pontos (getBaralho $ baralhoJogador x) > 3
                                    where x = distribuir (EstadoJogo (converte (xs)) (Baralho []) (Baralho []) 100) 

-- After the cards are dealt, it is not possible for either the house or the table to have more than 21 points
prop_distribuicao_nao_perde :: [CartaValida] -> Property
prop_distribuicao_nao_perde xs = length xs > 3 ==>
                                            pontos (getBaralho (baralhoJogador ej)) < 22 && pontos (getBaralho (baralhoMesa ej)) < 22
                                            where ej = distribuir (EstadoJogo (converte xs) (Baralho []) (Baralho []) 100)
