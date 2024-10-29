module BlackJack (
    EstadoJogo(..),
    Baralho(..),
    getBaralho,
    tamanho,
    pontos,
    distribuir,
    hit,
    casa,
    vitorioso
)
where 
--funções usadas no trabalho anterior
data Baralho = Baralho [String]

getBaralho::Baralho -> [String]
getBaralho (Baralho xs) = xs

instance Show Baralho where 
    show (Baralho str) = show2 str

show2::[String] -> String
show2 [x] = x
show2 (x:xs) = x ++ " " ++ show2 xs


data EstadoJogo = EstadoJogo {baralho        :: Baralho
                             ,baralhoJogador :: Baralho
                             ,baralhoMesa    :: Baralho
                             ,creditos       :: Int    } 


tamanho :: Baralho -> Int
tamanho (Baralho x) = length x



pontos:: [String] -> Int 
pontos xs = ver $ filter (<22) $ foldl(\acc x -> if x == 0 then  put 1 acc ++ put 11 acc else put x acc) [0] [valor x| x <- xs]
        where
        put = \z-> \xs -> [z + x| x <- xs]
        ver xs 
            | xs == []  = 22
            | otherwise = maximum xs
        
        
valor:: String -> Int
valor (x:_:[]) 
            | x == 'A' = 0
            | x `elem` "23456789" = read [x]
            | otherwise = 10


            

distribuir::EstadoJogo ->  EstadoJogo
distribuir (EstadoJogo (Baralho x) _ _ cr) = EstadoJogo (Baralho (drop 4 x)) (Baralho (take 2 x)) (Baralho (drop 2 (take 4 x))) cr 


hit:: EstadoJogo -> EstadoJogo
hit (EstadoJogo (Baralho br) (Baralho x) y z) = EstadoJogo (Baralho (tail br)) (Baralho (x ++ take 1 br)) y z


casa :: EstadoJogo -> EstadoJogo
casa (EstadoJogo (Baralho br) (Baralho x) (Baralho y) z) 
                            |pontos x == 22 = EstadoJogo (Baralho br) (Baralho x) (Baralho y) z -- retirar esta linha assim que for possível testar
                            |pontos y < 17  = casa $ EstadoJogo (Baralho (drop 1 br)) (Baralho x) (Baralho (y ++ take 1 br)) z
                            |otherwise      = EstadoJogo (Baralho br) (Baralho x) (Baralho y) z


vitorioso:: EstadoJogo -> Int -> EstadoJogo
vitorioso (EstadoJogo x (Baralho j) (Baralho mesa) cr) aposta
                                                |jGanha  j mesa          =  EstadoJogo x (Baralho j) (Baralho mesa) (cr + aposta)
                                                |pontos j == pontos mesa =  EstadoJogo x (Baralho j) (Baralho mesa) cr
                                                |otherwise               =  EstadoJogo x (Baralho j) (Baralho mesa) (cr - aposta) 
                                                where jGanha j mesa = pontos j > pontos mesa && pontos j < 22 || pontos mesa == 22 && pontos j != 22
