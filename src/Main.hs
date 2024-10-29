{-
Programming Principles
Project 3 - Submission Model

> stack ghc Main.hs

The above command produces a Main executable, which should be executable
through one of the following four types of commands:

> ./Main file -- loads a deck to play Blackjack
> ./Main      -- loads the default deck default.bar
> ./Main -n X -- loads a random deck formed by X normal decks of cards
> ./Main -t   -- runs the tests
-}
import System.Environment
import System.Random
import System.Random.Shuffle
import System.Directory
import BlackJack
import Testes
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

main = do 
    args <- getArgs
    br <- defenir $ concat args
    if br == ["over"] then 
        return ()
    else do
        let cr = 100
        let bar = Baralho br
        inicioRonda bar cr

inicioRonda:: Baralho -> Int -> IO()
inicioRonda br cr =  do 
                  putStrLn $ "cards: "    ++ show (tamanho br)
                  putStrLn $ "credits: " ++ show cr
                  let ej = distribuir $ EstadoJogo br (Baralho []) (Baralho []) cr
                  aposta2 <- getAposta (br) (creditos ej)
                  if aposta2 == 0 then
                    putStrLn $ "final balance: " ++ show (creditos ej) 
                  else do
                    printCartas (baralhoJogador ej) (baralhoMesa ej)
                    ronda ej aposta2


defenir :: String -> IO [String]
defenir (xs) 
        |null xs           = do
                            str <- readFile("default.bar")
                            return $ lines str

        |take 2 xs == "-n" = do 
                            let times = read $ snd $ splitAt 2 xs :: Int
                            let str = getBaralho $ converte baralho1  
                            let bar = join str times 
                            gerador <- getStdGen
                            return $ shuffle' bar (length bar) gerador
        
        |take 2 xs == "-t" = do
                            quickCheck prop_mao_inicial
                            quickCheck prop_creditos_pos_ronda
                            quickCheck prop_final_vez_casa
                            quickCheck prop_jogador_nao_perde_dinheiro
                            quickCheck prop_jogador_nao_ganha_dinheiro
                            quickCheck prop_distribuicao_nao_perde
                            return ["over"]
        
        |otherwise         = do 
                            existe <- doesFileExist xs
                            if existe then do
                                    str <- readFile(xs)
                                    return $ lines str
                            else do 
                                putStrLn "./Main file -- loads a deck to play Blackjack\n./Main -- loads the default deck default.bar\n./Main -n X -- loads a random deck formed by X normal decks of cards\n./Main -t -- runs the tests"
                                return ["over"]


join :: [String] -> Int -> [String]
join str times 
            | times <= 1 = str
            | otherwise  = str ++ join str (times - 1)


getAposta :: Baralho -> Int -> IO Int
getAposta br cr = do 
                if(tamanho br <= 20 || cr == 0) then do 
                    return 0
                else do
                    x <- getLine::IO String
                    if x == "sair" then
                        return 0
                    else do
                        let ap = read $ snd $ splitAt 8 x  :: Int 
                        if 0 < ap && ap <= cr then 
                            return ap
                        else 
                            getAposta br cr


printCartas:: Baralho -> Baralho -> IO()
printCartas brj brm = do 
                        putStrLn $ "player: " ++ show brj
                        putStrLn $ "house: " ++ show brm 





ronda::EstadoJogo -> Int -> IO()
ronda (EstadoJogo (Baralho br) (Baralho j) (Baralho m) cr) aposta = do 
                                                                if aposta == 0 then do
                                                                    putStrLn $ "cards: "      ++ show (tamanho $ Baralho br)
                                                                    putStrLn $ "credits: "    ++ show cr
                                                                    putStrLn $ "final balance: " ++ show cr
                                                                else do
                                                                    if pontos j > 21 then do 
                                                                                        let estj = vitorioso (EstadoJogo(Baralho br) (Baralho j) (Baralho m) cr) aposta
                                                                                        fimDeRonda cr (creditos estj)
                                                                                        inicioRonda (baralho estj) (creditos estj)
                                                                    else do
                                                                            if pontos j == 21 then do 
                                                                                                    let estj = vitorioso (casa (EstadoJogo(Baralho br) (Baralho j) (Baralho m) cr)) aposta
                                                                                                    printCartas (baralhoJogador estj) (baralhoMesa estj)
                                                                                                    fimDeRonda cr (creditos estj)
                                                                                                    inicioRonda (baralho estj) (creditos estj)
                                                                            else do 
                                                                                tipo <- getLine :: IO String
                                                                                if tipo == "hit" then do
                                                                                    let estj = hit (EstadoJogo(Baralho br) (Baralho j) (Baralho m) cr)
                                                                                    printCartas (baralhoJogador estj) (baralhoMesa estj) 
                                                                                    ronda estj aposta
                                                                                else do 
                                                                                        if tipo == "stand" then do
                                                                                            let estj = vitorioso (casa (EstadoJogo(Baralho br) (Baralho j) (Baralho m) cr)) aposta
                                                                                            printCartas (baralhoJogador estj) (baralhoMesa estj) 
                                                                                            fimDeRonda cr (creditos estj)
                                                                                            inicioRonda (baralho estj) (creditos estj)
                                                                                        else 
                                                                                            error "Wrong input"

                                                                        
                                                                                        
                                                                                        

   
fimDeRonda:: Int -> Int -> IO()
fimDeRonda cr1 cr2 = do 
                        if cr1 == cr2 then 
                            putStrLn "Draw"
                        else do 
                                if cr1 > cr2 then 
                                    putStrLn "Defeat"
                                else 
                                    putStrLn "Victory"