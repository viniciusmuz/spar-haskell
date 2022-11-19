module Main where
  import Controllers.TxtController
  import Controllers.InterfaceController
  import Models.Pilha
  import Models.Cartao
  import Data.Char

  main :: IO()  
  main = do
    putStrLn welcome
    input <- getLine :: IO String
    menuOptions (map toUpper input)

  welcome :: String
  welcome = "*** Bem-vindo ao Spar! ***" ++ "\n" ++ 
          "Digite a letra correspondente à ação que você deseja executar\n" ++
          "[E]studar\n" ++
          "[A]dicionar cartões\n" ++
          "[R]emover cartões\n" ++
          "[M]odificar cartões\n" ++
          "[C]ustomizar intervalos dos cartões\n"
          putLine


  mainMenu:: String
  mainMenu = do
          putStrLn initialMenu
          option <- getLine
          putStrLn ""
          menuOptions (map toUpper option)

  menuOptions:: String -> IO ()
  menuOptions option |option == "E" = "Opção ainda em desenvolvimento"
                    |option == "A" = choosePilhaMenu
                    |option == "R" = "Opção ainda em desenvolvimento"
                    |option == "M" = "Opção ainda em desenvolvimento"
                    |option == "C" = "Opção ainda em desenvolvimento"
                    |otherwise = errorMenu

  choosePilhaMenu:: IO ()
  choosePilhaMenu = do
    putStrLn "> Escolha o número da pilha onde deseja adicionar a carta: "
    numPilha <- readLn
    db <- loadDB
    putStrLn ""

    case (numPilha > 0 && numPilha <= length db) of
      True -> do
        putStrLn putLine
        let pilha = db!!(numPilha-1)
        putStrLn $ "<<  " ++ (nome pilha) ++ "  >>\n"
        putStrLn "Implementar função de add card"
      False -> do
        putStrLn "\n# Número da pilha inválido inválido #\n"
        chooseDeckMenu 

  errorMenu:: IO()
  errorMenu = do
    putStrLn "################# Opção inválida! #################\n"
    mainMenu    