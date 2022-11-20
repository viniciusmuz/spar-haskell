module Main where
  import Controllers.TxtController
  import Controllers.InterfaceController
  import Models.Pilha
  import Models.Cartao
  import Models.Sessao
  import Data.Char

  main :: IO()  
  main = do
    putStrLn welcome
    input <- getLine
    menuOptions (map toUpper input)

  welcome :: String
  welcome = "*** Bem-vindo ao Spar! ***" ++ "\n" ++ 
          "Digite a letra correspondente à ação que você deseja executar\n" ++
          "[E]studar\n" ++
          "[A]dicionar cartões\n" ++
          "[R]emover cartões\n" ++
          "[M]odificar cartões\n" ++
          "[C]ustomizar intervalos dos cartões\n" ++
          putLine


  mainMenu:: IO()
  mainMenu = do
          putStrLn initialMenu
          option <- getLine
          putStrLn ""
          menuOptions (map toUpper option)

  menuOptions:: String -> IO ()
  menuOptions option |option == "E" = print("studyMenu")
                     |option == "A" = choosePilhaMenu
                     |option == "R" = removeCardPilhaMenu
                     |option == "M" = editCardMenu
                     |option == "C" = print("Opção ainda em desenvolvimento")
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
        choosePilhaMenu 

  removeCardPilhaMenu:: IO ()
  removeCardPilhaMenu = do
    putStrLn "> Escolha o número da pilha onde deseja remover a carta: "
    numPilha <- readLn
    db <- loadDB
    putStrLn ""

    case (numPilha > 0 && numPilha <= length db) of
      True -> do
        putStrLn putLine
        let pilha = db!!(numPilha-1)
        putStrLn $ "<<  " ++ (nome pilha) ++ "  >>\n"
        putStrLn "Implementar função de remover card"
      False -> do
        putStrLn "\n# Número da pilha inválido inválido #\n"
        choosePilhaMenu

  ditCardMenu:: Deck -> Card -> IO ()
  editCardMenu deck card = do
    putStrLn "> Qual será a frente da carta?"
    newFront <- getLine

    putStrLn "\n> Qual será o verso da carta?"
    newBack <- getLine

    let editedDeck = editCard deck card newFront newBack
    editDeckAndSave (name editedDeck) (cards editedDeck) 
    putStrLn "\nCarta editada com sucesso!\n"
    mainMenu

  --studyMenu::IO()

  errorMenu:: IO()
  errorMenu = do
    putStrLn "################# Opção inválida! #################\n"
    mainMenu    