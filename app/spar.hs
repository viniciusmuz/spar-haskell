module Main where
  import Controllers.TxtController
  import Controllers.InterfaceController
  import Controllers.PilhaController
  import Models.Pilha
  import Models.Cartao
  import Models.Sessao
  import Data.Char
  import Data.Time
  import Data.Time.Clock
  import Data.Time.Calendar
  import Data.Time.Calendar.OrdinalDate

  welcome :: String
  welcome = "*** Bem-vindo ao Spar! ***" ++ "\n" ++ 
          "Digite a letra correspondente à ação que você deseja executar\n" ++
          "[E]studar\n" ++
          "[A]dicionar cartões\n" ++
          "[R]emover cartões\n" ++
          "[M]odificar cartões\n" ++
          "[C]ustomizar intervalos dos cartões\n" ++
          putLine

  main :: IO()  
  main = do
    putStrLn welcome
    input <- getLine
    menuOptions (map toUpper input)

  menuOptions:: String -> IO ()
  menuOptions option |option == "E" = print("studyMenu") 
                     |option == "A" = choosePilhaMenu option
                     |option == "R" = choosePilhaMenu option
                     |option == "M" = choosePilhaMenu option
                     |option == "C" = print("Opção ainda em desenvolvimento") 
                     |otherwise = errorMenu  

  mainMenu:: IO()
  mainMenu = do
          putStrLn initialMenu
          option <- getLine
          putStrLn ""
          menuOptions (map toUpper option)
  

  
  directMenu:: Pilha -> String -> IO()
  directMenu pilha string |string == "E" = print("studyMenu")
                          |string == "A" = addCardPilhaMenu pilha
                          |string == "R" = print("opção ainda em dev")
                          |string == "M" = print("opção ainda em dev")
                          |string == "C" = print("opção ainda em dev")  

  choosePilhaMenu:: String -> IO ()
  choosePilhaMenu opcao = do
    putStrLn "> Escolha o número da pilha onde deseja realizar a operação: "
    numPilha <- readLn 
    db <- loadDB
    putStrLn ""

    case (numPilha > 0 && numPilha <= length db) of
      True -> do
        putStrLn putLine
        let pilha = db!!(numPilha-1)
        putStrLn $ "<<  " ++ (nome pilha) ++ "  >>\n"
        directMenu pilha opcao
      False -> do
        putStrLn "\n# Número da pilha inválido inválido #\n"
        choosePilhaMenu opcao
  
  addCardPilhaMenu:: Pilha -> IO ()
  addCardPilhaMenu pilha = do
    putStrLn putLine
    putStrLn "> Qual será a frente da carta?"
    front <- getLine

    putStrLn "\n> Qual será o verso da carta?"
    back <- getLine

    putStrLn "\n> Qual o dia de hj"
    criacao <- readLn :: IO Int

    putStrLn "\n> Qual será o dia de vencimento carta?"
    vencimento <- readLn :: IO Int
    
    let vencimentoDay = toEnum vencimento
    let criacaoDay = toEnum criacao

    let newCard = Cartao criacaoDay vencimentoDay front back
    let editedPilha = adicionarCartao pilha newCard
    editPilhaAndSave (nome editedPilha) (cartoes editedPilha) 
    putStrLn "\nCarta adicionada com sucesso!\n"
    mainMenu 
  
  errorMenu:: IO()
  errorMenu = do
    putStrLn "################# Opção inválida! #################\n"
    mainMenu

  date :: IO (Integer, Int, Int) -- :: (year, month, day)
  date = getCurrentTime >>= return . toGregorian . utctDay

  day :: IO Int
  day = (\(_, _, d) -> d) <$> date    