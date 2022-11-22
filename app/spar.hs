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
          "[C]Criar Pilha\n"++
          "[R]Remover Pilha\n"++
          "[D]Editar Pilha\n" ++
          "[E]Estudar\n" ++
          putLine

  main :: IO()  
  main = do
    putStrLn welcome
    input <- getLine
    menuOptions (map toUpper input)

  menuOptions:: String -> IO ()
  menuOptions option |option == "E" = choosePilhaMenu 
                     |option == "C" = createPilha 
                     |option == "D" = choosePilhaMenu 
                     |option == "R" = choosePilhaMenu
                     |otherwise = errorMenu  

  mainMenu:: IO()
  mainMenu = do
          putStrLn initialMenu
          option <- getLine
          putStrLn ""
          menuOptions (map toUpper option)
  

  operationsPilha:: Pilha -> String -> IO()
  operationsPilha pilha input |input == "A" = addCardPilha pilha
                              |input == "E" = editCardPilha pilha (cartoes pilha)
                              |input == "R" = choosePilhaMenu
                              |input == "X" = mainMenu
                              |otherwise = errorMenu

  choosePilhaMenu:: IO ()
  choosePilhaMenu = do
    putStrLn "> Escolha o número da pilha onde deseja realizar a operação: "
    numPilha <- readLn
    db <- loadDB
    putStrLn ""

    case (numPilha > 0 && numPilha <= length db) of
      True -> do
        putStrLn putLine
        let pilha = db!!(numPilha-1)
        putStrLn $ "<<  " ++ (nome pilha) ++ "  >>\n"
        print(pilha)
        putStrLn "[A] Add carta [E] Editar Card  [R] Remover deck             [X] Voltar\n"
        option <- getLine
        putStrLn ""
        operationsPilha pilha option
      False -> do
        putStrLn "\n# Número da pilha inválido inválido #\n"
        choosePilhaMenu
  
  createPilha:: IO ()
  createPilha = do
    putStrLn "Digite o nome da pilha:"
    namePilha <- getLine
    addAndSave namePilha
    putStrLn "\nPilha criada com sucesso!\n"
    mainMenu

  addCardPilha:: Pilha -> IO ()
  addCardPilha pilha = do
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

  editCardPilha:: Pilha -> [Cartao] -> IO()
  editCardPilha pilha cards = do
    putStrLn putLine
    print(show (nome pilha))
    putStrLn ""
    print(cards)
    putStrLn "Escolha o cartão que deseja editar: "
    numCartao <- readLn
    case (numCartao > 0 && numCartao <= length cards) of
      True -> do
        putStrLn putLine
        let cartao = cards!!(numCartao-1)
        putStrLn "> Qual será a frente da carta?"
        novaFrente <- getLine

        putStrLn "\n> Qual será o verso da carta?"
        novoVerso <- getLine
        
        let editedPilha = editarCartao pilha cartao novaFrente novoVerso
        print(show(nome editedPilha))
        editPilhaAndSave (nome editedPilha) (cartoes editedPilha)
        putStrLn "\nCarta editada com sucesso!\n" 
        mainMenu
      False -> do 
        putStrLn "\n# Número do Cartão é inválido #\n"
        editCardPilha pilha cards
  

  errorMenu:: IO()
  errorMenu = do
    putStrLn "################# Opção inválida! #################\n"
    mainMenu

  date :: IO (Integer, Int, Int) -- :: (year, month, day)
  date = getCurrentTime >>= return . toGregorian . utctDay

  day :: IO Int
  day = (\(_, _, d) -> d) <$> date    