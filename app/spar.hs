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
    

    menu <- menuPilhas
    putStrLn menu

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
          
          menu <- menuPilhas
          putStrLn menu
          
          option <- getLine
          putStrLn ""
          menuOptions (map toUpper option)
  

  operationsPilha:: Pilha -> String -> IO()
  operationsPilha pilha input |input == "A" = addCardPilha pilha
                              |input == "E" = editCardPilha pilha (cartoes pilha)
                              |input == "R" = removePilha pilha
                              |input == "X" = mainMenu
                              |otherwise = errorMenu

  studyPilhaMenu:: IO()
    putStrLn "> Escolha o número da pilha que deseja estudar: "
    numPilha <- readLn
    db <- loadDB
    putStrLn ""

    case (numPilha > 0 && numPilha <= length db) of
      True -> do
        putStrLn putLine
        let pilha = db!!(numPilha-1)
        putStrLn $ "<<  " ++ (nome pilha) ++ "  >>\n"
        print(pilha)
        chooseCardMenu pilha (cartoes pilha)
      False -> do
        putStrLn "\n# Número da pilha inválido inválido #\n"
        choosePilhaMenu

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
        putStrLn "[A] Add carta [E] Editar Carta  [R] Remover Pilha             [X] Voltar\n"
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

  
  removePilha:: Pilha -> IO ()
  removePilha pilha = do
    putStrLn "\n> Tem certeza que deseja remover a pilha? [Y]"
    option <- getLine 
    case (map toUpper option) == "Y" of
      True -> do
        removeAndSave pilha
        putStrLn "\nO pilha foi removido com sucesso!\n"
        mainMenu
      False -> do
        mainMenu

  verifyMonth:: Int -> Int -> Int
  verifyMonth criacao mes |mes == 1 = criacao + 61
                          |mes == 2 = criacao + 92
                          |mes == 3 = criacao + 120
                          |mes == 4 = criacao + 151
                          |mes == 5 = criacao + 181
                          |mes == 6 = criacao + 212
                          |mes == 7 = criacao + 242
                          |mes == 8 = criacao + 273
                          |mes == 9 = criacao + 303
                          |mes == 10 = criacao + 334
                          |mes == 11 = criacao + 265
                          |mes == 12 = criacao + 30

  addCardPilha:: Pilha -> IO ()
  addCardPilha pilha = do
    putStrLn putLine
    putStrLn "> Qual será a frente da carta?"
    front <- getLine

    putStrLn "\n> Qual será o verso da carta?"
    back <- getLine

    putStrLn "\n> Qual o data de hoje? "
    dia <- readLn :: IO Int

    putStrLn "\n> Qual o mês atual? (Janeiro = 1, Fevereiro = 2,...) "
    mes <- readLn :: IO Int

    putStrLn "\n> Quanto dias essa carta deve durar? "
    vencimento <- readLn :: IO Int

    let criacao = verifyMonth dia mes
    let criacaoDay = toEnum (criacao + 59883)
    let vencimentoDay = toEnum (criacao + vencimento + 59883)
    
    let newCard = Cartao criacaoDay vencimentoDay 0 front back
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
  
  chooseCardMenu:: Pilha -> [Cartoes]-> IO()
  chooseCardMenu pilha cards = do
    pilhaSearch <- search (nome pilha)
    case (length (cartoes pilhaSearch)) == 0 of
      True -> do
        putStrLn putLine
        putStrLn "              Essa não possui cartões:(           \n"
        mainMenu
      False -> do  
        studyCards pilha cards

  studyCard:: Pilha -> [Cartao] -> IO()
  studyCard pilha cartoes = do
    
    
  errorMenu:: IO()
  errorMenu = do
    putStrLn "################# Opção inválida! #################\n"
    mainMenu

  date :: IO (Integer, Int, Int) -- :: (year, month, day)
  date = getCurrentTime >>= return . toGregorian . utctDay

  day :: IO Int
  day = (\(_, _, d) -> d) <$> date    