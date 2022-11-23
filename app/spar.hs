module Main where
  import Controllers.TxtController
  import Controllers.InterfaceController
  import Controllers.PilhaController
  import Controllers.SessaoController
  import Models.Pilha
  import Models.Cartao
  import Models.Sessao
  import Data.Char
  import Data.Time
  import Data.Time.Clock
  import Data.Time.Calendar
  import Data.Time.Calendar.OrdinalDate
  
  hoje = 59883

  welcome :: String
  welcome = "*** Bem-vindo ao Spar! ***" ++ "\n" ++ 
          "Digite a letra correspondente à ação que você deseja executar\n" ++
          "[C]Criar Pilha\n"++ -- FEITO
          "[R]Remover Pilha\n"++ -- FEITO
          "[D]Editar Pilha\n" ++ -- FEITO
          "[E]Estudar\n" ++ -- FAZENDO
          putLine

  main :: IO()  
  main = do
    putStrLn "Para começar, digite a data de hoje: \n"
    date <- readLn :: IO Int
    putStrLn "Agora digite o número correspondente ao mês atual: \n"
    mes <- readLn :: IO Int
    let hojeDay = verifyMonth date mes
    let dataHoje = toEnum (hojeDay + hoje)
    putStrLn putLine
    let sessao = Sessao dataHoje []
    writeDBSessao [sessao]

    putStrLn welcome
    
    menu <- menuPilhas
    putStrLn menu

    input <- getLine
    menuOptions sessao (map toUpper input)

  menuOptions:: Sessao -> String -> IO ()
  menuOptions sessao option |option == "E" = studyPilhaMenu sessao -- FAZENDO 
                            |option == "C" = createPilha sessao
                            |option == "D" = choosePilhaMenu sessao
                            |option == "R" = choosePilhaMenu sessao
                            |otherwise = errorMenu sessao

  mainMenu:: Sessao -> IO()
  mainMenu sessao = do
          putStrLn initialMenu
          
          menu <- menuPilhas
          putStrLn menu
          
          option <- getLine
          putStrLn ""
          menuOptions sessao (map toUpper option)
  

  operationsPilha:: Sessao -> Pilha -> String -> IO()
  operationsPilha sessao pilha input |input == "A" = addCardPilha sessao pilha
                                      |input == "E" = editCardPilha sessao pilha (cartoes pilha)
                                      |input == "R" = removePilha sessao pilha
                                      |input == "X" = mainMenu sessao
                                      |otherwise = errorMenu sessao

  studyPilhaMenu:: Sessao -> IO()
  studyPilhaMenu sessao = do
    putStrLn "> Escolha o número da pilha que deseja estudar: "
    numPilha <- readLn :: IO Int
    db <- loadDB
    putStrLn ""

    case (numPilha > 0 && numPilha <= length db) of
      True -> do
        putStrLn putLine
        let pilha = db!!(numPilha-1)
        putStrLn $ "<<  " ++ (nome pilha) ++ "  >>\n"
        print(pilha)
        chooseCardMenu sessao pilha (cartoes pilha)
      False -> do
        putStrLn "\n# Número da pilha inválido inválido #\n"
        choosePilhaMenu sessao

  choosePilhaMenu:: Sessao -> IO ()
  choosePilhaMenu sessao = do
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
        operationsPilha sessao pilha option
      False -> do
        putStrLn "\n# Número da pilha inválido inválido #\n"
        choosePilhaMenu sessao
  
  createPilha:: Sessao -> IO ()
  createPilha sessao = do
    putStrLn "Digite o nome da pilha:"
    namePilha <- getLine
    addAndSave namePilha
    putStrLn "\nPilha criada com sucesso!\n"
    mainMenu sessao

  
  removePilha:: Sessao -> Pilha -> IO ()
  removePilha sessao pilha = do
    putStrLn "\n> Tem certeza que deseja remover a pilha? [Y]"
    option <- getLine 
    case (map toUpper option) == "Y" of
      True -> do
        removeAndSave pilha
        putStrLn "\nO pilha foi removido com sucesso!\n"
        mainMenu sessao
      False -> do
        mainMenu sessao

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
                          |mes == 11 = criacao
                          |mes == 12 = criacao + 30

  addCardPilha:: Sessao -> Pilha -> IO ()
  addCardPilha sessao pilha = do
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
    mainMenu sessao

  editCardPilha:: Sessao -> Pilha -> [Cartao] -> IO()
  editCardPilha sessao pilha cards = do
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
        mainMenu sessao
      False -> do 
        putStrLn "\n# Número do Cartão é inválido #\n"
        editCardPilha sessao pilha cards
  
  chooseCardMenu:: Sessao -> Pilha -> [Cartao]-> IO()
  chooseCardMenu sessao pilha cards = do
    pilhaSearch <- search (nome pilha)
    case (length (cartoes pilhaSearch)) == 0 of
      True -> do
        putStrLn putLine
        putStrLn "              Essa não possui cartões:(           \n"
        mainMenu sessao
      False -> do  
        studyCards sessao pilha cards

  getFase:: [Cartao] -> Int -> [Cartao]
  getFase [] fase = []
  getFase (a:xs) fase = do
    if (fase ) == 0 then [a] ++ getFase xs fase
    else getFase xs fase

  studyCards:: Sessao -> Pilha -> [Cartao] -> IO()
  studyCards sessao pilha cartoes = do
    shufflePilhaAndSave pilha
    
    let headCartao = head cartoes
    putStrLn "Aperte enter para ver a frente do cartão: "
    getLine
    print (frente headCartao)
    putStrLn putLine
    putStrLn "Aperte enter para ver o verso do cartão: "
    getLine
    print (verso headCartao)
    putStrLn putLine
    putStrLn "[A]certei!\n[E]rrei :(\n "
    input <- getLine
    if (map toUpper input) == "A" then do
      let newPilha = addFase headCartao pilha 
      editSessaoAndSave (dataEstudo sessao) ((cartoesEstudados sessao) ++ [headCartao])
      studyCards (searchSessao (dataEstudo sessao)) newPilha (cartoes newPilha)
    else do
      putStrLn "Você errou"
      studyPilhaMenu (searchSessao (dataEstudo sessao))

  addFase :: Cartao -> Pilha -> Pilha
  addFase cartao pilha = do
    if (fase cartao) == 4 then do
      let newPilha = removerCartao pilha cartao
      return editPilhaAndSave (nome newPilha) (cartoes newPilha)
    else do
      let newCard = Cartao (dataCriacao cartao) (dataVencimento cartao) ((fase cartao) + 1) (frente cartao) (verso cartao)
      let newPilha = editarCartao pilha cartao (frente cartao) (verso cartao)
      return editPilhaAndSave (nome newPilha) (cartoes newPilha)
    


    
  errorMenu:: Sessao -> IO()
  errorMenu sessao = do
    putStrLn "################# Opção inválida! #################\n"
    mainMenu sessao

  date :: IO (Integer, Int, Int) -- :: (year, month, day)
  date = getCurrentTime >>= return . toGregorian . utctDay

  day :: IO Int
  day = (\(_, _, d) -> d) <$> date    