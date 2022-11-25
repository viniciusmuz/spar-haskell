module Main where
  import Controllers.TxtController
  import Controllers.InterfaceController
  import Controllers.PilhaController
  import Controllers.SessaoController
  import qualified Models.Pilha as Pilha
  import qualified Models.Cartao as Cartao
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
  

  operationsPilha:: Sessao -> Pilha.Pilha -> String -> IO()
  operationsPilha sessao pilha input |input == "A" = addCardPilha sessao pilha
                                      |input == "E" = editCardPilha sessao pilha (Pilha.cartoes pilha)
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
        putStrLn $ "<<  " ++ (Pilha.nome pilha) ++ "  >>\n"
        chooseCardMenu sessao pilha
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
        putStrLn $ "<<  " ++ (Pilha.nome pilha) ++ "  >>\n"
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

  
  removePilha:: Sessao -> Pilha.Pilha -> IO ()
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

  addCardPilha:: Sessao -> Pilha.Pilha -> IO ()
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
    
    let newCard = Cartao.Cartao criacaoDay vencimentoDay 0 front back
    let editedPilha = Pilha.adicionarCartao pilha newCard
    editPilhaAndSave (Pilha.nome editedPilha) (Pilha.cartoes editedPilha) 
    putStrLn "\nCarta adicionada com sucesso!\n"
    mainMenu sessao

  editCardPilha:: Sessao -> Pilha.Pilha -> [Cartao.Cartao] -> IO()
  editCardPilha sessao pilha cards = do
    putStrLn putLine
    print(show (Pilha.nome pilha))
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
        
        let editedPilha = Pilha.editarCartao pilha cartao novaFrente novoVerso
        print(show(Pilha.nome editedPilha))
        editPilhaAndSave (Pilha.nome editedPilha) (Pilha.cartoes editedPilha)
        putStrLn "\nCarta editada com sucesso!\n" 
        mainMenu sessao
      False -> do 
        putStrLn "\n# Número do Cartão é inválido #\n"
        editCardPilha sessao pilha cards
  
  chooseCardMenu:: Sessao -> Pilha.Pilha -> IO()
  chooseCardMenu sessao pilha = do
    pilhaSearch <- search (Pilha.nome pilha)
    case (length (Pilha.cartoes pilhaSearch)) == 0 of
      True -> do
        putStrLn putLine
        putStrLn "              Essa pilha não possui cartões:(           \n"
        mainMenu sessao
      False -> do  
        studyCards sessao pilha

  getFase:: [Cartao.Cartao] -> Int -> [Cartao.Cartao]
  getFase [] fase = []
  getFase (a:xs) fase = do
    if (fase ) == 0 then [a] ++ getFase xs fase
    else getFase xs fase

  studyCards:: Sessao -> Pilha.Pilha -> IO()
  studyCards sessao pilha = do
    db <- shufflePilhaAndSave pilha
    pilhaEmbaralhada <- search (Pilha.nome pilha)
    let headCartao = head (Pilha.cartoes pilhaEmbaralhada)
    putStrLn "Aperte enter para ver a frente do cartão: "
    getLine
    print (Cartao.frente headCartao)
    putStrLn putLine
    putStrLn "Aperte enter para ver o verso do cartão: "
    getLine
    print (Cartao.verso headCartao)
    putStrLn putLine
    putStrLn "[A]certei, continuar o estudo!\n[E]rrei! :(, continuar estudo!\n[X] Voltar ao menu "
    input <- getLine
    if (map toUpper input) == "A" then do
      let newPilha = addFase headCartao pilhaEmbaralhada
      let newPilha2 = Pilha.Pilha (Pilha.nome newPilha) (tail (Pilha.cartoes newPilha))
      editSessaoAndSave (dataEstudo sessao) ((cartoesEstudados sessao) ++ [headCartao])
      sessao <- (searchSessao (dataEstudo sessao))
      studyCards (sessao) (newPilha2)
    else if(map toUpper input) == "E" then do
      let newPilha = downFase headCartao pilhaEmbaralhada
      let newPilha2 = Pilha.Pilha (Pilha.nome newPilha) (tail (Pilha.cartoes newPilha))
      editSessaoAndSave (dataEstudo sessao) ((cartoesEstudados sessao) ++ [headCartao])
      sessao <- (searchSessao (dataEstudo sessao))
      studyCards (sessao) (newPilha2)
    else do
      mainMenu sessao
      

  addFase :: Cartao.Cartao -> Pilha.Pilha -> Pilha.Pilha
  addFase cartao pilha = do
    if (Cartao.fase cartao) == 4 then do
      let newPilha = Pilha.removerCartao pilha cartao
      let salva = editPilhaAndSave (Pilha.nome newPilha) (Pilha.cartoes newPilha)
      newPilha
    else do
      let newCard = Cartao.Cartao (Cartao.dataCriacao cartao) (Cartao.dataVencimento cartao) ((Cartao.fase cartao) + 1) (Cartao.frente cartao) (Cartao.verso cartao)
      let newPilha = Pilha.editarCartao pilha cartao (Cartao.frente cartao) (Cartao.verso cartao)
      let pilhas = editPilhaAndSave (Pilha.nome newPilha) (Pilha.cartoes newPilha)
      newPilha

  downFase:: Cartao.Cartao -> Pilha.Pilha -> Pilha.Pilha
  downFase cartao pilha = do 
    if (Cartao.fase cartao) == 0 then do
      let newCard = Cartao.Cartao (Cartao.dataCriacao cartao) (Cartao.dataVencimento cartao) ((Cartao.fase cartao)) (Cartao.frente cartao) (Cartao.verso cartao)
      let newPilha = Pilha.editarCartao pilha cartao (Cartao.frente cartao) (Cartao.verso cartao)
      let pilhas = editPilhaAndSave (Pilha.nome newPilha) (Pilha.cartoes newPilha)
      newPilha
    else do
      let newCard = Cartao.Cartao (Cartao.dataCriacao cartao) (Cartao.dataVencimento cartao) ((Cartao.fase cartao) - 1) (Cartao.frente cartao) (Cartao.verso cartao)
      let newPilha = Pilha.editarCartao pilha cartao (Cartao.frente cartao) (Cartao.verso cartao)
      let pilhas = editPilhaAndSave (Pilha.nome newPilha) (Pilha.cartoes newPilha)
      newPilha   
  errorMenu:: Sessao -> IO()
  errorMenu sessao = do
    putStrLn "################# Opção inválida! #################\n"
    mainMenu sessao

  date :: IO (Integer, Int, Int) -- :: (year, month, day)
  date = getCurrentTime >>= return . toGregorian . utctDay

  day :: IO Int
  day = (\(_, _, d) -> d) <$> date    