module Main where
  import Controllers.TxtController
  import Controllers.InterfaceController
  import Controllers.IntervaloController
  import Controllers.PilhaController
  import Controllers.SessaoController
  import Models.Pilha
  import Models.Cartao
  import Models.Sessao
  import qualified Models.Intervalo as Intervalo
  import Data.Char
  import Data.Time
  import Data.Time.Clock
  import Data.Time.Calendar
  import Data.Time.Calendar.OrdinalDate
  import qualified Models.Pilha as Pilha
  
  welcome :: String
  welcome = "*** Bem-vindo ao Spar! ***" ++ "\n" ++ 
          "Digite a letra correspondente à ação que você deseja executar\n" ++
          "[E]studar\n" ++
          "[C]riar Pilha\n"++
          "[G]erenciar Pilha\n"++
          "[V]isualizar sessões de estudo anteriores\n"++
          "[A]lterar intervalos\n"++
          putLine

  main :: IO()  
  main = do
    putStrLn welcome
    
    menu <- menuPilhas
    putStrLn menu

    input <- getLine
    menuOptions (map toUpper input)

  menuOptions:: String -> IO ()
  menuOptions option |option == "E" = studyPilhaMenu 
                     |option == "C" = createPilha 
                     |option == "G" = choosePilhaMenu 
                     |option == "V" = stats
                     |option == "A" = alterarIntervalosMenu
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
  studyPilhaMenu = do
    putStrLn "> Escolha o número da pilha que deseja estudar: "
    numPilha <- readLn
    db <- loadDB
    putStrLn ""

    case (numPilha > 0 && numPilha <= length db) of
      True -> do
        putStrLn putLine
        let pilha = db!!(numPilha-1)
        putStrLn $ "<<  " ++ (nome pilha) ++ "  >>\n"
        inicioSessao <- getCurrentTime
        dia <- getCurrentTime
        cartoesAleatorios <- Pilha.shuffle (filtraCartoesDia (dia) (cartoes pilha))
        studyCartoes pilha cartoesAleatorios 0 inicioSessao
      False -> do
        putStrLn "\n# Número da pilha inválido #\n"
        choosePilhaMenu

  filtraCartoesDia:: UTCTime -> [Cartao] -> [Cartao]
  filtraCartoesDia dia [] = []
  filtraCartoesDia dia (h:t) = do
    let diferenca = diffDays (utctDay dia) (dataVencimento h)
    if(diferenca >= 0) then filtraCartoesDia dia t ++ [h]
    else filtraCartoesDia dia t  

  studyCartoes :: Pilha -> [Cartao] -> Integer -> UTCTime -> IO()
  studyCartoes pilha cards quantidade inicio
          | length cards == 0 = do 
            finalizarSessao inicio (quantidade)
            mainMenu
          | otherwise = do
            let cartao = (head cards)
            intervalos <- getIntervalos
            putStrLn (frente cartao)
            putStrLn "***\nPressione Enter para ver o verso do cartão\n***"
            discard <- getLine
            putStrLn (verso cartao)
            putStrLn "***\n[A]certou\n[E]rrou\n[X]Para o estudo ***"
            feedback <- getLine
            dataHoje <- getCurrentTime
            if (map toUpper feedback) == "A" then do
              let numeroDias = getIntervalo intervalos ((fase cartao) + 1)
              let newVencimento = addDays (numeroDias) (utctDay dataHoje)
              let newCard = Cartao (dataCriacao cartao) (newVencimento) (fase cartao + 1) (frente cartao) (verso cartao)
              if(fase newCard > 5) then do
                let newPilha2 = removerCartao pilha cartao
                editPilhaAndSave (nome newPilha2) (cartoes newPilha2)
              else do
                let newPilha2 = removerCartao pilha cartao
                let newPilha = adicionarCartao newPilha2 newCard
                editPilhaAndSave (nome pilha) (cartoes newPilha)
              putStrLn putLine
              studyCartoes pilha (tail cards) (quantidade + 1) inicio 
            else if (map toUpper feedback) == "E" then do
              if(fase cartao /= 1) then do
                let numeroDias = getIntervalo intervalos ((fase cartao) - 1)
                let newVencimento = addDays numeroDias (utctDay dataHoje)
                let newCard = Cartao (dataCriacao cartao) (newVencimento) (fase cartao - 1) (frente cartao) (verso cartao)
                let newPilha2 = removerCartao pilha cartao
                let newPilha = adicionarCartao newPilha2 newCard
                editPilhaAndSave (nome pilha) (cartoes newPilha)
                putStrLn putLine
              else do
                putStrLn putLine  
              studyCartoes pilha (tail cards) (quantidade + 1) inicio
            else do
              finalizarSessao inicio (quantidade + 1)
              mainMenu
            
  
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
        printPilha (cartoes pilha) 1 ""
        putStrLn "[A] Add carta [E] Editar Carta  [R] Remover Pilha             [X] Voltar\n"
        option <- getLine
        putStrLn ""
        operationsPilha pilha (map toUpper option)
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

  addCardPilha:: Pilha -> IO ()
  addCardPilha pilha = do
    putStrLn putLine
    putStrLn "> Qual será a frente da carta?"
    front <- getLine

    putStrLn "\n> Qual será o verso da carta?"
    back <- getLine

    hoje <- utctDay <$> getCurrentTime
    
    let newCard = Cartao hoje hoje 1 front back
    let editedPilha = adicionarCartao pilha newCard
    editPilhaAndSave (nome editedPilha) (cartoes editedPilha) 
    putStrLn "\nCarta adicionada com sucesso!\n"
    mainMenu

  editCardPilha:: Pilha -> [Cartao] -> IO()
  editCardPilha pilha cards = do
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
  
  chooseCardMenu:: Pilha -> [Cartao]-> IO()
  chooseCardMenu pilha cards = do
    pilhaSearch <- search (nome pilha)
    case (length (cartoes pilhaSearch)) == 0 of
      True -> do
        putStrLn putLine
        putStrLn "              Essa não possui cartões:(           \n"
        mainMenu
      False -> do  
        putStrLn "Chegou"

  stats::IO()
  stats = do
    sessoes <- loadSessoesDB
    case length sessoes == 0 of
      True -> do
        putStrLn "Você ainda não realizou nenhuma sessão de estudo :( \n\n "
        mainMenu
      False -> do
        putStrLn "Sessões: \n"
        putStrLn (printSessoes sessoes)
        putStrLn putLine
        mainMenu

  printSessoes:: [Sessao] -> String
  printSessoes [] = ""
  printSessoes (h:t) = "Data de estudo: " ++ show (dataEstudo h) ++ "\nDuracao: " ++ show (duracao h) ++ "\nCartoes Estudados: " ++ show (cartoesEstudados h) ++ "\n\n" ++ printSessoes t

  alterarIntervalosMenu:: IO ()
  alterarIntervalosMenu = do
    putStrLn putLine

    putStrLn "\n> Quantos dias depois você quer ver uma carta acertada nenhuma vez? "
    fase1 <- getLine
    let f1 = (read fase1 :: Integer)

    putStrLn "\n> Quantos dias depois você quer ver uma carta acertada uma vez? "
    fase2 <- getLine
    let f2 = (read fase2 :: Integer)

    putStrLn "\n> Quantos dias depois você quer ver uma carta acertada duas vezes? "
    fase3 <- getLine
    let f3 = (read fase3 :: Integer)

    putStrLn "\n> Quantos dias depois você quer ver uma carta acertada três vezes? "
    fase4 <- getLine
    let f4 = (read fase4 :: Integer)

    putStrLn "\n> Quantos dias depois você quer ver uma carta acertada quatro vezes? "
    fase5 <- getLine
    let f5 = (read fase5 :: Integer)

    let novosIntervalos = Intervalo.Intervalo f1 f2 f3 f4 f5
    setIntervalos novosIntervalos
    putStrLn "\nIntervalos salvos com sucesso!\n"
    mainMenu
    
  errorMenu:: IO()
  errorMenu = do
    putStrLn "################# Opção inválida! #################\n"
    mainMenu

  printPilha:: [Cartao] -> Int -> String -> IO()
  printPilha cartoes posicao str
      | length cartoes == 0 = putStrLn str
      | otherwise = do
        printPilha (tail cartoes) (posicao + 1) (str ++ show (posicao) ++ " - " ++ frente (head cartoes) ++ "\n")