module Controllers.InterfaceController where
    import Controllers.TxtController
    import Controllers.PilhaController
    import Models.Pilha
    import Models.Cartao
    import Data.List (group)
    
    putLine:: String
    putLine = "───────────────────────────────────────────────────\n"

    initialMenu:: String
    initialMenu = "Digite a letra correspondente à ação que você deseja executar\n" ++
                "[C]Criar Pilha\n"++
                "[R]Remover Pilha\n"++
                "[D]Editar Pilha\n" ++
                "[E]Estudar\n" ++
                putLine

    menuPilhas:: IO String
    menuPilhas = do
        namesPilhas <- getPilhaNames
        case length namesPilhas == 0 of
            True -> do
                return "Você não possui pilhas"
            False -> do
                let numberedPilhas = zipWith (\n line -> show n ++ " - " ++ line) [1..] namesPilhas
                let list = ("Suas pilhas:\n" : numberedPilhas)
                let menuString = unlines list
                return menuString            