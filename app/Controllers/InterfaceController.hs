module Controllers.InterfaceController where
    putLine:: String
    putLine = "───────────────────────────────────────────────────\n"

    initialMenu:: String
    initialMenu = "Digite a letra correspondente à ação que você deseja executar\n" ++
                "[C]Criar Pilha\n"++
                "[R]Remover Pilha\n"++
                "[D]Editar Pilha\n" ++
                "[E]Estudar\n" ++
                putLine