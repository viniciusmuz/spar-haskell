welcome :: String
welcome = "*** Bem-vindo ao Spar! ***\n\n" ++ 
        "Digite a letra correspondente à ação que você deseja executar\n" ++
        "[E]studar\n" ++
        "[A]dicionar cartões\n" ++
        "[R]emover cartões\n" ++
        "[M]odificar cartões\n" ++
        "[C]ustomizar intervalos dos cartões"

commands :: [String]
commands = ["E", "A", "R", "M", "C"]

main :: IO()
main = do
    putStrLn welcome
    input <- getLine :: IO String
    if input `elem` commands
        then print "Você escolheu uma opção válida!"
        else print "Opção inválida"