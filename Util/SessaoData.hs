module Util.SessaoData where
import Models.Sessao

carregarArquivo :: IO [Sessao]
carregarArquivo = do
    arquivo <- readFile "./Data/Sessoes.txt"
    let lista = (read arquivo :: [Sessao])
    return lista

escreverArquivo :: [Sessao] -> IO()
escreverArquivo sessao = writeFile "./Data/Sessoes.txt" (show sessao)