module Util.PilhaData where
import Models.Pilha

carregarArquivo :: IO [Pilha]
carregarArquivo = do
    arquivo <- readFile "./Data/Pilhas.txt"
    let lista = (read arquivo :: [Pilha])
    return lista

escreverArquivo :: [Pilha] -> IO()
escreverArquivo pilha = writeFile "./Data/Pilhas.txt" (show pilha)