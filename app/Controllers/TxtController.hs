module Controllers.TxtController where
  import System.IO
  import System.Directory
  import Models.Pilha
  import Data.List (delete)
  import Models.Sessao
  import Models.Intervalo (Intervalo)
  
  
  
  loadDB :: IO [Pilha]
  loadDB = do
    file <- readFile "../database/Pilhas.txt"
    let lista = (read file :: [Pilha])
    seq (length lista) (return ())
    return lista
    
  
  printDB :: IO ()
  printDB = do
    file <- readFile "../database/Pilhas.txt"
    let lista = (read file :: [Pilha])
    seq (length lista) (return ())
    print lista

 
  writeDB :: [Pilha] -> IO ()
  writeDB pilha = do
    writeFile "../database/Pilhas.txt" (show pilha)

  loadSessoesDB :: IO [Sessao]
  loadSessoesDB = do
    file <- readFile "../database/Sessoes.txt"
    let lista = (read file :: [Sessao])
    seq (length lista) (return ())
    return lista

  writeSessoesDB :: [Sessao] -> IO ()
  writeSessoesDB sessao = do
    writeFile "../database/Sessoes.txt" (show sessao)

  writeIntervalosDB :: Intervalo -> IO ()
  writeIntervalosDB intervalos = do
    writeFile "../database/Intervalos.txt" (show intervalos)

  loadIntervalosDB :: IO Intervalo
  loadIntervalosDB = do
    file <- readFile "../database/Intervalos.txt"
    let intervalos = (read file :: Intervalo)
    return intervalos