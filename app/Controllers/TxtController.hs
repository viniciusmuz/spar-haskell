module Controllers.TxtController where
  import System.IO
  import System.Directory
  import Models.Pilha
  import Data.List (delete)
  import Data.Time
  import Models.Sessao

  -- |Loads Pilhas database into memory, and returns it as a list ([Pilha])
  loadDB :: IO [Pilha]
  loadDB = do
    file <- readFile "../database/Pilhas.txt"
    let lista = (read file :: [Pilha])
    seq (length lista) (return ())
    return lista
    
  -- |Prints Pilhas database into stdin
  printDB :: IO ()
  printDB = do
    file <- readFile "../database/Pilhas.txt"
    let lista = (read file :: [Pilha])
    seq (length lista) (return ())
    print lista

  -- |Writes given deck list into Pilhas database.
  writeDB :: [Pilha] -> IO ()
  writeDB pilha = do
    writeFile "../database/Pilhas.txt" (show pilha)


  -- |Loads Pilhas database into memory, and returns it as a list ([Pilha])
  loadDBSessoes :: IO [Sessao]
  loadDBSessoes = do
    file <- readFile "../database/Sessoes.txt"
    let lista = (read file :: [Sessao])
    seq (length lista) (return ())
    return lista
    
  loadDBSessao :: Day -> IO [Sessao]
  loadDBSessao date = do
    file <- readFile "../database/Sessoes.txt"
    let lista = (read file :: [Sessao])
    seq (length lista) (return ())
    return (getSessao date lista)
    
  getSessao :: Day -> [Sessao] -> [Sessao]
  getSessao dia [] = []
  getSessao dia (a:xs) = do
    if dataEstudo a == dia then [a]
    else getSessao dia xs

  -- |Prints Pilhas database into stdin
  printDBSessao :: IO ()
  printDBSessao = do
    file <- readFile "../database/Sessoes.txt"
    let lista = (read file :: [Sessao])
    seq (length lista) (return ())
    print lista

  -- |Writes given deck list into Pilhas database.
  writeDBSessao :: [Sessao] -> IO ()
  writeDBSessao sessao = do
    writeFile "../database/Sessoes.txt" (show sessao)