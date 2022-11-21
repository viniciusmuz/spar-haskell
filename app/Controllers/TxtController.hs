module Controllers.TxtController where
  import System.IO
  import System.Directory
  import Models.Pilha
  import Data.List (delete)

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
