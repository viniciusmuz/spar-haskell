{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Controllers.PilhaController where
  import Controllers.TxtController
  import Data.List (elemIndex, permutations)
  import Data.Maybe (fromMaybe)
  import Models.Pilha
  import Models.Cartao
  import qualified Models.Pilha as Pilha
  

  getPilhaNames :: IO [String]
  getPilhaNames = do
    db <- loadDB
    let nomes = [nome pilha | pilha <- db]
    return nomes

  add :: Pilha -> IO [Pilha]
  add pilha = do
    db <- loadDB
    let addedList = db ++ [pilha]
    return addedList

  procuraPilha :: [String] -> String -> Int
  procuraPilha [] n = -1
  procuraPilha (a:xs) n = do
    if a == n then 0
    else 1 + procuraPilha xs n
    

  class CanAdd v where
    addAndSave :: v -> IO [Pilha]
  instance CanAdd Pilha where
    addAndSave pilha = do
      addedList <- add pilha
      writeDB addedList
      return addedList
  instance CanAdd String where
    addAndSave nomePilha = do
      addedList <- add Pilha { nome=nomePilha, cartoes=[] }
      writeDB addedList
      return addedList

  class CanRemove v where
    remove :: v -> IO [Pilha]
  instance CanRemove String where
    remove nameToSearch = do
      db <- loadDB
      pilha <- search nameToSearch
      let pilhas = filter (\deckToCompare -> not (pilha >== deckToCompare)) db
      return pilhas
  instance CanRemove Pilha where
    remove pilha = do
      db <- loadDB
      let pilhas = filter (\deckToCompare -> not (pilha >== deckToCompare)) db
      return pilhas

  class CanRemoveAndSave v where
    removeAndSave :: v -> IO [Pilha]
  instance CanRemoveAndSave String where
    removeAndSave nameToSearch = do
      pilhas <- remove nameToSearch
      writeDB pilhas
      return pilhas
  instance CanRemoveAndSave Pilha where
    removeAndSave deck = do
      pilhas <- remove deck
      writeDB pilhas
      return pilhas


  class CanSearch v where
    search :: v -> IO Pilha
  instance CanSearch String where
    search nameToSearch = do
      db <- loadDB
      let m = filter (nameToSearch >-=) db
      return (if null m then Pilha { nome="NIL", cartoes=[] } else head m)

  class CanEditPilhaNameAndSave v1 v2 where
    editPilhaAndSave :: v1 -> v2 -> IO [Pilha]
  instance CanEditPilhaNameAndSave String String where
    editPilhaAndSave pilhaName newPilhaName = do
      pilha <- editaPilha pilhaName newPilhaName
      writeDB pilha
      return pilha
  instance CanEditPilhaNameAndSave String [Cartao] where
    editPilhaAndSave pilhaName newCards = do
      pilha <- editaPilha pilhaName newCards
      writeDB pilha
      return pilha

  
  class CanEditPilhaName v1 v2 where
    editaPilha :: v1 -> v2 -> IO [Pilha]
  instance CanEditPilhaName String String where
    editaPilha pilhaName newPilhaName = do
      db <- loadDB
      let dbAsNames = map nome db
      let idx = fromMaybe (-1) (elemIndex pilhaName dbAsNames)
      if idx == -1 then (do
        print "Index doesn't exists"
        return db) else (do
        let oldElm = db!!idx
        let newElm = Pilha { nome=newPilhaName, cartoes =cartoes oldElm }
        let (s, _:end) = splitAt idx db
        let newDb = s ++ newElm : end
        return newDb)


  instance CanEditPilhaName String [Cartao] where
    editaPilha pilhaName newCards = do
      db <- loadDB
      let dbAsNames = map nome db
      if procuraPilha dbAsNames pilhaName == -1 then do
        putStrLn "Couldn't find the Pilha"
        return db
      else do
        let idx = procuraPilha dbAsNames pilhaName
        let oldElm = db!!idx
        let newElm = Pilha { nome=nome oldElm, cartoes=newCards }
        let (s, _:end) = splitAt idx db
        let newDb = s ++ newElm : end
        return newDb
   
  (>-=) :: String -> Pilha -> Bool
  (>-=) cName pilha = cName == nome pilha
  (>==) :: Pilha -> Pilha -> Bool
  (>==) deck1 deck2 = deck1 == deck2
