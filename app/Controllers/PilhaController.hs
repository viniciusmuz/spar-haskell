{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Controllers.PilhaController where
  import Controllers.TxtController
  import Data.List (elemIndex, permutations)
  import Data.Maybe (fromMaybe)
  import Models.Pilha
  import Models.Cartao


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

  class CanEditPilhaNameAndSave v1 v2 where
    editPilhaAndSave :: v1 -> v2 -> IO [Pilha]
  instance CanEditPilhaNameAndSave String String where
    editPilhaAndSave pilhaName newPilhaName = do
      pilha <- editPilha pilhaName newPilhaName
      writeDB pilha
      return pilha
  instance CanEditPilhaNameAndSave String [Cartao] where
    editPilhaAndSave pilhaName newCards = do
      pilha <- editPilha pilhaName newCards
      writeDB pilha
      return pilha

  
  class CanEditPilhaName v1 v2 where
    editPilha :: v1 -> v2 -> IO [Pilha]
  instance CanEditPilhaName String String where
    editPilha pilhaName newPilhaName = do
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
    editPilha pilhaName newCards = do
      db <- loadDB
      let dbAsNames = map nome db
      let idx = fromMaybe (-1) (elemIndex pilhaName dbAsNames)
      if idx == -1 then (do
        putStrLn "Couldn't find the Pilha"
        return db) else (do
        let oldElm = db!!idx
        let newElm = Pilha { nome=nome oldElm, cartoes=newCards }
        let (s, _:end) = splitAt idx db
        let newDb = s ++ newElm : end
        return newDb)