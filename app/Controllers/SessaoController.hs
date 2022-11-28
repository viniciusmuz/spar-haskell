{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Controllers.SessaoController where
  import Controllers.TxtController ( loadSessoesDB, writeSessoesDB )
  import Data.List (elemIndex, permutations)
  import Data.Maybe (fromMaybe)
  import Models.Pilha
  import Models.Cartao
  import qualified Models.Pilha as Pilha
  import Data.Time
  import Models.Sessao
  import Data.Time (diffUTCTime)
  
  add :: Sessao -> IO [Sessao]
  add sessao = do
    db <- loadSessoesDB
    let addedList = db ++ [sessao]
    return addedList

  finalizarSessao :: UTCTime -> Integer -> IO()
  finalizarSessao inicio cartoesEstudados = do
    now <- getCurrentTime
    let duracao = diffUTCTime now inicio
    let novaSessao = Sessao (utctDay inicio) duracao cartoesEstudados
    addedList <- add novaSessao
    writeSessoesDB (addedList)
    