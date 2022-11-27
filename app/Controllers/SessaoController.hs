{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Controllers.SessaoControler where
  import Controllers.TxtController ( loadSessoesDB, writeSessoesDB )
  import Data.List (elemIndex, permutations)
  import Data.Maybe (fromMaybe)
  import Models.Pilha
  import Models.Cartao
  import qualified Models.Pilha as Pilha
  import Data.Time
  import Models.Sessao
  import Data.Time (diffUTCTime)
  
  finalizarSessao :: UTCTime -> Integer -> IO()
  finalizarSessao inicio cartoesEstudados = do
    now <- getCurrentTime
    let duracao = diffUTCTime now inicio
    let novaSessao = Sessao (utctDay inicio) duracao cartoesEstudados
    sessoes <- loadSessoesDB
    writeSessoesDB (novaSessao : sessoes)