{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Controllers.IntervaloController where
  import Controllers.TxtController ( loadIntervalosDB, writeIntervalosDB )
  import Data.List (elemIndex, permutations)
  import Data.Maybe (fromMaybe)
  import Models.Pilha
  import Models.Cartao
  import qualified Models.Pilha as Pilha
  import Data.Time
  import Models.Sessao
  import Data.Time (diffUTCTime)
  import Models.Intervalo
  
  setIntervalos :: Intervalo -> IO ()
  setIntervalos intervalo = do
    writeIntervalosDB intervalo

  getIntervalos :: IO Intervalo
  getIntervalos = loadIntervalosDB

  getIntervalo :: Intervalo -> Integer -> Integer
  getIntervalo intervalo fase
      | fase == 1 = f1 intervalo
      | fase == 2 = f2 intervalo
      | fase == 3 = f3 intervalo
      | fase == 4 = f4 intervalo
      | fase == 5 = f5 intervalo
      | otherwise = -1