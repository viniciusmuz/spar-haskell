module Controllers.SessaoController where
    import Controllers.TxtController
    import Data.List (elemIndex, permutations)
    import Data.Maybe (fromMaybe)
    import Models.Sessao
    import Models.Cartao
    import Data.Time
--import System.Random (getStdGen, randomRIO)

    class CanSearch v where
    -- |Searches a Deck in the database, by name.
        searchSessao :: v -> IO Sessao
    instance CanSearch Day where
        searchSessao dayToSearch = do
            db <- loadDBSessoes
            let m = filter (dayToSearch >-=) db
            return (if null m then Sessao { dataEstudo=dayToSearch, cartoesEstudados=[] } else head m)

    procuraSessao :: [Day] -> Day -> Int
    procuraSessao [] n = -1
    procuraSessao (a:xs) n = do
        if a == n then 0
        else 1 + procuraSessao xs n


    class CanEditSessaoAndSave v1 v2 where
        editSessaoAndSave :: v1 -> v2 -> IO [Sessao]
    instance CanEditSessaoAndSave Day [Cartao] where
        editSessaoAndSave sessaoDay newCards = do
            sessao <- editaSessao sessaoDay newCards
            writeDBSessao sessao
            return sessao

    
    class CanEditSessao v1 v2 where
        editaSessao :: v1 -> v2 -> IO [Sessao]
    instance CanEditSessao Day [Cartao] where
        editaSessao sessaoDay newCards = do
            dbSessao <- loadDBSessao sessaoDay
            let dbAsDay = map dataEstudo dbSessao
            if ((procuraSessao dbAsDay sessaoDay) == -1) then do
                putStrLn "Sessão não encontrada"
                return dbSessao
            else do
                let idx = procuraSessao dbAsDay sessaoDay
                let oldElm = dbSessao!!idx
                let newElm = Sessao { dataEstudo= dataEstudo oldElm, cartoesEstudados=newCards }
                let (s, _:end) = splitAt idx dbSessao
                let newDb = s ++ newElm : end
                return newDb

    (>-=) :: Day -> Sessao -> Bool
    (>-=) sDay sessao = sDay == dataEstudo sessao