module Models.Sessao where
    import Data.Time

    data Sessao = Sessao {
        dataEstudo:: Day,
        duracao:: NominalDiffTime,
        cartoesEstudados:: Integer
    } deriving (Show, Read, Eq)