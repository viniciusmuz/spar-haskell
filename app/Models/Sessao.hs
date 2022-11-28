module Models.Sessao where
    import Data.Time
    import Models.Cartao

    data Sessao = Sessao {
        dataEstudo:: Day,
        duracao:: NominalDiffTime,
        cartoesEstudados:: Integer
    } deriving (Show, Read, Eq)
