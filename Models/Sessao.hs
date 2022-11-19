module Models.Sessao where
    import Data.Time

    data Sessao = Sessao {
        dataEstudo:: Day,
        duracao:: TimeOfDay,
        cartoesEstudados:: Integer
    } deriving (Show, Read, Eq)