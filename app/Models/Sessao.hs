module Models.Sessao where
    import Data.Time
    import Models.Cartao

    data Sessao = Sessao {
        dataEstudo:: Day,
        --duracao:: TimeOfDay,
        cartoesEstudados:: [Cartao]
    } deriving (Show, Read, Eq)

    adicionaCartaoEstudado :: Sessao -> Cartao -> Sessao
    adicionaCartaoEstudado sessaoOld cartao = Sessao (dataEstudo sessaoOld) (cartoesEstudados sessaoOld ++ [cartao])