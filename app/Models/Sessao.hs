module Models.Sessao where
    import Data.Time
    import Models.Cartao

    data Sessao = Sessao {
        dataEstudo:: Day,
<<<<<<< HEAD
        --duracao:: TimeOfDay,
        cartoesEstudados:: [Cartao]
    } deriving (Show, Read, Eq)

    adicionaCartaoEstudado :: Sessao -> Cartao -> Sessao
    adicionaCartaoEstudado sessaoOld cartao = Sessao (dataEstudo sessaoOld) (cartoesEstudados sessaoOld ++ [cartao])
=======
        duracao:: NominalDiffTime,
        cartoesEstudados:: Integer
    } deriving (Show, Read, Eq)
>>>>>>> d37333645904b29fb1b70a0c33e7a8fdbed77bc2
