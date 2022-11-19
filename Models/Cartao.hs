module Models.Cartao where
    import Data.Time

    data Cartao = Cartao {
        dataCriacao:: Day,
        dataVencimento:: Day,
        frente:: String,
        verso:: String
    } deriving (Show, Read, Eq)