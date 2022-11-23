module Models.Cartao where
    import Data.Time

    data Cartao = Cartao {
        dataCriacao:: Day,
        dataVencimento:: Day,
        fase:: Int,
        frente:: String,
        verso:: String
    } deriving (Show, Read, Eq)