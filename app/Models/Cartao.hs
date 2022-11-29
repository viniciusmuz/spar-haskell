module Models.Cartao where
    import Data.Time

    data Cartao = Cartao {
        dataCriacao:: Day,
        dataVencimento:: Day,
        fase:: Integer,
        frente:: String,
        verso:: String
    } deriving (Show, Read, Eq)