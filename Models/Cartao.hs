module Models.Cartao where
    import Data.Time

    data Cartao = Cartao {
        dataCriacao:: Day,
        fase:: Int,
        frente:: String,
        verso:: String
    } deriving (Show, Read, Eq)

    erro:: Cartao -> Cartao
    erro cartao =
        Cartao (dataCriacao cartao) 0 (frente cartao) (verso cartao)

    dificil:: Cartao -> Cartao
    dificil cartao
        | fase cartao == 0 = medio cartao
        | otherwise = Cartao (dataCriacao cartao) (fase cartao - 1) (frente cartao) (verso cartao)

    medio:: Cartao -> Cartao
    medio cartao =
        Cartao (dataCriacao cartao) (fase cartao) (frente cartao) (verso cartao)
    
    acerto:: Cartao -> Cartao
    acerto cartao
        | fase cartao == 5 = medio cartao
        | otherwise = Cartao (dataCriacao cartao) (fase cartao + 1) (frente cartao) (verso cartao)