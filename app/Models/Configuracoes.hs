module Models.Configuracoes where

    data Configuracoes = Configuracoes {
        fase1:: Integer,
        fase2:: Integer,
        fase3:: Integer,
        fase4:: Integer,
        fase5:: Integer
    } deriving (Show, Read, Eq)