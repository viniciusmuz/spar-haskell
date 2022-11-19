module Models.Pilha where
import qualified Models.Cartao as Cartao
import Data.List
import Data.Time

data Pilha = Pilha {
  nome:: String,
  cartoes:: [Cartao.Cartao]
} deriving (Show, Read, Eq)

adicionarCartao:: Pilha -> Cartao.Cartao -> Pilha
adicionarCartao pilha cartao = 
    Pilha (nome pilha) (cartoes pilha ++ [cartao])

removerCartao:: Pilha -> Cartao.Cartao -> Pilha
removerCartao pilha cartao =
  Pilha (nome pilha) (delete cartao (cartoes pilha))

editarCartao:: Pilha -> Cartao.Cartao -> String -> String -> Pilha
editarCartao pilha cartao frente verso = do
    let cartao = Cartao.Cartao (Cartao.dataCriacao cartao) (Cartao.fase cartao) frente verso
    let pilha = removerCartao pilha cartao
    Pilha (nome pilha) (cartoes pilha ++ [cartao])

atualizarCartao:: Pilha -> Cartao.Cartao -> Cartao.Cartao -> Pilha
atualizarCartao pilha cartaoAntigo cartaoNovo = do
    let removido = removerCartao pilha cartaoAntigo
    adicionarCartao pilha cartaoNovo