module Models.Pilha where
import qualified Models.Cartao as Cartao
import Data.List
import Data.Time
import System.Random (newStdGen, randomRIO, getStdRandom, Random (randomR))
import Data.Function
import Data.Functor
import Control.Monad (replicateM)

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
    let editedCartao = Cartao.Cartao (Cartao.dataCriacao cartao) (Cartao.dataVencimento cartao) (Cartao.fase cartao) frente verso
    let editedPilha = removerCartao pilha cartao
    Pilha (nome editedPilha) (cartoes editedPilha ++ [editedCartao])

atualizarCartao:: Pilha -> Cartao.Cartao -> Cartao.Cartao -> Pilha
atualizarCartao pilha cartaoAntigo cartaoNovo = do
    let removido = removerCartao pilha cartaoAntigo
    adicionarCartao pilha cartaoNovo

cartoesVencidos:: Pilha -> Day -> [Cartao.Cartao] -> [Cartao.Cartao]
cartoesVencidos pilha dia vencidos
    | null (cartoes pilha) = vencidos
    | otherwise = do
        let cartao = head (cartoes pilha)
        let pilhaSemCartao = removerCartao pilha cartao
        if dia >= Cartao.dataVencimento cartao
            then cartoesVencidos pilhaSemCartao dia (vencidos ++ [cartao])
            else cartoesVencidos pilhaSemCartao dia vencidos

ordemAleatoria:: Pilha -> IO [Cartao.Cartao]
ordemAleatoria pilhaOriginal = do
    let cartoesOriginal = (cartoes pilhaOriginal)
    cartoesRandomizados <- shuffle cartoesOriginal
    return cartoesRandomizados


shuffle:: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))