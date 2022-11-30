# Spar

# Introdução

Uma das maneiras mais eficazes de aprender um assunto novo é através da repetição. Ao [rever o conteúdo repetidamente](https://www.theguardian.com/education/2016/jan/23/spaced-repetition-a-hack-to-make-your-brain-store-information) em intervalos variados de tempo, conseguimos guardar conteúdo na nossa memória de longo prazo e criar conexões cada vez mais fortes entre os conceitos. Adotar essa prática também possibilita eliminar o [cramming](https://web.archive.org/web/20071009104548/http://www.bmb.psu.edu/courses/psu16/troyan/studyskills/cramming.htm), que consiste em tentar absorver (normalmente sem sucesso) grandes quantidades de informação em pequenos períodos.

  
O Spar é uma ferramenta que visa ajudar estudantes a adotar a prática da repetição espaçada. Sua funcionalidade gira em torno de cartões, que possuem frente e verso contendo, respectivamente, perguntas e respostas. O estudante vê a frente do cartão, responde mentalmente e confere o verso do cartão para verificar se acertou. O Spar coletará feedback do estudante (se ele acertou ou errou) e irá, com base nessa informação, distribuir os cartões estudados ao longo dos dias. Todo dia, ao abrir o Spar, o estudante terá acesso ao que deve estudar.
# Recursos

 - Estudar os cartões que vencem no dia atual.
 - Criar e editar pilhas.
 - Criar e editar cartões.
 - Definir intervalos customizados, ou seja, quantos dias depois o usuário quer estudar novamente uma carta que foi acertada 1, 2, 3, 4 ou 5 vezes.
 - Ver as sessões de estudos, com o tempo que cada sessão durou e a quantidade de cartas que foram estudadas. 

# Dependências
Para compilar o Spar através do código fonte, é preciso ter o GHC instalado. O método recomendado de instalação do GHC é usando o [GHCup](https://www.haskell.org/ghcup/). 

Após a instalação, no diretório `./app` do repositório, execute o comando `ghc spar.hs` para compilar o executável, e depois execute-o. 

Alternativamente, é possível executar a aplicação no modo interpretado através do GHCi, usando o comando `ghci spar.hs`. Certifique-se de usar uma versão recente do Haskell caso queira usar o Spar dessa forma.
# Capturas de tela
![Menu inicial](https://i.ibb.co/VtXNrYh/menu-inicial.png)

![Estudando cartões](https://i.ibb.co/f9JHrq7/estudando-cartoes.png)

![Gerenciando cartões e pilhas](https://i.ibb.co/LxB6wTB/gerenciamento.png)
![Alterar os intervalos de datas usados](https://i.ibb.co/R27frtf/alterar-intervalos.png)
