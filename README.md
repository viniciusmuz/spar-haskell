# spar-haskell
O Spar é uma ferramenta que visa ajudar estudantes a adotar a prática da repetição espaçada.

# Introdução
  Uma das maneiras mais eficazes de aprender um assunto novo é através da repetição. Ao rever o conteúdo repetidamente em intervalos variados de tempo, conseguimos guardar conteúdo na nossa memória de longo prazo e criar conexões cada vez mais fortes entre os conceitos. Adotar essa prática também possibilita eliminar o cramming, que consiste em tentar absorver (normalmente sem sucesso) grandes quantidades de informação em pequenos períodos. 

O Spar é uma ferramenta que visa ajudar estudantes a adotar a prática da repetição espaçada. Sua funcionalidade gira em torno de cartões, que possuem frente e verso contendo, respectivamente, perguntas e respostas. O estudante vê a frente do cartão, responde mentalmente e confere o verso do cartão para verificar se acertou. O Spar coletará feedback do estudante (se ele errou, foi fácil, difícil, mediano) e irá, com base nessa informação, distribuir os cartões estudados ao longo dos dias. Todo dia, ao abrir o Spar, o estudante terá acesso ao que deve estudar.

# Funcionalidades

**Gerenciamento dos cartões (persistidos em disco)**
* Adicionar.
* Remover.
* Fazer alterações.
* Organizar os cartões em pilhas.
* Customizar os intervalos.
* Criar rodadas de preparação para prova, com cartões mesclados.

**Momento de estudo**

Iniciar uma sessão de estudos que exibirá um cartão por vez, de forma aleatória.
* Os cartões serão embaralhados a cada sessão.
* O sistema armazena a data que o programa foi executado pela ultima vez e utiliza isso para fazer os cálculos dos intervalos.

Dar a possibilidade de o estudante marcar a dificuldade de cada cartão.
* Se ele errar, deverá estudar novamente na mesma sessão (ex.: 21 dias -> sessão atual -> 1 dia).
* Se ele achar fácil, o intervalo para estudar o cartão novamente será aumentado (ex.: 1 dia -> 3 dias -> 7 dias -> 14 dias -> 21 dias).
* Se ele achar difícil, o intervalo será diminuído para um nível anterior (ex.: 14 dias -> 7 dias). 
* Se ele achar mediano, o mesmo intervalo será mantido. 

**Estatísticas**

* Ver quanto tempo as sessões de estudo estão durando.
* Ver quantos cartões serão estudados nos próximos dias.
