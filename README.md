# Jogo da cobrinha

## Como rodar
Para compilar o jogo é utilizada a ferramenta [stack](https://docs.haskellstack.org/en/stable/README/), que pode ser instalada a partir do [GHCup](https://www.haskell.org/ghcup/). Com a stack instalada, basta compilar e executar. Pode ser necessário instalar as bibliotecas do OpenGL, dependendo do seu sistema

## Como jogar

- WASD: teclas utilizadas para mudar a direção da cobrinha
- Espaço: tecla utilizada para acelerar a cobrinha
- R: tecla utilizada para reiniciar o jogo na tela de game over
- Esc: tecla utilizada para sair do jogo

## Sobre o Projeto
A proposta deste projeto foi estabelecida na disciplina de Paradigmas de Programação do curso de Ciência da Computação da Universidade Federal de Santa Maria (UFSM). O objetivo era explorar um pouco mais os recursos da linguagem de programação funcional Haskell.

### Tema do projeto
Paradigmas funcionais tem como uma de suas principais características o foco em funções, semelhantes às da matemática, cujo o comportamento depende apenas dos seus parâmetros, ou seja, não possuem um estado interno. Por isso, foi escolhido um tipo de aplicação em que a mudança de estado deve acontecer a todo o momento e em quase todas as funções: jogos. Para manter a "funcionalidade" do programa, foi evitado ao máximo o uso de IO(). Ao invés disso, o estado a ser modificado é passado por parametro e retornado pelas funções que o modificam.

### Tema do jogo

O tema do jogo surgiu a partir da imagem abaixo, utilizada para auxiliar na explicação da estrutura das listas em haskell. O corpo segmentado do "mostro lista" lembra um pouco a cobrinha do jogo, daí a ideia.

![](http://s3.amazonaws.com/lyah/listmonster.png)

### Gráficos

Para a parte gráfica, foi utilizada a biblioteca [gloss](https://hackage.haskell.org/package/gloss), que funciona como uma interface simplificada para o OpenGL. Ela conta tanto com funções IO() quanto funções puras, mas para este projeto foram utilizadas apenas as funções puras
