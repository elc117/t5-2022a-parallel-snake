# Jogo da cobrinha em paralelo

## Como rodar
Para compilar o jogo é utilizada a ferramenta [stack](https://docs.haskellstack.org/en/stable/README/), que pode ser instalada a partir do [GHCup](https://www.haskell.org/ghcup/). Com a stack instalada, basta compilar e executar. Pode ser necessário instalar as bibliotecas do OpenGL, dependendo do seu sistema

## Como jogar

- WASD: teclas utilizadas para mudar a direção das cobrinhas
- Espaço: tecla utilizada para acelerar as cobrinhas
- R: tecla utilizada para reiniciar o jogo na tela de game over
- Esc: tecla utilizada para sair do jogo

## Sobre o Projeto
A proposta deste projeto foi estabelecida na disciplina de Paradigmas de Programação do curso de Ciência da Computação da Universidade Federal de Santa Maria (UFSM). O objetivo era explorar um pouco mais os conceitos de programação concorrente.

### Tema do projeto
Este projeto é uma extensão do primeiro trabalho personalizado: [O jogo da cobrinha em Haskell](https://github.com/Jubarte27/snake-haskell). Como uma das opções para o novo projeto era explorar o paralelismo, foi escolhido realizar uma versão do jogo da cobrinha em que múltiplas cobrinhas são controladas ao mesmo tempo.

### Resultados e implementação
Por ser uma linguagem funcional, Haskell é excelente para se trabalhar com paralelismo. Como as funções não possuem efeitos secundários, é muito difícil ocorrerem problemas de concorrência de dados. No caso do jogo da cobrinha em paralelo, se preocupar com concorrência não foi necessário.

Cada cobrinha foi separada em "mundos". Desse modo, as alterações no estado dos mundos são realizadas em paralelo para cada mundo. A representação gráfica dos mundos também é calculada em paralelo. Graças as facilidades da linguagem, não foi necessário se preocupar com a criação e execução de threads. Ao invés disso, foram utilizados algumas funções que indicam ao compilador quais operações podem ser executadas em paralelo.

Por isso, não seria estranho dizer que programar em Haskell utilizando paralelismo é tão simples quanto programar sem paralelismo.
