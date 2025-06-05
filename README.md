# biblio-almas
Um protótipo de um jogo adventure.
## Documento de Design de Jogo: A Biblioteca das Almas Perdidas

## 1. Título
A Biblioteca das Almas Perdidas

## 2. Logline
Preso em uma biblioteca etérea e infinita onde cada livro é a essência de uma alma, o jogador deve desvendar segredos arcanos, interagir com histórias vivas e navegar por corredores que desafiam a lógica para encontrar um caminho de volta à realidade – ou aceitar um novo propósito entre as prateleiras do conhecimento eterno.

## 3. Gênero
Aventura em Texto (Interactive Fiction) / Puzzle / Fantasia Mística

## 4. Público-Alvo
Jogadores que apreciam narrativas imersivas, resolução de quebra-cabeças baseados em lógica e exploração textual. Fãs de obras como "As Crônicas de Nárnia" (aspecto de entrar em outros mundos), contos de Jorge Luis Borges (bibliotecas infinitas, paradoxos), ou jogos como Zork e The Librarians (série de TV).

## 5. Visão Geral da Jogabilidade (Gameplay Loop)
O jogador interage com o mundo através de comandos de texto simples (ex: `IR NORTE`, `PEGAR LIVRO`, `LER PERGAMINHO`). O ciclo principal envolve:

1.  **Exploração:** Mover-se entre diferentes seções, salas e corredores da Biblioteca.
2.  **Observação:** Examinar o ambiente, objetos, inscrições e livros para coletar pistas.
3.  **Interação:** Pegar itens, usar itens em outros objetos ou no ambiente, falar com entidades.
4.  **Resolução de Puzzles:** Utilizar a lógica e os itens coletados para superar obstáculos, desbloquear novas áreas ou progredir na narrativa.
5.  **Mergulho em Livros:** Entrar em "mundos-livro" para vivenciar histórias e resolver quebra-cabeças internos que podem afetar a Biblioteca maior.

## 6. Mundo do Jogo (Setting)
### A Biblioteca
Uma entidade semi-senciente, vasta e aparentemente infinita. Não segue as leis normais da física ou do tempo. Seus corredores podem mudar, e suas seções podem refletir gêneros literários, emoções ou eras históricas.

*   **O Átrio de Chegada:** Ponto de partida, um local majestoso e um pouco intimidante.
*   **Corredores Labirínticos:** Conectam diferentes seções. Alguns podem ser estáveis, outros podem mudar suas conexões.
*   **Seções Temáticas:**
    *   **A Ala dos Contos Inacabados:** Livros com histórias que precisam de uma conclusão, talvez com almas presas em loop.
    *   **O Arquivo das Sabedorias Proibidas:** Livros perigosos, protegidos por enigmas ou guardiões.
    *   **O Jardim das Poesias Vivas:** Onde as palavras dos poemas se manifestam de forma literal.
    *   **A Galeria dos Bestiários Esquecidos:** Livros contendo criaturas que podem, por vezes, interagir com o jogador.
*   **Salas Ocultas:** Pequenas salas secretas que podem conter itens importantes, lore ou atalhos.
*   **"Mundos-Livro":** Ao interagir de forma específica com certos livros (ex: `ENTRAR LIVRO VERMELHO`), o jogador é transportado para um cenário contido dentro da narrativa daquele livro. Cada mundo-livro é um mini-ambiente com seus próprios desafios e regras.

## 7. Personagens
### O Jogador ("O Leitor" / "O Achado")
*   Sem passado definido, acorda na Biblioteca sem saber como chegou.
*   Motivação inicial: Descobrir onde está e como escapar.
*   Pode evoluir para um papel de curador, libertador de almas, ou buscador de um conhecimento específico.

### O Arquivista Chefe
*   Uma entidade antiga e enigmática, ligada à própria Biblioteca.
*   Pode ser um guia, um mentor, ou um antagonista sutil, dependendo das escolhas e do progresso do jogador.
*   Comunica-se através de mensagens crípticas, livros que aparecem subitamente, ou raros encontros diretos.

### Almas/Livros Falantes
*   Alguns livros podem se comunicar diretamente, representando a alma ou a história que contêm.
*   Podem pedir ajuda, oferecer pistas, contar suas histórias, ou tentar enganar o jogador.
*   Ex: "O Livro da Donzela Aprisionada", "O Grimório do Mago Arrependido".

### Guardiões da Biblioteca
*   Entidades que protegem certas seções ou livros. Podem ser hostis, ou podem ser superados através de lógica, oferendas corretas, ou respondendo a enigmas.

### Estratégia de Introdução de NPCs

Para enriquecer a exploração e a narrativa, a introdução de novos NPCs (Almas/Livros Falantes, Guardiões específicos, ou outros habitantes da Biblioteca) será frequentemente vinculada à descoberta de novas áreas ou seções temáticas da Biblioteca. Cada nova região explorável poderá apresentar personagens únicos que oferecem pistas, desafios, ou aprofundam a lore daquele local específico e da Biblioteca como um todo.

## 8. Narrativa e Objetivos
### Premissa Central
Você está perdido na Biblioteca das Almas Perdidas.

### Objetivo Principal
(pode variar ou ser escolhido pelo jogador indiretamente):
*   **Escapar:** Encontrar uma saída da Biblioteca de volta à realidade.
*   **Restaurar a Ordem:** A Biblioteca pode estar em desequilíbrio (livros corrompidos, almas presas injustamente). O jogador pode assumir a tarefa de consertar isso.
*   **Buscar Conhecimento:** Usar os vastos recursos da Biblioteca para encontrar um conhecimento específico que o jogador (ou o Arquivista) deseja.
*   **Tornar-se o Novo Arquivista:** Se o Arquivista Chefe estiver ausente, morrendo ou precisando de um sucessor.

### Conflitos
*   Puzzles e enigmas que bloqueiam o progresso.
*   Corredores que mudam, dificultando a navegação.
*   Almas corrompidas ou hostis.
*   Guardiões.
*   O mistério da própria Biblioteca e como o jogador chegou lá.

## 9. Mecânicas Principais
### Parser de Comandos
*   Primariamente `VERBO + SUBSTANTIVO` (ex: `PEGAR CHAVE`, `LER LIVRO`).
*   Pode incluir alguns `VERBO + SUBSTANTIVO + PREPOSIÇÃO + SUBSTANTIVO` (ex: `COLOCAR ORBE NO PEDESTAL`).
*   Comandos comuns: `IR [DIREÇÃO]`, `EXAMINAR [ALGO/LOCAL]`, `PEGAR [ITEM]`, `LARGAR [ITEM]`, `USAR [ITEM]`, `ABRIR [ALGO]`, `FECHAR [ALGO]`, `FALAR COM [ALGUÉM]`, `LER [ALGO]`, `INVENTARIO`, `AJUDA`, `SALVAR`, `CARREGAR`.

### Inventário
*   Capacidade limitada ou ilimitada (a ser definido).
*   Itens são cruciais para resolver puzzles.

### Sistema de Puzzles
*   **Baseados em Itens:** Usar o item certo no lugar certo (ex: chave na fechadura, um livro específico em uma estante vazia).
*   **Lógicos:** Decifrar códigos, sequências, padrões descritos em textos.
*   **Narrativos:** Entender a história de um livro ou de uma alma para saber como ajudá-la ou o que ela precisa.
*   **Ambientais:** Interagir com o ambiente de formas específicas (ex: `PUXAR ALAVANCA`).

### Mecânica de "Mergulho nos Livros"
*   Comando específico (ex: `MERGULHAR EM [NOME DO LIVRO]`, `ENTRAR NO [LIVRO]`).
*   O jogador é transportado para um ambiente descrito no livro, com seus próprios puzzles e interações.
*   Sair do livro pode requerer resolver seu conflito interno ou encontrar um "portal de saída" textual.
*   Ações dentro de um livro podem ter consequências fora dele.

### Estado do Mundo
O jogo deve rastrear:
*   Localização do jogador.
*   Itens no inventário.
*   Estado dos puzzles (resolvidos/não resolvidos).
*   Portas/passagens (abertas/fechadas).
*   NPCs encontrados e seu estado.
*   Eventos importantes que ocorreram.

## 10. Interface do Usuário (UI)
### Entrada
Prompt de comando (`> `).

### Saída
*   Descrição da localização atual (nome da sala, descrição visual/sensorial, saídas visíveis, itens/personagens presentes).
*   Feedback para cada comando do jogador (ex: "Você pega a esfera luminosa.", "Não há nada com esse nome aqui.", "Não entendi o que você quis dizer com 'XYZ'.").
*   Diálogos com NPCs.

### Comandos de Sistema
`AJUDA` (lista de verbos comuns), `SALVAR`, `CARREGAR`, `SAIR`.

## 11. Escopo Inicial (Para um protótipo/primeiro capítulo)
*   O Átrio de Chegada e 2-3 seções/corredores adjacentes.
*   Introdução ao Arquivista Chefe (talvez através de uma mensagem inicial).
*   1-2 Almas/Livros com quem interagir.
*   1 Livro "mergulhável" com um puzzle simples dentro dele.
*   2-3 puzzles principais para progredir entre as primeiras áreas.
*   Objetivo inicial claro: Sair do Átrio ou encontrar o primeiro fragmento de informação sobre a natureza da Biblioteca.

## 12. Possíveis Expansões Futuras
*   Múltiplos finais baseados nas escolhas e ações.
*   Um sistema de "corrupção" ou "purificação" de livros/almas.
*   Um antagonista mais ativo tentando distorcer ou destruir a Biblioteca.
*   Habilidades especiais que o jogador pode aprender (ex: "Sentir Ecos de Histórias", "Decifrar Linguagens Antigas").
*   Maior variedade de seções e mundos-livro.

Este documento serve como um guia inicial. Muitos detalhes serão descobertos e refinados durante o processo de desenvolvimento e escrita. O foco deve ser em criar uma atmosfera rica e puzzles inteligentes que recompensem a atenção e a curiosidade do jogador.
