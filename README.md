## Execução

Para compilar os tokens: 
```
alex tokens.x
```

Para compilar o parser:
```
ghc parser.hs
```

Para rodar o parser:
```
.\parser.exe <nome do arquivo>
```

## Estrutura da memória

A memória da linguagem é manipulada sob a seguinte estrutura de dados:

```
(Bool¹, [(Token, Token)]², [(Token, [(Token, Token)], [Token])]³, [[(Token, (Token, Token))]]⁴, [[(Token, [(Token, Token)], [Token])]]⁵)
```

1. **Flag**: Uma variável booliana que será acionada para fazer mudanças semânticas durante a análise sobre blocos de códigos, como subprogramas. Quando estiver falsa, o bloco de código será analisado apenas sintaticamente e, se necessário, guardado na memória.
2. **Variáveis**: Uma lista de tuplas, onde cada tupla possui dois tokens que guardarão informações das variáveis existentes no escopo do programa principal. O primeiro Token é o Token ID da variável. O segundo Token é o Token *TypeValue**.
3. **Sintaxe de Funções**: Uma lista de triplas onde o primeiro elemento é um Token ID com o nome da função guardada; O segundo elemento é uma lista de tuplas (ID, *TypeValue**) guardando os parâmetros; E o terceiro elemento é uma lista de Tokens referentes ao corpo da função.
4. **Structs**: Uma lista de listas de tuplas, onde será armazenado as *structs* criadas pelo usuário. Deve ser interpretado da seguinte forma: Uma lista de structs, onde cada struct é uma tupla com o primeiro elemento sendo o Token ID do seu nome e o segundo elemento sendo uma lista de variáveis, onde cada variável é uma tupla (ID, *TypeValue**).
5. **Pilha de ativação**: É uma lista de estruturas de funções, onde serão instanciadas as funções ativadas, atribuindo seus valores e fazendo a análise de seus tokens. Dessa forma, é somente uma lista do item 3. Entretanto, onde é guardado só os parâmetros na sintaxe, será também guardado as variáveis locais a medida que elas vão sendo inicializadas e removidas.

*Tokens TypeValue são os Tokens BoolValue, FloatValue, IntValue, StringValue, CharValue que, por sua vez são constituidos de tuplas: (Token, Valor, Posição).

## Problemas

### Problema 1
Crie um programa que, dados três valores numéricos *x*, *y* e *c*, onde *x* e *y* são números racionais e c é um número inteiro, previamente armazenados no códigofonte, avalia a expressão *x² − y + c* e imprime seu resultado na tela.

### Problema 2
Crie um programa que leia uma quantidade desconhecida de números e informe quantos deles estão nos seguintes intervalos fechados: [0, 25], [26, 50], [51, 75] e [76, 100]. A entrada de dados deve terminar quando for lido um número negativo.

### Problema 3
Crie um programa que leia duas matrizes numéricas e, quando possível, imprima a soma e o produto dessas matrizes. Caso uma operação não possa ser realizada para as matrizes lidas, imprima uma mensagem informando da impossibilidade.

### Problema 4 
Defina o tipo *rational_t* para representar números racionais. O tipo *rational_t* deve ser representado como um registro (ou tipo correspondente) com campos inteiros numerador e denominador. Em seguida, escreva os seguintes subprogramas:
- Um programa que, dados dois parâmetros inteiros *a* e *b*, onde *b != 0*, retorna um valor *rational_t* para representar a fração *a/b*.
- Subprograma que, dados dois parâmetros do tipo rational_t*, retorna *true* se eles representam o mesmo número racional ou *false*, em caso contrário.
- Subprogramas que retornem um valor *rational_t* correspondente a soma, negação, subtração, multiplicação, inverso e divisão entre valores *rational_t*, passados como parâmetros (um subprograma por operação).

No programa principal, invoque cada um dos subprogramas e imprima os resultados produzidos, indicando numerador e denominador.

### Problema 5
Crie um subprograma chamado mdc, com três argumentos n, m (passados por valor) e r (passado por referência), nesta ordem. O subprograma mdc deve calcular o maior divisor comum entre dois números naturais estritamente positivos n e m, de acordo com o seguinte algoritmo recursivo:
- Se n for um divisor de m, n é o maior divisor comum de n e m.
- Se m for um divisor de n, m é o maior divisor comum de n e m.
- Se n não for um divisor de m, e se m for maior que n, então o maior divisor comum de m e n é também o maior divisor comum de n e do resto da divisão de m por n.

O subprograma deve retornar seu resultado por meio de parâmetro r, que deve ser posteriormente impresso na tela pelo programa principal.

### Problema 6
Uma árvore binária de busca generaliza a ideia de listas encadeadas crescentes. Em uma árvore binária de busca, os nós têm um campo chave de um tipo ordenável e apresentam as seguintes propriedades: para qualquer nó n, a chave de n é maior ou igual à chave de qualquer nó na subárvore esquerda de n e menor ou igual à chave de qualquer nó na subárvore direita de n. Implemente uma árvore binária de busca com chaves de tipo inteiro e as seguintes operações:
- Transforme uma sequência de valores em uma árvore binária de busca.
- Encontre a chave mínima da árvore, indicando seu nível.
- Encontre a chave máxima da árvore, indicando seu nível.
- Imprima a árvore de busca na saída padrão, nível a nível.