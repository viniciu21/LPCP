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
(Bool¹, [(Token, Token)]², [[Token]]³, [[(Token, Token)]]⁴)
```

1. Uma variável booliana que será acionada para fazer mudanças semânticas durante a análise sobre blocos de códigos, como subprogramas. Quando estiver falsa, o bloco de código será analisado apenas sintaticamente e, se necessário, guardado na memória.
2. Uma lista de tuplas, onde cada tupla possui dois tokens que guardarão informações das variáveis existentes no escopo. O primeiro Token é o Token ID da variável. O segundo Token é o Token *TypeValue**.
3. Uma lista de lista de Tokens, onde serão guardadas a lista de *statements* de uma função. Ou seja, é uma lista de funções onde cada função é representada por uma lista de Tokens.
4. Uma lista de listas de tuplas de Tokens, onde será armazenado as *structs* criadas pelo usuário. Deve ser interpretado da seguinte forma: Uma lista de structs, onde cada struct é uma lista de variáveis, onde cada variável é uma tupla (ID, *TypeValue**).

*Tokens TypeValue são os Tokens BoolValue, FloatValue, IntValue, StringValue, CharValue que, por sua vez são constituidos de tuplas: (Token, Valor, Posição).