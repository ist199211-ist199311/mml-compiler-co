# Teste Prático

### *Early Bird* 2023-06-16 18:00

Implementar nova instrução:

```
iterate <vec> for <count> using <func> if <cond>;
```

Se a condição `<cond>` for verdadeira, executa `<count>` iterações a percorrer
o vetor `<vec>` (área de memória indexável por ponteiro), chamando a função
`<func>` para cada elemento do vetor, devendo esta aceitar um argumento
do tipo apontado por `<vec>`.

Por exemplo:

```
auto square = (int el) -> void { el * el!! };

auto f = ([int] vec, int size) -> void {
    iterate vec for 2 using square if size > 2;
};
```

É necessário alterar o projeto quanto à análise lexical, análise sintática,
análise semântica, e geração de código.

Programa de testes (**não** disponibilizado durante o teste prático) disponível
em `iterate.mml`.
