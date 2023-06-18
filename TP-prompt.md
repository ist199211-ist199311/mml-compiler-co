```
unless <condition> iterate <vector> for <count> using <function>
```

Exemplo:

```
auto square = (int element) -> void { element * element!! };
auto f = ([int] vector, int size) -> void {
  unless size < 2 iterate vector for 2 using square;
};
```
