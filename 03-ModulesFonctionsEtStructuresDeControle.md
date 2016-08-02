# Modules, fonctions et structures de contrôle

Si vous n'avez pas fait l'impasse sur les deux chapitre précédents, vous n'aurez certainement pas pu résister au besoin de tester, et pour cela, le plus simple aura été de passer par le shell interactif. Mais il est temps de passer aux choses sérieuses et de créer de véritables programmes.

Pour illustrer notre propos, nous allons développer un même module pour Erlang et Elixir. Pour chacun, nous verrons les différentes formes d'écritures possibles, ce qui nous permettra de passer en revue les principales structures de contrôle.

## Modules

Erlang et Elixir sont des langages fonctionnels. N'espérez donc pas apprendre quoi que ce soit qui s'approche de la programmation objet. Ici, à première vue,  tout est _plat_. Un module n'est, d'ailleurs, de prime abord, qu'un ensemble de fonctions regroupées dans un même espace de nommage. 

Avec Erlang, un module sera décrit dans un fichier qui devra **obligatoirement avoir le même nom que le module**, suivi de l'extension `erl`. Donc si notre module a comme nom `sample`, nous le décrirons dans un fichier `sample.erl`. Si vous ne respectez pas cette règle, la compilation échouera avec un magnifique message d'erreur :

```
sample.beam: Module name 'example' does not match file name 'sample'
```

Autre détail important, en Erlang un module est déclaré comme un atome, donc avec un nom sous la forme `/[a-z]+[a-zA-Z0-9_]*/` ; soit, il commence **obligatoirement** par une lettre minuscule, suivie par zéro ou plusieurs lettres (majuscules ou minuscules), chiffres et _underscores_. Enfin, pour déclarer notre module, nous utilisons le mot clé `-module` suivi, entre parenthèses, de son nom, et nous terminons par un point (`.`).

Le module que nous allons créer servira à résoudre quelques problèmes mathématiques, nous l'appellerons donc `mathematics`. Créez donc un fichier `mathematics.erl` et ajoutez-y la ligne suivante :

```
-module(mathematics).
```

C'est tout. Avec cette simple ligne, nous avons un module, qui certes ne comporte aucune fonction, mais c'est un module. Toute fonction qui se trouvera après cette déclaration fera automatiquement partie du module.

Avec Elixir, les choses sont très différentes. Tout d'abord, il n'y a aucune obligation de concordance entre le nom du fichier et le nom du module ; la seule exigence sur le nom du fichier, est qu'il soit suivi de l'extension `ex`. Pour ce qui est du nom du module, il doit avoir la forme `/[A-Z]+[a-zA-Z0-9_]*/` ; soit, la même que pour Erlang sauf qu'il commence **obligatoirement** par une majuscule.

> Elixir support la notion de groupe de nommage (`namespace`). Il est donc possible d'avoir un module `Hello` et un module `Hello.World`.

Pour déclarer un module, Elixir utilise le mot clé `defmodule` suivi du nom du module et d'un bloc commençant par `do` et finissant par `end`. Créez donc un fichier `methematics.ex` et ajoutez-lui le contenu suivant :

```
defmodule Mathematics do
end
```

Vous l'aurez compris, nous placerons dans le bloc `do`...`end` l'ensemble des fonctions du module. 

> Il est possible, avec Elixir, d'avoir plusieurs déclarations de modules dans un même fichier.

## Fonctions

Pour déclarer une fonction en Erlang, nous n'utilisons aucun mot clé particulier. Il suffit de lui donner un nom et de préciser, entre parenthèses, les paramètres. Ensuite, nous plaçons le corps de la fonction entre une _flèche_ (`->`) et un point (`.`) :

```
ma_fonction(parameters) ->
  % Corps de la fonction
  ok.
```

Dans l'exemple ci-dessus, j'ai mis, dans le corps de ma fonction, un simple commentaire et je l'ai terminé par `ok.`. En effet, en Erlang, comme dans beaucoup de langages, une fonction renvoie toujours la valeur du dernier terme exécuté, et doit **toujours** renvoyer une valeur (l'atome `ok` dans le cas présent). 

Avant de continuer, il est temps de faire un petit tour sur la ponctuation en Erlang. Si l'on prend les langages les plus utilisés aujourd'hui, nous trouverons une forme syntaxique dominante utilisant des délimiteurs de blocs de codes (souvent des accolades), avec, mais la tendance tent à l'éliminer, un délimiteur (souvent un point virgule) entre les expressions de ces blocs. Nous pensons ici au C/C++, Java ou PHP. Python a fait le choix de supprimer toute ponctuation en s'appuyant sur l'indentation. Erlang a choisi de se reposer sur une ponctuation dans laquelle :

* le point (`.`) sert à marquer la fin d'une fonction
* la virgule (`,`) sert à marquer la fin d'une expression
* le point-virgule (`;`) sert à séparer des conditions de choix
* et l'absence de ponctuation termine des conditions de choix

Voyons cela sur un exemple :

```
traiter(Reponse) ->
    io:format("Vous avez dit "),
    case Reponse of
        oui -> io:format(" oui!~n");
        non -> io:format(" non!~n");
        _ -> 
            io:format(" quelque chose"),
            io:format(" que je ne comprend pas!~n")
    end.
```

Nous avons donc ici la fonction `traiter` qui prend un paramètre (`Reponse`). Le corps de notre fonction est placé entre la _flèche_ (`->`) située à la fin de la première ligne et le point placé juste après le `end` de la dernière ligne. Notez que nous aurions très bien pu mettre le point final sur la ligne suivante. En effet, rien ne nous oblige à l'accoler au `end`. Dans le corps de la fonction, nous avons deux expressions. La première, à la ligne 2, pour écrire _"Vous avez dit "_ et la seconde (lignes 3 à 9) qui va nous permettre d'écrire une suite à notre phrase en fonction du contenu de `Reponse`. Nous avons donc séparé ces deux expressions par une virgule, placée à la fin de la ligne 2. Ici aussi, cet emplacement est purement arbitraire, et vous êtes libre de mettre cette virgule ou bon vous semble, tant qu'elle sépare bien les deux expressions. Dans l'expression de choix, commençant par `case ... of` (ligne 3) et se terminant par le `end` de la ligne 9, chaque choix est donné par la valeur du choix possible (`oui`, `non` ou _n'importe quelle valeur_ (`_`)) suivi d'une flèche, les expressions à exécuter pour le choix, et un point virgule (`;`) terminal (lignes 4 et 5), sauf après le dernier choix (ligne 8). Notez bien que, les expressions à exécuter pour un choix sont séparées par des virgules (ligne 7). 

Comme nous l'avons indiqué, la position des séparateurs (`->`, `,`, `;` et `.`) est totalement libre. De même que l'indentation. Vous pouvez donc parfaitement écrire le même code de la façon suivante :

```
traiter(Reponse) 
-> io:format("Vous avez dit ")
,   case Reponse of oui
-> io:format(" oui!~n")
;non 
-> io:format(" non!~n")
;
_   
-> 
io:format(" quelque chose")
,io:format("que je ne comprend pas!~n")
    end
.
```

Le reste n'est qu'une question de lisibilité.

> Cette ponctuation dans Erlang est souvent la source de pas mal d'erreures quand on commence à utiliser le langage. Il est donc important de bien la comprendre.

Elixir a abandonné la ponctuation d'Erlang, et comme vous avez pu le voir lors de la déclaration de notre module, s'est tourné vers une syntaxe ou les blocs sont encadrés par le _maintenant_ classique couple `do` ... `end`. 

Pour déclarer une fonction en Elixir, il faut utiliser le mor clé `def` suivi du nom de la fonction, de ses paramètres entre parenthèses et du bloc composant le corps :

```
def ma_fonction(parameters) do
    # Corps de la fonction
end
```

Comme avec Erlang, la fonction renverra le résultat de la dernière expression. Alors qu'il n'existe aucune valeur de retour par défaut en Erlang, Elixir a fait le choix de retourner `nil` par défaut. Ce sera le cas avec la fonction ci-dessus. Pour être cohérents avec l'exemple créé avec Erlang nous devons donc écrire ceci :

```
def ma_fonction(parameters) do
    # Corps de la fonction
    :ok
end
```

Pour Erlang, la règle de nommage d'une fonction est identique à celle d'un module (`/[a-z]+[a-zA-Z0-9_]*/`). Elixir suit la même règle, à un détail prés : une fonction peut commencer par un underscore (`_`) : `/[a-z_]+[a-zA-Z0-9_]*/`.

Maintenant que nous savons comment créer une fonction, ajoutons en une dans notre module `mathematics`. Modifiez le fichier `mathematics.erl` de la façon suivante :

```
-module(mathematics).

fibonacci(N) ->
  ok.
```

Si nous compilons ce code (nous verrons plus loin comment), nous aurons deux messages :

```
mathematic.erl:3: Warning: function fibonacci/1 is unused
mathematic.erl:3: Warning: variable 'N' is unused
```

Il ne s'agit que de _warnings_, donc rien de grave. Mais le premier message nous apporte une information importante. Il nous indique que la fonction `fibonacci` n'est pas utilisée ! Pour comprendre la raison de ce message, il faut savoir que par défaut, une fonction est privée dans un module Erlang. Donc elle ne peut être appelée que par une fonction du même module. Or ce n'est pas le cas ici. De plus nous avons besoin de la rendre publique afin de pouvoir l'utiliser. Pour cela nous avons le mot clé `-export` prenant en paramètre la liste des fonctions à exporter, suivi, pour chacune de son arité. Le code de notre module devient donc :

```
-module(mathematic).

-export([fibonacci/1]).

fibonacci(N) ->
  ok.
```

Avec Elixir c'est _l'inverse_ ; les fonctions sont par défaut publiques et il faudra remplacer `def` par `defp` pour déclarer une fonction privée à un module. Nous pouvons donc modifier le fichier `mathematics.ex` de la façon suivante :

```
defmodule Mathematics do
  def fibonacci(n) do
    :ok
  end
end
```

Bien qu'elle soit loin de faire ce que l'on en attend, nous pouvons déjà essayer de voir comment compiler et utiliser notre fonction. 

Nous allons rester dans le shell interactif pour le moment. Placez-vous dans le répertoire contenant le fichier `mathematics.erl` et démarrez un shell Erlang. Nous allons utiliser la fonction `c` du shell pour compiler le module. Cette fonction prend en paramètre un atome correspondant au nom du module (et donc du fichier sans l'extension) :

```
1> c(mathematics).
mathematics.erl:5: Warning: variable 'N' is unused
{ok,mathematics}
```
    
La fonction `c` nous renvoie un tuple dont le premier membre (l'atome `ok`) nous indique que la compilation s'est bien passée et dont le second membre est un rappel du nom du module. Comme nous nous y attendions, nous avons un _warning_ nous rappelant que la variable `N` n'est pas utilisée. 

Maintenant que notre fichier est compilé, nous pouvons appeler la fonction `fibonacci` :

```
2> mathematics:fibonacci(0).
ok
3>
```

Si vous sortez du shell, vous verrez qu'à côté du fichier `mathematics.erl`, Erlang a généré un fichier `mathematics.beam`. C'est le fichier contenant le _bytecode_ pour la VM Erlang.

Pour Elixir, nous utilisons également la fonction `c` du shell. A la différence de la fonction éponyme d'Erlang, elle prend ici un tableau de `bitstring` contenant le nom des fichiers à compiler :

```
iex(1)> c ["mathematics.ex"]
mathematics.ex:2: variable n is unused
[Mathematics]
```

En retour, `c` nous renvoie la liste des modules compilés. Nous pouvons maintenant utiliser les fonctions de ce module :

```
iex(2)> Mathematics.fibonacci(0)
:ok
```

Tout comme le compileur Erlang avait créé un fichier `BEAM`, Elixir a généré un fichier nommé `Elixir.Mathematics.beam` contenant le _bytecode_ pour la VM Erlang.

## Structures de contrôle

Il existe plusieurs manières de calculer les nombres de Fibonacci, la méthode récursive venant instinctivement à l'esprit. Partons donc de l'écriture algorithmique suivante :

```
Entier fibonacci(Entier n)
  Si n <= 1 Alors
    Renvoyer n
  Sinon
    Renvoyer fibonacci(n-2) + fibonacci(n-1)
   Fin Si
Fin
```

Il n'y a ici rien de mystérieux. Nous avons juste besoin de mettre en place une condition (**`Si...Sinon...Fin Si`**). Pour cela, il existe plusieurs solutions à notre disposition. 

Nous pouvons utiliser un `if`. En Elixir la syntaxe est très similaire à celle que l'on a l'habitude de trouver : 

```
if <condition> do
    <expression>
else
    <expression>
end
```

Nous pourrons donc écrire notre fonction `fibonacci` de la façon suivante :

```
def fibonacci(n) do
  if n <= 1 do
    n
  else
    fibonacci(1-2) + fibonacci(n-1)
  end
end
```

Pour Erlang la syntaxe est plus _exotique_ et s'écrit ainsi :

```
if
    <condition1> -> <expression1>;
    <condition2> -> <expression2>;
    ...
    <conditionN> -> <expressionN>
end
```

Erlang recherchera donc la première condition _vraie_ et renverra l'expression correspondante. Il est important de savoir qu'il doit **obligatoirement** y avoir une condition vraie, faute de quoi le compilateur vous rappellera à l'ordre avec un message d'erreur :

```
exception error: no true branch found when evaluating an if expression
```

Le compilateur ne pouvant pas savoir s'il y aura toujours une condition vrai, nous serons obligé d'écrire notre `if` en le terminant pas une condition `true` :

```
if
    <condition1> -> <expression1>;
    <condition2> -> <expression2>;
    ...
    true -> <expression>
end
```

Nous écrirons donc notre fonction `fibonacci` de la façon suivante :

```
fibonacci(N) ->
  if
    N =< 1 -> N;
    true -> fibonacci(N-2) + fibonacci(N-1)
  end.
```

Il est possible de remplacer le `if` par un `case`. Il s'agit là d'une structure de contrôle similaire au `switch...case` que l'on trouve dans d'autres langages, mais utilisant le principe du pattern matching. La syntaxe du `case` d'Elixir est la suivante :

```
case <value> do
    <matching1> -> <expression1> 
    <matching2> -> <expression2>
    ...
    <matchingN> -> <expressionN>
end
```

Elixir va donc rechercher le matching correspondant à la `valeur` et renvoyer l'évaluation de l'expression associée. Si aucun matching n'est trouvé, Elixir renverra une exception :

```
** (CaseClauseError) no case clause matching: <valeur>
```

Pour Erlang, la syntaxe `case` est très similaire :

```
case <value> of
    <matching1> -> <expression1> 
    <matching2> -> <expression2>
    ...
    <matchingN> -> <expressionN>
end
```

Et en cas d'absence de matching, l'exception sera la suivante :

```
** exception error: no case clause matching <value>
```

Nous pouvons donc réécrire nos fonctions `fibonacci` en utilisant le `case`.

`Elixir`
```
defmodule Mathematics do
  def fibonacci(n) do
    case n do
      0 -> 0
      1 -> 1
      other -> fibonacci(other-1) + fibonacci(other-2)
    end
  end
end
```

`Erlang`
```
-module(mathematics).

-export([fibonacci/1]).

fibonacci(N) ->
  case N of
    0 -> 0;
    1 -> 1;
    Other -> fibonacci(Other-1) + fibonacci(Other-2)
 end.
```

## _Guard_

Nous pouvons maintenant tester nos modules en utilisant les méthodes de compilation, via le shell, que nous avons vu précédemment :

`Erlang`
```
$ erl
1> c(mathematics).
{ok,mathematics}
2> mathematics:fibonacci(10).
55
```

`Elixir`
```
$ iex
iex(1)> c ["mathematics.ex"]
[Mathematics]
iex(2)> Mathematics.fibonacci(10)
55
```

Il y a cependant un problème que nous devons résoudre. En effet, comme vous l'avez remarqué, Erlang, comme Elixir, sont des langages à typage dynamique. Voyons donc ce qui se passe si l'on passe _autre chose_ qu'un entier à la fonction `fibonacci` :

`Erlang`
```
3> mathematics:fibonacci("hello").
** exception error: an error occurred when evaluating an arithmetic expression
     in function  mathematics:fibonacci/1 (mathematics.erl, line 9)
```

Nous avons une erreur lors de l'évaluation arithmétique de la ligne 9. C'est le fait de vouloir substituer `1` à `Other` (`Other-1`) qui génère cette erreur ; `Other` ayant ici la valeur `"hello"`. Pour Elixir, nous avons une erreur du même type :

```
iex(3)> Mathematics.fibonacci('hello')
** (ArithmeticError) bad argument in arithmetic expression
    mathematics.ex:6: Mathematics.fibonacci/1
```

Pour éviter ce type de problème, il faudrait pouvoir signifier que la fonction `fibonacci` ne prend que des entiers en paramètre. Il serait également bon de préciser que ces entiers doivent obligatoirement être positifs.

Pour cela, nous pouvons utiliser un _factionnaire_ (_guard_ en anglais) pour donner des conditions sur les paramètres de la fonction. Avec Erlang, cela se fait en utilisant le mot clé `when` placé avant le corps de la fonction, et suivi de la liste des conditions séparées par des virgules ou des points virgule. Dans le cas où les expressions de _guard_ sont séparées par des virgules, il faut qu'elles soient toutes vraies pour que l'ensemble soit respecté ; la virgule s'interprète donc comme un `and`. Avec le point virgule, le factionnaire _laisse passer_ si au moins une expression de _guard_ est respectée ; le point-virgule agit donc comme un `or`. Nous réécrirons donc notre code de la façon suivante :

```
-module(mathematics).

-export([fibonacci/1]).

fibonacci(N) when is_integer(N), N >= 0 ->
  case N of
    0 -> 0;
    1 -> 1;
    Other -> fibonacci(Other-1) + fibonacci(Other-2)
  end.
```

Elixir a apporté un peu plus de souplesse à la notion de _guard_ en permettant d'assembler les expressions de _guard_ via les opérateurs booléens `and`, `or` et `not`, certainement plus intuitif que la virgule et le point virgule d'Erlang.

```
defmodule Mathematics do
  def fibonacci(n) when is_integer(n) and n >= 0 do
    case n do
      0 -> 0
      1 -> 1
      other -> fibonacci(other-1) + fibonacci(other-2)
    end
  end
end
```

Avec ces modifications, si nous essayons de passer un paramètre invalide lors de l'appel de la fonction `fibonacci`, nous aurons une erreur nous indiquant qu'il n'existe pas de fonction pour le paramètre passé.

`Erlang`
```
$ erl
1> c(mathematics).
{ok,mathematics}
2> mathematics:fibonacci("hello").
** exception error: no function clause matching mathematics:fibonacci("hello") (mathematics.erl, line 5)
```

`Elixir`
```
$ iex
iex(1)> c ["mathematics.ex"]
[Mathematics]
iex(2)> Mathematics.fibonacci('hello')
** (FunctionClauseError) no function clause matching in Mathematics.fibonacci/1
    mathematics.ex:2: Mathematics.fibonacci('hello')
```

Les _guards_ peuvent s'utiliser partout où il peut y avoir un risque lié à un matching. Dans le cas de la fonction `fibonacci` il peut donc être associé au matching du `case`, nous laissant ainsi le loisir de gérer l'exception nous-même :

```
-module(mathematics).

-export([fibonacci/1]).

fibonacci(N) ->
  case N of
    0 -> 0;
    1 -> 1;
    Other when is_integer(Other), Other >= 0 ->
      fibonacci(Other-1) + fibonacci(Other-2);
    _ -> throw("Wrong parameter")
  end.
```

Dans ce cas, l'utilisation d'un paramètre invalide lèvera l'exception `Wrong parameter` :

```
$ erl
1> c(mathematics).
{ok,mathematics}
2> mathematics:fibonacci("hello").
** exception throw: "Wrong parameter"
     in function  mathematics:fibonacci/1 (mathematics.erl, line 11)
```

Nous pouvons, bien entendu, faire exactement la même chose avec Elixir.

Terminons en voyant comment nous pouvons utiliser les _guards_ sur des fonctions anonymes. Voici un exemple avec Erlang :

```
1> MyFun = fun(X, Y) when Y =/= 0 -> X / Y;
1>            (X, Y) when Y =:= 0 -> division_by_zero
1>         end.
#Fun<erl_eval.12.106461118>
2> MyFun(7, 3).
2.3333333333333335
3> MyFun(7, 0).
division_by_zero
```

Pour Elixir, nous déclarerons la même fonction de la façon suivante :

```
iex(1)> my_fun = fn
...(1)>   x, y when y != 0 -> x / y
...(1)>   x, y when y == 0 -> :division_by_zero
...(1)> end
#Function<12.106461118/2 in :erl_eval.expr/5>
iex(2)> my_fun.(7, 3)
2.3333333333333335
iex(2)> my_fun.(7, 0)
:division_by_zero
```

Nous avons simplement allégé l'écriture en omettant les parenthèses autour des paramètres.

## Retour sur le Pattern Matching

S'il est une chose qu'il faut toujours avoir à l'esprit avec Erlang (et Elixir) c'est que toute manipulation de donnée se fait en suivant les principes de _pattern matching_. Et **tout** veut dire que cela s'applique également aux paramètres des fonctions. Imaginons que nous ayons un tuple `{"hello", 123, an_atom}` et que nous souhaitons le passer à une fonction `my_function` dans laquelle nous aurons besoin de manipuler le tuple dans sa globalité, mais également de récupérer la valeur du troisième champs de ce tuple. Dans ce cas, nous pouvons définir la fonction de la façon suivante :

```
my_function({_, _, Atom} = Tuple) ->
  %...
```

Dans ce cas, dans la fonction, `Atom` a pour valeur `an_atom` et `Tuple` a pour valeur `{"hello", 123, an_atom}`. Avec Elixir, nous ferions cela de la façon suivante :

```
def my_function({_, _, atom} = tuple) do
  # ...
end
```

Sachant cela, nous pouvons simplifier encore notre fonction `fibonacci` en supprimant le `case` que l'on remplacera par du patern matching au niveau des paramètres de la fonction.

`Erlang`
```
-module(mathematics).

-export([fibonacci/1]).

fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) when is_integer(N), N > 1 ->
  fibonacci(N-1) + fibonacci(N-2);
fibonacci(_) ->
  throw("Wrong parameter").
```

> Notez bien l'utilisation du point-virgule (`;`) pour séparer les différentes déclarations de la fonction `fibonacci`.

`Elixir`
```
defmodule Mathematics do
  def fibonacci(0), do: 0
  def fibonacci(1), do: 1
  def fibonacci(n) when is_integer(n) and n >= 0 do
    fibonacci(n-1) + fibonacci(n-2)
  end
  def fibonacci(_), do: throw("Wrong parameter")
end
```

L'élément remarquable ici est l'utilisation de la syntaxe `def <function name>([<parameter>[, ...]]), do: <result>`. Quand une fonction ne nécessite pas de bloc, Elixir met à notre disposition cette forme d'écriture plus simple.

## Fibonacci itérative

Nous nous sommes instinctivement lancé dans la réalisation d'une solution récursive pour l'implémentation de la fonction `fibonacci`. Cependant nous aurions pu préférer la méthode itérative. Le plus simple, dans ce cas, consistant à passer par la forme linéaire :

```
Entier fibonacci(Entier n)
Entier f_n_1
Entier f_n
Entier i
Debut
    f_n_1 <- 0
    f_n <- 1
    Pour i dans {0..(n-2)} :
        (f_n_1, f_n) <- (f_n, f_n + f_n_1)
    Fin Pour
    Renvoyer f_n
Fin
```

Il s'agit donc de faire une boucle au cours de laquelle nous calculons, à chaque itération, les valeurs de `f(n)` et `f(n-1)`. Dans votre langage de prédilection, il ne vous faudra pas plus d'une minute pour écrire cette fonction. Et bien avec Erlang ou Elixir _ce n'est pas possible_ !!! En fait, si ! C'est possible. Mais pas forcement comme vous l'entendez. Ceci pour la simple raison qu'il n'existe pas de structure de contrôle permettant de faire des itérations. Donc, oubliez le `for` ou le `while` vous n'en trouverez pas trace dans Erlang ou Elixir.

Pour écrire une version itérative, nous allons devoir fouiller dans la librairie standard d'Erlang et d'Elixir pour trouver une solution. Voici, pour chaque langage, une implémentation possible.

`Erlang`
```
-module(mathematics).

-export([fibonacci/1]).

fibonacci(N) ->
  {_, Result} = lists:foldl(
    fun(_, {F_N_1, F_N}) ->
        {F_N, F_N_1 + F_N}
    end,
    {0, 1},
    lists:seq(2, N)
  ),
  Result.
```

Nous utilisons ici les fonctions `foldl` et `seq` du module `lists`.

La fonction `foldl` prend en paramètre une fonction, un accumulateur et une liste. Pour chaque élément de la liste, la fonction recevra en paramètre l'élément courant et l'accumulateur et renverra une version _à jour_ de l'accumulateur. Dans le cas présent, notre accumulateur est un tuple contenant les valeurs de la suite de Fibonacci pour `f(n-1)` et `f(n)`. A chaque itération, nous mettons à jour cet accumulateur en replaçant les deux valeurs, respectivement, par `f(n)` et `f(n-1) + f(n)` (ligne 8). Nous initialisons cet accumulateur avec le couple `{0, 1}` correspondant aux deux premières valeurs de la suite (ligne 10). Et nous faisons appel à la fonction pour l'ensemble des éléments de la séquence comprise entre 2 et `n` (ligne 11). La séquence ne sert ici qu'à gérer l'itération et dans les faits nous n'avons pas besoin de la valeur de l'élément courant. C'est la raison pour laquelle nous l'avons ignoré dans la déclaration de la fonction (ligne 7).

Pour Elixir, la solution est très semblable :

```
defmodule Mathematics do
  def fibonacci(n) do
    {_, result} = Enum.reduce(
      2..n,
      {0, 1},
      fn(_, {f_n_1, f_n}) ->
        {f_n, f_n_1 + f_n}
      end)
    result
  end
end
```

Ici nous utilisons la fonction `reduce`, du module `Enum`, qui prend en paramètre une liste, un accumulateur et une fonction. Notez que nous aurions également pu utiliser la fonction `List.foldl`. Nous utilisons ici un `range` pour créer implicitement la liste gérant l'itération.

## Conclusion

Maintenant que nous savons créer des modules et des fonctions, et que nous avons vu les bases sur les structures de contrôle, nous pouvons commencer à développer pleinement. Il faudra cependant parcourir la librairie standard d'Erlang et Elixir. Elles sont extrêmement riches. Prenez bien le temps de chercher avant de réinventer la roue.
