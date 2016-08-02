# Macros et Protocoles

Nous allons ici toucher à des notions qui, bien qu'elles soient regroupées sous un même chapeau, sont très différentes entre Erlang et Elixir. C'est principalement le cas pour les macros. En ce qui concerne les protocoles, il n'y a pas d'équivalent en Erlang.

## Les macros

### Erlang

Avec Erlang, quand nous parlons de macro, nous pouvons faire un parallèle avec ce que l'on retrouve en C. En effet, contrairement à Elixir, Erlang utilise les principes de préprocessing lexical ; donc est capable de faire de la substitution avant la génération de l'arbre syntaxique. Pour nous en rendre compte, voyons l'AST qui sera généré par Erlang si nous définissons une macro. Pour faire cela, voyons avant tout comment définir une macro et l'utiliser.

Pour créer une macro, nous utilisons l'attribut `-define`, suivie, entre parenthèses, du nom et du _remplacement_. Le nom est composé d'une suite de caractères alphanumériques, auxquels s’ajoute l’underscore (`_`), et doit *obligatoirement* commencer par une lettre ou un underscore. Le _remplacement_ peut être n'importe quelle expression Erlang valide.

```
-define(Hello, "Hello").
-define(world, "World").
-define(_1234, "Hello World").
```

Il est possible de créer des macros avec paramètre. Pour ce faire, nous les spécifierons entre parenthèses, après le nom :

```
-define(ERROR(Message, Code), {error, Code, Message}).
-define(LOG(Message), io:format("[LOG] ~p~n", [Message]).
```

Pour utiliser une macro, nous ferons précéder son nom d'un point d'interrogation :

```
?Hello. 
?LOG("Ceci est un message.").
```

Notez qu'il est possible d'utiliser une macro dans la définition d'une autre macro : 

```
-define(App, "My Application").
-define(LOG(Message), io:format("[~s] ~p~n", [?App, Message])).
```

Et, bonne nouvelle, il n'y a aucune contrainte sur l'ordre de définition. Ainsi, la macro `App` pourrait très bien être définie après la macro `LOG(Message)`

Revenons maintenant à notre arbre syntaxique. Pour cela, nous allons écrire le code suivant :

```
-module(example).
-export([test/0]).

-define(MA_MACRO, "ma macro").

test() ->
  ?MA_MACRO.
```

Voici l'arbre syntaxique de ce code : 

```
01 [{attribute,1,file,{"example.erl",1}},
02  {attribute,1,module,example},
03   {attribute,2,export,[{test,0}]},
04    {function,6,test,0,[{clause,6,[],[],[{string,7,"ma macro"}]}]},
05     {eof,8}]
```

Sans entrer dans les détails, nous devons noter ici que `?MA_MACRO` a bien été remplacé par sa valeur (ligne 4). Pour avoir quelque chose de plus _lisible_ nous pouvons également regarder la sortie après l'étape de préprocessing. Pour cela il suffit d'exécuter la commande `erlc` avec l'option `-P` sur le fichier `example.erl`. A l'issu de cette exécution, vous trouverez un fichier `example.P` contenant le code suivant :

```
-file("example.erl", 1).

-module(example).

-export([test/0]).

test() ->
    "ma macro".
```

En complément de celles que nous pouvons créer nous même, Erlang propose les macros prédéfinies suivantes :

| Macro            | Définition                                            |
| :--------------- | :---------------------------------------------------- |
| `?MODULE`        | Donne le nom du module courant.                       |
| `?MODULE_STRING` | Donne le nom du module courant, sous forme de chaine. |
| `?FILE`          | Donne le nom du fichier du module courant.            |
| `?LINE`          | Donne le numéro de la ligne courante.                 |
| `?MACHINE`       | Donne le nom de la machine Erlang (`'BEAM'`).         |

Afin de rendre les macros encore plus intéressantes, il peut être utile de pouvoir les _partager_ entre plusieurs modules ; sur le même modèle que ce que nous pourrions faire en C avec les fichiers _headers_ (`.h`). Erlang propose un mécanisme d'_include_ similaire. Nous l'avons effleuré, sans vraiment l'évoquer, lorsque nous avons écrit un fichier `.hrl` au chapitre 5. En effet, nous savons déjà que l'extension des fichiers Erlang est `.erl`. Les fichiers `.hrl` sont des fichiers d'entête dans lequel nous placerons les définitions que nous voudrons partager entre plusieurs modules (macros, records ...). Pour accéder au contenu d'un fichier d'entête depuis un module, nous utiliserons l'attribut `-include(...)` avec, entre les parenthèses, le chemin vers le fichier `.hrl`. Cet attribut doit être déclaré après l'attribut `-module(...)`.

```
-module(my_module).

-include("path/to/definitions.hrl").

...
```

Il existe également l'attribut `-include_lib(...)` qui permet également de charger un fichier d'entête, mais dans ce cas, le chemin n'est pas absolu, mais correspond au chemin d'un fichier d'include d'une _autre_ application.

Nous aborderons la gestion de dépendances au chapitre 8, cependant, pour comprendre l'utilisation d'`-include_lib`, disons simplement que si nous écrivons une application ayant comme dépendance l'application `mon_application`, fournissant elle même un fichier d'entête `mes_definitions.hrl` placé dans le répertoire `include`, pour utiliser ce fichier depuis un module de notre application, nous utiliserons :

```
-include_lib("mon_application/include/mes_definitions.hrl").
```

Ceci permet de simplifier l'accès au fichier d'entête, sans avoir à nous soucier de son emplacement _réèl_.

### Elixir

Avec Elixir, les macros ont pris une dimension différente qui s'écarte grandement de ce que propose Erlang. En effet, Elixir n'utilise pas les principes de préprocessing, au profit d'une solution basée sur les fonctions. En fait, il ne s'agit pas de fonctions telles que nous les avons vus au chapitre 4, mais bien de macros. 

Nous utiliserons `defmacro` de la même manière qu'une définition de fonction (`def`). Donc en le faisant suivre par un nom (respectant la même convention de nommage que les fonctions) puis un bloc dans lequel nous coderons le comportement. Ainsi dans le cas d'une définition simple, nous aurons le code suivant :

```
defmodule Macros do
  defmacro hello do
    "Hello"
  end
  defmacro _1234, do: "world"
end
```

Vous noterez que, contrairement à Erlang, les macros sont définies dans un module. Ainsi, pour utiliser notre macro, nous ferons un appel du type :

```
Macros.hello
Macros._1234
```

Nous pouvons vouloir partager des macros entre différents modules. Dans ce cas, il faudra _importer_ le module définissant la ou les macros dans le module souhaitant les utiliser. Pour cela nous utiliserons `require` suivi du nom du module :

```
defmodule Sample do
  require Macros
  def hello_world do
    Macros.hello <> " " Macros._1234
  end
end
```

`require` n'est utile que si l'on souhaite accéder aux macros définies dans le module. Pour utiliser les fonctions, nous n'en avons pas besoin. 

Il existe également le mot clé `import` qui permet d'importer le contenu d'un module dans un autre, en nous permettant d'éviter l'utilisation du qualifier. Prenons comme exemple le module suivant :

```
defmodule HelloWorld do
  defmacro hello do
    "Hello"
  end

  def world do
    "world"
  end
end
```

Si maintenant nous utilisons `import` pour y accéder dans un nouveau module, nous pourrons écrire ceci :

```
defmodule TestHW do
  import HelloWorld
  def test do
    HelloWorld.hello <> " " <> world
  end
end
```

Comme vous pouvez le voir, pour utiliser la macro (`hello`) nous avons du spécifier le qualifier (le nom du module dans lequel elle est définie) alors ue pour la fonction (`world`) nous avons fait un appel direct.

`import` permet de spécifier ce que l'on souhaite importer (ou non) dans le module courant. Pour cela nous pouvons utiliser `only` ou `except` pour préciser, respectivement, que l'on ne souhaite importer que certaines fonctions et /ou macros, ou, au contraire, que l'on ne souhaite pas en importer certaines. `only` et `except` seront suivi d'une liste de nom de fonctions et macro avec leur arité, sous forme de liste de clé valeur. Ainsi les deux écritures suivantes donnent le même résultat :

```
import HelloWorld, only: [hello: 0]
import HelloWorld, except: [world: 0]
```

Les macros que nous avons vues jusqu'à maintenant sont relativement simples. Nous pouvons bien entendu créer des choses plus complexes. Pour cela nous devons comprendre la structure des expressions internes du langage. Pour cela nous allons prendre en exemple en utilisant la macro `quote` permettant de récupérer une telle structure. Voici un exemple :

```
iex(1)> quote do: hello("world")
{:hello, [], ["world"]}
```

`quote` nous a renvoyé un tuple donc le premier élément est un atome correspond au nom de la fonction (`hello`). Le second élément est une liste de métadonnées (vide dans le cas présent). Le dernier est la liste des paramètres envoyés à la fonction. 

Pour bien comprendre, voyons un second exemple :

```
iex(2)> quote do: IO.puts("world")
{{:., [], [{:__aliases__, [alias: false], [:IO]}, :puts]}, [], ["world"]}
```

Si nous découpons le résultat en partant du niveau le plus profond, nous identifions le tuple `{:__aliases__, [alias: false], [:IO]}` qui a la même force que ce que nous avons vu sur l'exemple précédent. L'atome `:__aliases__` nous indique que nous avons affaire à un alias, ce qui, dans le cas d'Elixir peut s'interpréter comme ce que l'on connaît sous le terme de _namespace_ dans d'autres langages. Dans le cas présent il s'agit de l'alias pour le module `IO`. Si nous notons ce tuple `IO_ALIAS` et que nous remontons d'un niveau, nous identifions le tuple `{:., [], [{:__aliases__, [alias: false], [:IO]}, :puts]}`, que nous simplifierons donc de la façon suivante : `{:., [], [IO_ALIAS, :puts]}`. Ici encore nous retrouvons un tuple à trois éléments. Le premier élément (l'atome `:.`) indique que nous faisons un appel de fonction, la fonction étant définie via les paramètres donnés par le tableau donné en troisième ; soit `IO.puts`. En remontant au dernier niveau, si nous remplaçons le tuple d'appel à la fonction par `IO_PUTS`, nous identifions `{{:., [], [{:__aliases__, [alias: false], [:IO]}, :puts]}, [], ["world"]}` que nous simplifions donc en `{IO_PUTS, [], ["world"]}`. Nous retrouvons à nouveau le même format de tuple qui qualifie bien l'appel de fonction `IO.puts("world")`.

Il peut être intéressant de pouvoir _agir_ sur le contenu de cette représentation interne en y injectant des données venant du contexte courant. Pour voir cela, imagions que nous ayons une variable `x` et que nous souhaitions injecter son contenu dans la représentation interne d'un appel de fonction. Nous pouvons essayer ceci :

```
iex(3)> x = "Hello World"
"Hello World"
iex(4)> quote do: IO.puts(x)
{{:., [], [{:__aliases__, [alias: false], [:IO]}, :puts]}, [],
 [{:x, [], Elixir}]}
```

Nous nous retrouvons avec une représentation qui coïncide bien avec ce que nous avons écrit, à savoir un appel de la fonction `puts` de l'alias `IO` avec le paramètre `x` ; mais qui ne correspond pas à ce que l'on attendait, à savoir un appel avec, en paramètre, la chaine `"Hello World"`. Pour régler cela, Elixir met à notre disposition la macro `unquote` qui permet d'injecter un contenu. Pour cela nous écrirons ceci :

```
iex(5)> quote do: IO.puts(unquote(x))
{{:., [], [{:__aliases__, [alias: false], [:IO]}, :puts]}, [], ["Hello World"]}
```

Nous avons maintenant la représentation exacte de ce que nous attendions.

La question que vous devez vous poser est "pourquoi avoir parlé de cette représentation ?". Pour le comprendre, définissons la macro suivante :

```
defmodule Sample do
  defmacro double(x) do
    2 * x
  end
end
```

Essayons maintenant d'utiliser cette macro :

```
iex(1)> c "sample.ex"
[Sample]
iex(2)> import Sample
nil
iex(3)> Sample.double(3)
6
iex(4)> Sample.double(2 + 3)
** (ArithmeticError) bad argument in arithmetic expression
             sample.ex:3: Sample."MACRO-double"/2
             iex:4: Sample.double/1
    (elixir) iex:4: :elixir_compiler.__FILE__/2
iex(4)>
```

Dans le premier cas, lors de l'appel avec un simple entier, nous obtenons le résultat attendu. Dans le second, lorsque nous passons l'expression arithmétique, nous recevons une erreur. Ceci est lié au fait que dans la macro, les paramètres reçus sont _quotés_. Quand nous avons envoyé l'entier 3, la macro a donc reçu : 

```
iex(4)> quote do: 3
3
```

Soit l'entier, qu'elle a donc essayé de multiplier par deux, ce qui nous a donné comme réponse 6. Par contre quand nous lui avons envoyé `3 + 4` elle à reçu : 

```
iex(5)> quote do: 2 + 3
{:+, [context: Elixir, import: Kernel], [2, 3]}
```

Soit le tuple de la représentation interne de cette valeur, ce qui a donc généré la levée d'exception, la multiplication d'un entier (`2`) par un tuple (`{:+, [context: Elixir, import: Kernel], [2, 3]}`) n'étant pas possible. Pour pouvoir résoudre ce problème il faut _unquoté_ le paramètre. Or `unquote` ne peut s'utiliser que dans un bloque `quote`. Nous devrons donc réécrire notre macro de la façon suivante :

```
defmodule Sample do
  defmacro double(x) do
    quote do
      2 * unquote(x)
    end
  end
end
```

Ce qui nous donnera le résultat suivant :

```
iex(1)> c "sample.ex"
[Sample]
iex(2)> import Sample
nil
iex(3)> Sample.double(2 + 3)
10
```

Pour être certain que vous avez bien compris, ou dans le cas ou il persisterait un doute, nous pouvons examiner ce que renvoie exactement la macro. Pour cela nous allons _quoté_ l'appel à la macro, puis demandé à obtenir l'AST de son exécution :

```
iex(4)> retour = quote do: Sample.double(2 + 3)
{{:., [], [{:__aliases__, [alias: false], [:Sample]}, :double]}, [],
 [{:+, [context: Elixir, import: Kernel], [2, 3]}]}
iex(5)> res = Macro.expand_once(retour, __ENV__)
{:*, [context: Sample, import: Kernel],
 [2, {:+, [context: Elixir, import: Kernel], [2, 3]}]}
```

Grace à la fonction `Macro.expand_once/2` nous retrouvons bien la représentation interne de la multiplication par 2 de l'addition. 

Nous en déduisons donc bien que, dans le cas d'une macro, les arguments passés ne sont pas évalués à priori, mais bien globalement dans la macro. Ceci nous permettra donc de créer ce genre de chose :

```
defmodule Sample do
  defmacro print(do: block) do
    quote do
      IO.puts(unquote(block))
    end
  end
end
``` 

Ce qui nous permettra d'écrire :

```
iex(1)> c "sample.ex"
sample.ex:1: warning: redefining module Sample
[Sample]
iex(2)> import Sample
nil
iex(3)> Sample.print do
...(3)> x = 2 + 3
...(3)> "resultat = #{x}"
...(3)> end
resultat = 5
:ok
```

Vous l'avez maintenant compris, grâce aux macros nous pouvons entrer dans le monde merveilleux de la métaprogrammation, créer nos propres DSL...

## Les Protocoles

### Elixir

La notion de protocole est une spécificité d'Elixir qui permet de pousser un cran plus loin les besoins en terme de polymorphisme. Nous avons déjà vu, au chapitre 4 que nous pouvons très facilement gérer le polymorphisme grâce aux _guards_ :

```
defmodule Sample do
  def square(i) when is_integer(i) do
    i * i
  end
  def square(_) do
    :unavailable
  end
end
```

Imaginons maintenant que, dans une application, nous ayons besoin de calculer le carré d'un nombre flottant. Nous ne pouvons pas utiliser le module `Sample` car il n'implémente cette possibilité que pour les entiers. Nous sommes donc obligés de modifier le code du module. Quand nous maitrisons l'ensemble du code, ce n'est pas forcement un problème, mais si ce n'est pas le cas, nous pouvons être amené à devoir engager de gros développement. Pour résoudre cela, Elixir a mis en place la notion de protocole.

Un protocole regroupe un ensemble de définitions de fonction sans leur implémentation. Si vous connaissez Java, vous pouvez comparer cela aux interfaces. Par la suite, sur la base d'un protocole, nous définirons des implémentations. Ces implémentations peuvent être distinctes les unes des autres, ce qui nous permettra, de définir une implémentation qui n’existerait pas, mais dont on aurait besoin.

si nous reprenons l'exemple précédent, nous commencerons donc par écrire le protocole :

`sample_proto.ex` :
```
defprotocol Sample do
  def square(x)
end
```

Nous pouvons ensuite développer une implémentation, pour les entiers :

`sample_int.ex` :
```
defimpl Sample, for: Integer do
  def square(x), do: x * x
end
```

Nous utilisons `defimpl` pour lequel nous spécifions, via `for` le type accpeté pour l'implémentation. Nous pouvons donc maintenant utiliser la fonction `Sample.square/1` avec des entiers : 

```
iex(1)> Sample.square 2
4
```

Et si jamais nous essayons d'utiliser un protocole avec un type pour lequel il n'existe pas d'implémentation, Elixir lèvera une exception :

```
iex(2)> Sample.square 2.3
** (Protocol.UndefinedError) protocol Sample not implemented for 2.3
     sample_proto.ex:1: Sample.impl_for!/1
     sample_proto.ex:2: Sample.square/1
```

Mais nous pouvons très facilement ajouter une nouvelle implémentation dans notre propre code, sans modifier le code d'un module.

Il est possible de créer des implémentations de protocoles pour tous les types de base (`Atom`, `BitString`, `Float`, `Function`, `Integer`, `List`, `Map`, `PID`, `Port`, `Reference`, `Tuple`), mais également pour les `records`. Nous pouvons donc écrire ceci :

```
defrecord MyRecord, value: 0 do
  record_type value: Integer.t
end

defimpl Sample, for: MyRecord do
  def square(x), do: x.value * x.value
end
```

Pour terminer, il faut savoir que, dans le cas d'un protocole déclarant plusieurs fonctions, nous n'avons aucune obligation d'implémenter toutes les fonctions. Elixir se contentera simplement de vous avertir, par un _warning_ lors de la compilation. Enfin, si nous en avons besoin, il est possible de créer un implémentation _par defaut_. Pour cela, il faudra développer un implémentation pour le type `Any` et préciser dans le protocole que si un implémentation n'existe pas pour un type, alors il doit utiliser celle pour `Any` :

`sample_proto.ex` :
```
defprotocol Sample do
  @fallback_to_any true
  def square(x)
end

defimpl Sample, for: Any do
  def square(x), do: "#{x} * #{x}"
end
```

Nous avons utilisé `@fallback_to_any` positionné à `true` dans le protocole, donc si nous utilisons `Sample.square/1` sur un type pour lequel il n'existe aucune implémentation, Elixir appèlera la fonction pour l'implémentation `Any` :

```
iex(1)> Sample.square("hello")
"hello * hello"
```

`@fallback_to_any` ne prend en compte que les cas ou il n'existe **aucune** implémentation pour le type. S'il existe une implémentation partielle, Elixir tentera d'appeler la fonction pour le type, et si elle n'exste pas, nous recevrons une erreure. Pour le voir, nous pouvons modifier le protocole et l'implémentation pour `Any` de la façon suivante :

`sample.proto.ex` :
```
defprotocol Sample do
  @fallback_to_any true
  def square(x)
  def add(x)
end

defimpl Sample, for: Any do
  def square(x), do: "#{x} * #{x}"
  def add(x), do: "#{x} + #{x}"
end
```

Si nous ne modifions pas l'implémentation pour les entiers (`sample_int.ex`), lors de la compilation, nous aurons le message suivant :

```
$ elixirc sample_proto.ex sample_int.ex
sample_int.ex:1: warning: undefined protocol function add/1 (for protocol Sample)
```

Et à l'utilisation, nous aurons bien un erreure si nous essayons d'appeler `Sample.add/1` avec un entier :

```
iex(1)> Sample.add("hello")
"hello + hello"
iex(2)> Sample.add(3)
** (UndefinedFunctionError) undefined function: Sample.Integer.add/1
     Sample.Integer.add(3)
```

### Erlang

Erlang ne propose pas de support des protocoles. Cependant, nous pouvons essayer de nous en approcher. Ceci va être pour nous l'occasion d'aborder de nouveaux principes.

Comme nous l'avons vu avec Elixir, l'idée d'un protocole est de proposer un ensemble de fonctions regroupées sous un même module, mais dont le résultat sera spécifique au type de la donnée sur lequel nous l'appliquerons. Prenons comme exemple un protocole nous permettant de faire le produit de deux valeurs. Pour cela nous pouvons déjà écrire, comme base de notre protocole, ceci :

`sample.erl` :
```
-module(sample).
-export([mul/2]).

mul(A, B) -> 
  ok. % TODO
```

Dans le cas présent, nous ne pouvons pas laisser la fonction `sample:mul/2` vide. Il faudra obligatoirement qu'elle fasse quelque chose. Avant de voir cela, nous pouvons avancer et déjà imaginer ce que pourrait donner le code de l'implémentation de ce _protocole_ dans le cas d'un entier :

`sample_int.erl` :
```
-module(sample_int).
-export([mul/2]).

mul(A, B) -> 
  A * B.
```

En ce qui concerne ce code, nous sommes certains qu'il répond bien à l'implémentation. Cependant, rien ne nous oblige à créer la fonction `sample_int:mul/2`. La preuve étant que si nous ne la créons pas, et que nous compilons `sample_int.erl`, Erlang ne dira rien. Et pour cause, il n'a aucune idée de nos intentions. Pour régler ce problème, nous pouvons utiliser le principe de comportement tel que ceux que nous avons vus au chapitre 6. En effet, nous pouvons créer un comportement afin d'indiquer que le module `sample_int` doit avoir un comportement donné, et doit donc implémenter certaines fonctions déterminées. Dans le cas présent, le candidat idéal serait notre _protocole_, à savoir le module `sample`. 

Pour donner à un module les caractères d'un `behavior`, il suffit de lui ajouter la fonction `behaviour_info/1`. Cette fonction devra, dans le cas où elle est appelée avec, en paramètre, l'atome `callbacks`, renvoyer la liste des fonctions, et leur arités, que doit implémenter le module souhaitant _hériter_ de ce comportement. Cette liste devra être renvoyé sur forme de liste de clé/valeur, la clé étant un atome donnant le nom de la fonction, et la valeur son arité.

Nous modifions donc le module `sample` de la façon suivante :

`sample.erl` :
```
-module(sample).
-export([behaviour_info/1]).
-export([mul/2]).

behaviour_info(callbacks) -> [{new, 1}, {mul, 2}];
behaviour_info(_) -> undefined.

mul(A, B) -> 
  ok. % TODO
```

Si maintenant nous voulons que le module `sample_int` ait le comportement `sample`, il suffit de l'indiquer :

`sample_int.erl` :
```
-module(sample_int).
-behaviour(sample).
-export([mul/2]).

mul(A, B) -> 
  A * B.
```

Imaginons que dans le code de `sample_int.erl` nous omettons de créer la fonction `mul/2`, dans ce cas, lors de la compilation, nous recevrons un message d'avertissement :

```
sample_int.erl:2: Warning: undefined callback function mul/2 (behaviour 'sample')
```

Il s'agit, comme c'était le cas avec Elixir, d'un simple _warning_.

Nous avons maintenant besoin que, lorsque nous appelons la fonction `mul/2` du _protocole_, donc du module `sample` que ce dernier _déroute_ l'appel vers la fonction d'implémentation. Dans le cas ou nous faisons cet appel avec des entiers, nous avons besoin que la fonction `sample_int:mul/2` soit appelé. Pour faire cela, il faudrait pouvoir récupérer le type des paramètres. Nous pourrions écrire une fonction pour cela, en utilisant les fonctions `is_*` du module `erlang`. Mais il faudrait également avoir un mécanisme qui nous permette de retrouver pour un type donné le module d'implémentation correspondant. Imaginons donc que l'appel à la fonction `sample:mul/2` se fasse avec, pour chaque paramètre, la donnée et l'implémentation à laquelle elle se raccroche, sous la forme `{impl_module, Data}`. Dans le cas d'un entier, nous aurions donc `{sample_int, Value}`. Dans ce cas, si dans `sample:mul/2` le module d'implémentation est le même pour les deux paramètres, nous pouvons appeler la fonction équivalente dans le module d'implémentation. Notre protocole a alors le code suivant :

`sample.erl` :
```
-module(sample).
-export([behaviour_info/1]).
-export([mul/2]).

behaviour_info(callbacks) -> [{new, 1}, {mul, 2}];
behaviour_info(_) -> undefined.

mul({Impl, A}, {Impl, B}) -> 
  Impl:mul(A, B).
```

Pour que cela fonctionne, nous ne passons donc plus directement les valeurs des entiers, mais des tuples. Nous pouvons faire créer ces tuples dans l'implémentation :

`sample_int.erl` :
```
-module(sample_int).
-behaviour(sample).
-export([new/1, mul/2]).

new(Data) ->
  {?MODULE, Data}.

mul(A, B) ->
  A * B.
```

Nous pouvons maintenant utiliser notre protocole de la façon suivante :

```
1> c(sample).
{ok,sample}
2> c(sample_int).
{ok,sample_int}
3> A = sample_int:new(3).
{sample_int,3}
4> B = sample_int:new(4).
{sample_int,4}
5> sample:mul(A, B).
12
```

Nous avons une solution qui n'est malheureusement pas aussi pratique et souple que celle proposée par Erlang. Dans le cas où nous souhaitons utiliser des types simples, elle ne présente pas d'intérêt ; et au contraire, elle peut être plus contraignante dans certains cas. Cependant, dans le cas de structures plus complexes, elle peut avoir un intérêt. Par exemple, nous pouvons écrite une implémentation pour gérer des matrices :

`sample_matrix.erl`
```
-module(sample_matrix).
-behaviour(sample).
-export([new/1, mul/2]).

new(Data) ->
  {?MODULE, Data}.

mul(M1, M2) ->
  Valid = length(M1) =:= length(lists:nth(1, M2)),
  if
    Valid ->
      create(length(M1), length(lists:nth(1, M2)), fun(C, R, _, _) ->
            sum_(row(M1, R), col(M2, C), 0)
        end);
    true ->
      {error, invalid_sizes}
  end.

row(M, N) ->
  lists:nth(N, M).

col(M, N) ->
  [lists:nth(N, R) || R <- M].

create(Columns, Rows, ContentGenerator) ->
  [[ContentGenerator(Column, Row, Columns, Rows)
    || Column <- lists:seq(1, Columns)]
   || Row <- lists:seq(1, Rows)].

sum_([], [], S) -> S;
sum_([E1|L1], [E2|L2], S) ->
  sum_(L1, L2, S+(E1*E2)).
```

Avec ce nouveau module, nous pouvons maintenant faire ceci :

```
6> c(sample_matrix).
{ok,sample_matrix}
7> M1 = sample_matrix:new([[1, 2, 0], [4, 3, -1]]).
{sample_matrix,[[1,2,0],[4,3,-1]]}
8> M2 = sample_matrix:new([[5, 1], [2, 3], [3, 4]]).
{sample_matrix,[[5,1],[2,3],[3,4]]}
9> sample:mul(M1, M2).
[[9,7],[23,9]]
10> sample:mul(M2, M1).
[[9,13,-1],[14,13,-3],[19,18,-4]]
```

Si maintenant nous voulons faire une multiplication entre un entier et une matrice, nous pouvons très simplement modifier notre protocole. En effet, pour le moment il ne permet de multiplier entre eux que des types identiques.

Pour faire cela, nous allons introduire une nouvelle notion : les _stateful modules_.

Un module _stateful_ nous permet de faire ceci :

```
1> StatefulModule = object:new(Data).
2> StatefulModule:function(param).
```

Donc nous faisons un appel de fonction directement sur le résultat d'un autre appel de fonction.

Pour que cela fonctionne, le code de notre module sera le suivant :

```
-module(stateful).
-export([new/1, function/2]).

new(Data) -> {?MODULE, Data}.

function(Param, {?MODULE, Data}) ->
  % Faire quelque chose avec Param et Data
  ok.
```

La fonction de création est relativement simple, et doit renvoyer un tuple dont le premier élément est un atome correspondant au nom du module. En retour nous aurons, dans `StatefulModule` le tuple `{stateful, Data}`. Par la suite l'appel `StatefulModule:function(param)` revient donc à écrire `{stateful, Data}:function(param)`. En voyant ce type d'appel, Erlang va automatiquement le transformer en un appel de fonction sous la forme : `stateful:function(param, {stateful, Data})`. Soit, il va récupérer le premier élément du tuple qu'il considérera comme le nom d'un module pour lequel il fera un appel à la fonction demandée, en lui passant en dernier paramètre le tuple.

Maintenant que nous connaissons ce principe, nous pouvons l'utiliser pour réécrire notre _protocole_ et les implémentations.

Le code du protocole devient alors le suivant :

`sample.erl` :
```
-module(sample).

-export([behaviour_info/1]).
-export([new/2, mul/2]).

behaviour_info(callbacks) -> [{new, 1}, {mul, 2}];
behaviour_info(_) -> undefined.

new(Module, Data) ->
  {Module, Data}.

mul(A, B) ->
  A:mul(B).
```

Donc quand nous ferons un appel de la fonction `sample:mul/2` cette fonction renverra en résultat le résultat de l'appel de la fonction `mul/2` du module avec lequel aura été créé `A`. Donc si `A` est une matrice et `B` un entier, `sample:mul(A,B)` fera un appel `{sample_matrix, M}:mul({sample_int, I})` correspondant donc à l'appel `sample_matrix:mul({sample_int, I}, {sample_matrix, M})`.

Nous pouvons maintenant passer aux implémentations. Dans le cas des entiers, nous écrirons ceci :

`sample_int.erl` :
```
-module(sample_int).
-behaviour(sample).

-export([new/1, mul/2]).

new(Data) ->
  {?MODULE, Data}.

mul({?MODULE, B}, {?MODULE, A}) ->
  new(A * B);
mul({sample_matrix, _} = B, {?MODULE, _} = A) ->
  B:mul(A).
```

Nous avons modifié la fonction `sample_int:mul/2` en prenant en compte le produit de deux entiers, mais également le produit d'un entier par une matrice. Dans ce dernier cas, nous nous sommes contentés d'appeler la fonction de multiplication d'une matrice par un entier du module `sample_matrix`. Ce choix est simplement dû au fait que ce produit donne une matrice en retour. Nous avons également modifié la sortie de telle sorte que nous recevions en retour un tuple dont le premier élément est un atome du nom du module type. Ainsi le produit de deux entiers donnera comme résultat le tuple `{sample_int, Resultat}`. Nous ferons bien entendu la même chose avec les matrices.

L'implémentation du module `sample_matrix` devient le suivant :

`sample_matrix.erl` :
```
-module(sample_matrix).
-behaviour(sample).
-export([new/1, mul/2]).

new(Data) ->
  {?MODULE, Data}.

mul({?MODULE, M2}, {?MODULE, M1}) ->
  Valid = length(M1) =:= length(lists:nth(1, M2)),
  if
    Valid ->
      new(create(length(M1), length(lists:nth(1, M2)), fun(C, R, _, _) ->
            sum_(row(M1, R), col(M2, C), 0)
          end));
    true ->
      {error, invalid_sizes}
  end;
mul({sample_int, N}, {?MODULE, M}) ->
  new([[E*N || E <- L] || L <- M]).

row(M, N) ->
  lists:nth(N, M).

col(M, N) ->
  [lists:nth(N, R) || R <- M].

create(Columns, Rows, ContentGenerator) ->
  [[ContentGenerator(Column, Row, Columns, Rows)
    || Column <- lists:seq(1, Columns)]
   || Row <- lists:seq(1, Rows)].

sum_([], [], S) -> S;
sum_([E1|L1], [E2|L2], S) ->
  sum_(L1, L2, S+(E1*E2)).
```

Il y a ici peu de changement, si ce n'est que, comme dans le cas des entiers, nous avons pris en compte le produit d'une matrice par un entier.

Avec ces modifications, nous sommes maintenant capables de réaliser les opérations suivantes :

```
1> N1 = sample_int:new(2).
{sample_int,2}
2> N2 = sample_int:new(3).
{sample_int,3}
3> M1 = sample_matrix:new([[1, 2, 0], [4, 3, -1]]).
{sample_matrix,[[1,2,0],[4,3,-1]]}
4> M2 = sample_matrix:new([[5, 1], [2, 3], [3, 4]]).
{sample_matrix,[[5,1],[2,3],[3,4]]}
5> sample:mul(N1, N2).
{sample_int,6}
6> sample:mul(M1, M2).
{sample_matrix,[[9,7],[23,9]]}
7> sample:mul(N1, M1).
{sample_matrix,[[2,4,0],[8,6,-2]]}
8> sample:mul(M1, N1).
{sample_matrix,[[2,4,0],[8,6,-2]]}
9> sample:mul(M1, N2).
{sample_matrix,[[3,6,0],[12,9,-3]]}
10> sample:mul(sample:mul(M1, M2), sample:mul(N1, N2)).
{sample_matrix,["6*",[138,54]]}
11> $6.
54
11> $*.
42
```

Le résultat donné en ligne 10 est normal. En effet, souvenez-vous de ce que nous avons vu au chapitre 3. Si une liste d'entier ne contient que des valeurs correspondant à des caractères imprimables, Erlang renverra cette liste sous forme d'une chaine de caractères. Donc `"6*"` est à interpréter ici comme correspondant à la liste `[54, 42]`.
