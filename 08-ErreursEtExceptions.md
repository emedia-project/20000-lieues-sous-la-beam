# Gestion des erreurs

Bien qu'Erlang et Elixir aient été designés de façon à permettre la mise en place de programmes ayant une grande tolérance aux pannes, il arrivera toujours un moment ou nous devons mettre en place des protections pour prendre en compte les erreurs pouvant subvenir lors du traitement de données. Ce travail est crucial et doit donc être mené avec beaucoup d'attention afin de s'assurer que nous pourrons rendre le meilleur service possible à nos utilisateurs en cas d'erreur. 

La gestion des erreurs est donc un élément fondamental. De plus avec Erlang et Elixir, nous devons aborder ce problème sous deux angles. Dans le premier cas, nous devons voir comment traiter les problèmes pouvant intervenir dans un traitement séquentiel, donc au sein d'un unique processus. Mais nous avons vu au chapitre 6 qu'une application Erlang est très souvent composée de plusieurs processus. Nous en avons profité pour voir comment se comporte une application dans le cas ou l'un de ces processus _planterais_ et les conséquences que cela engendrerait. Dans ce chapitre, nous allons revenir au début et voir comment traiter les erreurs en restant au sein d'un même processus. Mais nous reviendrons sur ce sujet au chapitre 10 pour voir comment le traiter de manière plus globale.

## _ok_ ou _error_

Vous avez probablement remarqué, dans les nombreux exemples des chapitres précédents, que très souvent nous avons des fonctions qui ont un retour sous forme de tuple dont le premier élément est un atome indiquant si le résultat renvoyé est conforme à ce que nous somme en droit d'attendre (`ok`) ou non (`error`).

Nous utilisons ce principe pour traiter des cas simples. Par exemple, nous pouvons, dans certains cas, ne pas pouvoir utiliser de _guard_ pour valider que les paramètres passés à une fonction sont conformes à ce qui est attendu par la fonction. Souvenez vous, par exemple, de ce que nous avons écrit pour traiter la multiplication la multiplication de matrice au chapitre 7 :

```
mul(M1, M2) ->
  Valid = length(M1) =:= length(lists:nth(1, M2)),
  if
    Valid ->
      ...
    true ->
      {error, wrong_sizes}
  end.
```

Dans cet exemple, nous renvoyons le résultat de la multiplication sous la forme `{sample_matrix, [...]}` ou `{error, wrong_sizes}` si les tailles des deux matrices ne permettaient pas de faire la multiplication.

Si vous parcourez la documentation d'Erlang, vous pourrez trouver de nombreux exemples utilisant ce principe. 

Bien entendu, ce principe n'est pas réservé à Erlang, et Elixir l'utilise également. Cependant, dans ce dernier, il n'est pas rare de trouver une fonction renvoyant un résultat sous forme de tuple (`{:ok, ...}`, `{:error, ...}`) et la même fonction terminée par un point d'exclamation (`!`), effectuant le même travail, mais levant une exception en cas de problème.  C'est par exemple les cas des fonctions `File.read/1` et `File.read!/1`. L'intérêt des fonctions notées avec un point d'exclamation fait que nous n'aurons pas besoin de _traiter_ leur retour. Cependant, nous serons dans certains cas obligés de traiter la levée d'exception tout en gardant, dans certains cas, la possibilité de laisser cette exception _remonter_, diminuant ainsi la quantité de code.

## Les exceptions

Lorsque nous développons, nous utilisons très souvent un mode de programmation défensive dans laquelle nous essayons de prévoir _tous_ les cas d'erreurs possibles afin de pouvoir lever une exception. Avec Erlang (et Elixir) ce principe est inclus dans le langage. Ainsi, imagions une fonction pouvant recevoir un ensemble fixé de paramètres. Avec votre langage favori, vous aurez tendance a écrire votre fonction en levant une exception si le paramètre passé ne fait pas partie de ceux que la fonction est capable de traiter. En Ruby, par exemple, nous pourrions ainsi écrire quelque chose comme ceci :

```
def f(var)
  case var
  when "one" then 1
  when "two" then 2
  when "three" then 3
  else raise "Error"
  end
end
```

Avec Erlang, nous écrirons le même code de la façon suivante :

```
f("one") -> 1;
f("two") -> 2;
f("three") -> 3.
```

Si par la suite nous essayons d'appeler la fonction `f` avec un paramètre invalide, nous obtenons le résultat suivant : 

```
2> f("four").
** exception error: no function clause matching call to f/1
```

Il en est de même avec Elixir :

```
defmodule Sample do
  def f('one'), do: 1
  def f('two'), do: 2
  def f('three'), do: 3
end
```

```
iex(2)> Sample.f('four')
** (FunctionClauseError) no function clause matching in Sample.f/1
     sample.ex:2: Sample.f('four')
```

L'exception est levée **par le système**. 

Il se peut, dans certaines conditions, que nous souhaitions lever nous même une exception. Pour ce faire nous avons à notre disposition plusieurs fonctions.

### _throw_

`throw` est utilisé pour lever une exception qui sera interceptée par l'appelant de la fonction dans laquelle elle se produit. Si l'exception n'est pas interceptée dans la fonction appelante, tout le code suivant l'appel sera ignoré, et l'exception sera propagée.

La fonction `throw`, qui prend en paramètre un terme quelconque, sera préférentiellement utilisée pour traiter une sortie _anticipé_, dans le cas il n'existe pas d'autre possibilité. Ces cas sont bien entendu relativement rares, mais `throw` pourra nous aider si nous utilisons un module qui n'aurait pas prévu une situation particulière. 

Nous pouvons prendre comme exemple une solution d'implémentation de la fonction `Enum.find/2` d'Elixir en Erlang :

```
-module(enum).
-export([find/2]).

find(Fun, Element) ->
  try
    lists:foreach(fun(E) ->
          case Fun(E) of
            true -> throw(E);
            _ -> ok
          end
      end, List),
    {error, not_found}
  catch
    throw:X -> {ok, X}
  end.
```

```
1> enum:find(fun(E) -> E =:= 8 end, [1, 2, 3, 5, 8, 13]).
{ok,8}
1> enum:find(fun(E) -> E =:= 7 end, [1, 2, 3, 5, 8, 13]).
{error,not_found}
```

### _error_ / _raise_

`error` en Erlang, ou `raise` en Elixir, est utilisé pour signifier des erreurs irréversibles. C'est ce type d'exception qui est utilisé par le système.

Avec Erlang, l'utilisation de la fonction `error` est identique à ce que nous avons vu pour `throw`. Donc en passant à la fonction un paramètre de type quelconque. Avec Elixir, il y a une petite différence. En effet, `raise` prend en paramètre une chaine de caractère correspondant à un message :

```
iex(1)> raise "error"
** (RuntimeError) error
```

Comme le montre l'exemple ci-dessus, Elixir renvoi une exception de type `RuntimeError` avec le message error. Pour nous en convaincre, nous pouvons écrire ceci :

```
iex(2)> try do
...(2)> raise "error"
...(2)> rescue
...(2)> e in RuntimeError -> e.message
...(2)> end
"error"
``` 

Elixir nous autorise donc à créer nos propres exceptions en utilisant `defexception` :

```
iex(3)> defexception MonException, message: "message par defaut"
nil
iex(4)> raise MonException
** (MonException) message par defaut
iex(4)> raise MonException, message: "mon message"
** (MonException) mon message
```

### _exit_

`exit` est généralement utilisé dans le cas ou nous souhaitons mettre fin immédiatement au processus courant. Si l'exception n'est pas capturée dans le processus courant, alors le message `{'EXIT', Raison}` est envoyé à l'ensemble des processus auquel il est rattaché.

L'appel `exit` se fait en passant en paramètre un terme de n'importe quel type, sur le même modèle que `throw`.

Nous reviendrons plus spécifiquement sur `exit` au chapitre suivant.

### Alors, exception ou _error_ ?

Après avoir vu les différentes méthodes de levée d'exception, nous sommes en droit de nous demander dans quel cas nous allons utiliser une exception plutôt qu'un retour sous forme de tuple ?

Dans la pratique ce choix vous appartient. Mais il est courant de trouver dans le code d'Erlang un retour via un tuple dans le cas ou le risque d'erreur et fréquent, laissant les exceptions au cas les plus rares et les plus critiques. Cependant ce choix est discutable, d'autant plus que nous ne pouvons pas toujours maitriser l'utilisation qui sera faite de notre code par d'autres développeurs. 

Afin d'éviter d'entrer dans ce débat, Elixir a pris le parti d'offrir les deux possibilités en proposant pour certaines fonctions deux versions, l'une renvoyant un tuple et l'autre (marqué d'un point d'exclamation) une exception. Ce choix à l'avantage de proposer le meilleur des deux mondes. En effet, pour la lecture de fichier par exemple, Elixir propose `File.read/1` et `File.read!/1`. Si nous écrivons un programme qui lit un fichier qui _doit_ exister, nous utiliserons préférentiellement `File.read!/1` alors que si l'existence du fichier est incertaine, nous adopterons plutôt l'autre version.

## _try_ ... _catch_ ...

### Erlang

Pour le traitement des exceptions, Erlang utilise une construction très semblable à ce que l'on retrouve dans la plupart des langages :

```
try
  ...
catch
  ...
finaly
  ...
end
```

Entre `try` et `catch` nous placerons le code devant être _surveillé_. Entre `catch` et `finaly` nous spécifierons les exceptions devant être capturées. Entre `finaly` et `end` nous placerons le code devant être exécuté _quoi qu'il arrive_. 

> `finaly` est optionnel.

Concernant le code à surveiller, il n'y a aucune subtilité particulière :

```
try 
  io:format("Hello!~n"),
  throw(error),
  io:format("Cette phrase ne sera jamais affichée~n")
catch
  ...
end.
```

Concernant la partie `catch` nous spécifierons les exceptions à capturer sous la forme `Type:Message`. `Type` correspondant à une des trois formes que nous avons vues au paragraphe précédent (`throw`, `error` ou `exit`) et `Message` au message envoyé par l'exception. Le principe du _pattern matching_ étant conservé, nous pouvons être spécifiques lors de la récupération des exceptions :

```
try
  ...
catch 
  throw:type_error -> io:format("[Error] mauvais type~n");
  _:internal_error -> io:format("[Error] erreur interne~n");
  exit:_           -> io:format("exit!~n");
  _:_              -> io:format("[Error] Quelque chose va mal!~n")
end
```

Nous pouvons bien entendu récupérer le type et le message dans des variables :

```
1> try
1>   io:format("Hello!~n"),
1>   throw(error),
1>   io:format("Cette phrase ne sera jamais affichée~n")
1> catch
1>   Type:Message -> io:format("Exception de type ~p avec le message ~p~n", [Type, Message])
1> end.
Hello!
Exception de type throw avec le message error
```

Il existe une autre forme d'écriture pour `try`, basé sur le même modèle que `case` ; donc en permettant de traiter les différents patterns de retour possible.

```
try f() do
  Pattern1 -> ... ;
  ...
  PatternN -> ...
catch
  ...
end
```

Pour illustrer cela, prenons le code suivant :

```
-module(mathematic).
-export([pair_impair/1, pair/1]).

pair_impair(N) when is_integer(N) ->
  if
    0 =:= N rem 2 -> {pair, N};
    true -> {impair, N}
  end.

pair(X) ->
  try pair_impair(X) of
    {pair, _} -> true;
    {impair, _} -> false
  catch
    error:Message -> {error, Message}
  end.
```

Nous avons donc une fonction `pair_impair/1` qui va renvoyer `{pair, N}` si `N` est pair et `{impair, N}` si `N` est impair. Nous avons également la fonction `pair/1` qui renverra `true` ou `false` si le paramètre passé est pair ou non. Dans cette fonction, nous traitons l'exception dans le cas ou le paramètre passé n'est pas un entier et nous traitons le retour de la fonction `pair_impair/1` de la même manière qu'avec un `case`. Tout comme avec ce denier, nous pouvons utiliser des _guards_ en complément des patterns.

> Notez bien que le code donné en exemple ne correspond pas à ce que nous _aurions_ écrit dans un cas réèl. En effet, il aurait été préférable de coder la fonction `pair` de la façon suivante :
> 
>     pair(X) when is_integer(X) ->
>       case pair_impair(X) of
>         {pair, _} -> true;
>         {impair, _} -> false
>       end

Si nous n'utilisons pas la forme `try ... do`, nous recevrons directement la valeur retournée par une fonction traitée par try :

```
try 
  pair_impair(X)
catch
  error:_ -> {error, not_an_integer}
end
```

En sortie de ce bloc, nous pourrons donc avoir les résultats suivants :

* `{pair, X}` si `X` est pair.
* `{impair, X}` si `X` est impair.
* `{error, not_an_integer}` si X n'est pas un entier.

<!-- erlang:get_stacktrace() -->

### Elixir

Elixir utilise une construction similaire à celle d'Erlang, avec cependant trois petites différences :

* `try` est bien un bloc.
* `finaly` est remplacé par `after`.
* La capture des exceptions levée via `raise` _peut_ se fait dans un _bloc_ `rescue` ou dans un _bloc_ `catch`.

La forme d’écriture est donc la suivante :

```
try do
  ...
catch
  ...
rescue
  ...
after
  ...
end
```

La distinction entre `catch` et `rescue` vient du fait que pour récupérer les exceptions, de type `throw` et `exit`, Elixir utilise le même principe de matching qu'Erlang :

```
try do
  ...
catch 
  throw, type_error -> IO.puts("[Error] mauvais type")
  _, internal_error -> IO.puts("[Error] erreur interne")
  exit:_            -> IO.puts("exit!")
  _, _              -> IO.puts("[Error] Quelque chose va mal!")
end
```

Or, comme nous l'avons vu, dans le cas de `raise`, Elixir utilise un principe de définition d'exception. Donc la capture dans le _bloc_ `catch` ne peut pas se faire sous la forme `type, message`. Nous devrons utiliser un matching permettant de capturer la définition :

```
try do
  raise "error"
rescue 
  e -> IO.puts("Raise: #{e.message}")
end
```

L'exécution de ce code nous renverra bien le message `Raise: error`, et nous pourrions nous en contenter, cependant, dans certains cas, il pourrait être utilise de pouvoir traiter spécifiquement une exception de type `raise`. C'est là qu'intervient le bloc `rescue` que nous utiliserons alors de la façon suivante :

```
try do
  ...
rescue
  e in RuntimeError -> IO.puts("[RuntimeError] #{e.message}")
  e in MyError -> IO.puts("[MyError] #{e.message}")
  e -> IO.puts("[???] #{e.message}")
end
```

Nous pouvons également traiter les exceptions par groupe :

```
try do
  ...
rescue
  e in [RuntimeError, MyError] -> IO.puts("[Runtime or My Error] #{e.message}")
  e -> IO.puts("[???] #{e.message}")
end
```
