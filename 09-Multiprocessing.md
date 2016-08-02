# Programmation concurrente

...

## _Hello Joe!_ ... _Hello Mike!_ ... _Hello Robert!_ ... _Hello Joe!_[^ERLANG_MOVIE]

Au cours des chapitres précédents, j'ai souvent utilisé le terme de _processus_ sans jamais en donner une définition précise. Cependant, c'est un terme qui ne doit pas vous être étrangé, mais que vous avez plus souvent l'habitude d'utiliser quand vous pensez _au niveau système_. Et bien un processus dans la BEAM peut être vu de la même manière. Il s'agit d'un ensemble d'instructions qui vont être executés de façon séquentielle, indépendament des autres processus. Quand nous démarrons un shell, nous somme dans un processus interactif, qui attend donc que nous lui donnions les intruction à executer. Ce processus possède un identifiant que l'on peut récupérer via la commande `self/0`.

Erlang :
```
1> self().
<0.32.0>
```

Elixir :
```
iex(1)> self
#PID<0.41.0>
```

Le résultat renvoyé par `self` est l'identifiant du processus courant, son `Pid`. Grace à lui nous allons pouvoir envoyer des messages à notre processus. Avec Erlang, nous enverrons un message en utilisant l'écriture :

```
Pid ! Message
```

Avec Elixir nous utiliserons la fonction `Kernel.send/2` :

```
send pid, message
```

Le message envoyé peut être de n'importe quel type.

Pour qu'un message puisse être interprété, il faut que le processus à qui il est envoyé soit _à l'écoute_ ; si ce n'est pas le cas, le message sera placé dans une queue en attente d'être lu. Si un processus s'arrète avant d'avoir _consommé_ tous les messages, ces deniers seront _perdus_. Pour récupérer un message, dans un processus, nous utilisons `receive` :

Erlang :
```
1> self() ! {message, "Hello World!"}.
{message,"Hello World!"}
2> receive
2> M -> io:format("Message reçu : ~p~n", [M])
2> end.
Message reçu : {message,"Hello World!"}
ok
```

Elixir :
```
iex(1)> send self, {:message, 'Hello World!'}
{:message, 'Hello World!'}
iex(2)> receive do
...(2)> m -> IO.puts "Message reçu : #{Macro.to_string(quote do: unquote(m))}"
...(2)> end
Message reçu : {:message, 'Hello World!'}
:ok
```

`receive` permet de récupérer le premier message dans la queue, qui est lue selon le mode FIFO. `receive` permet de récupérer un unique message, il faudra donc l'executer autant de fois que nécessaire. Si jamais il n'y a aucun message dans la queue, `receive` mettra le processus en attente jusqu'à ce qu'un nouveau message soit envoyé au processus.

Ce que nous avons vu est unidirectionnel, un processus _A_ envoie un message à un processus _B_. Et vous aurez noté que dans le bloc `receive` nous n'avons aucune information sur l'expéditeur. L'identifiant de l'expéditeur doit donc être envoyé explicitement dans le message, faute de quoi il n'y aura pas de réponse possible. Le réponde, se fera alors en envoyant un message en utilisant le `Pid` envoyé.

[^ERLANG_MOVIE]: [https://www.youtube.com/watch?v=xrIjfIjssLE](https://www.youtube.com/watch?v=xrIjfIjssLE)

## Création de processus

Jusqu'à présent nous nous sommes contenté de nous envoyer des messages _à nous même_. Pour créer de nouveau processus, nous utiliserons la fonction `spawn` à laquelle nous passerons :

* Soit la fonction d'un module donné avec sa liste de paramètre.
* Soit une fonction anonyme.

`spawn` nous renvoie le `Pid` du processu créé. C'est donc lui que nous utiliserons pour envoyer les messages :

Erlang :
```
1> Child = spawn(fun() ->
1> io:format("Je suis un nouveau processus, j'attend notre message...~n"),
1> receive
1> M -> io:format("Message reçu : ~p~n", [M])
1> end
1> end).
Je suis un nouveau processus, j'attend notre message...
<0.34.0>
...
3> Child ! {self(), "Je suis ton père!"}.
Message reçu : {<0.32.0>,"Je suis ton père!"}
{<0.32.0>,"Je suis ton père!"}
```

Nous avons donc, depuis le processus `<0.32.0>` crée un processus dont le `Pid` est stocké dans `Child`, et auquel nous avons envoyé le message `{self(), "Je suis ton père!"}`. Après avoir reçu ce message, le processus fils se termine.

Nous pouvons faire exactement la même chose avec Elixir :

```
iex(1)> child = spawn(fn() ->
...(1)> IO.puts "Je suis un nouveau processus, j'attend notre message..."
...(1)> receive do
...(1)> m -> IO.puts "Message reçu : #{Macro.to_string(quote do: unquote(m))}"
...(1)> end
...(1)> end)
Je suis un nouveau processus, j'attend notre message...
#PID<0.54.0>
...
iex(3)> send child, {self, 'Je suis ton père!'}
Message reçu : {#PID<0.41.0>, 'Je suis ton père!'}
{#PID<0.41.0>, 'Je suis ton père!'}
iex(4)> Process.info(child)
nil
```

Bien que j'ai nommé le processus créé comme étant le _fils_ il ne faut pas se méprendre, une fois le processus créé, il n'existe (dans le cas présent) plus aucun lien entre les deux, si ce n'est que le _père_ a créé le _fils_ et peut donc être le seul a posséder son identifiant. Mais à part cela, ils sont totalement indépendant. Et si le père venait à se terminer, cela ne mettrait pas fin au processus fils. Dans le pire des cas, le `Pid` du fils serait perdu.

Pour illustrer cela, nous pouvons mettre en place un exemple dans lequel un processus _père_ créé deux processus _fils_ et se termine. Comme nous pourrons le voir, les deux _fils_ continuerons à communiquer.

Pour cet exemple, le processus père sera relativement simple. Il va simplement _spawner_ deux processus puis envoyer au premier l'identifiant du second sous la forme `{brother, Pid}`.

`father.erl` :
```
-module(father).

-export([run/0]).

run() ->
  io:format("[~p] Je suis le processus père.~n", [self()]),
  Child1 = spawn(child_one, create, []),
  Child2 = spawn(child_two, create, []),
  io:format("[~p] J'ai créé deux processus : ~p et ~p~n", [self(), Child1, Child2]),
  io:format("[~p] Puis je passe l'identifiant de ~p à ~p~n", [self(), Child2, Child1]),
  Child1 ! {brother, Child2},
  io:format("[~p] J'ai terminé, je peux mourir!~n", [self()]).
```

Une fois créé, le processus `child_one` va démarrer une boucle d'attente afin d'intercepter les messages qui pourraient lui être envoyés. Il sera en mesure de traiter trois types de messages :

* `{brother, Pid}` : s'il reçoit ce message, il enverra un message `{hello, self()}` au processus ayant l'identifiant `Pid`, donc au processus `child_two`.
* `{count, N, Pid}` : En recevant ce message, il affichera la valeur de `N` et renverra à `Pid` une message `{count, N+1, self()}`.
* `bye` : En recevant ce message, `child_one` met fin à sa boucle d'attente et se terminera. Pour cela, contrairement aux deux cas précédents, il suffit de ne pas relancer la boucle, donc de ne pas rappeler la fonction `loop()`.

`child_one.erl` :
```
-module(child_one).
-export([create/0]).

create() ->
  io:format("[~p] Je suis ~p, je viens de naitre!~n", [self(), ?MODULE]),
  io:format("[~p] Je me place en attente de messages...~n", [self()]),
  loop().

loop() ->
  receive
    {brother, Pid} ->
      io:format("[~p] Je viens de recevoir le PID ~p~n", [self(), Pid]),
      io:format("[~p] Je lui envoi un message de bienvenu avec mon PID~n", [self()]),
      io:format("[~p] Puis je me remet en attente de message~n", [self()]),
      Pid ! {hello, self()},
      loop();
    {count, N, Pid} ->
      io:format("[~p] ~p...~n", [self(), N]),
      timer:sleep(2000),
      Pid ! {count, N+1, self()},
      loop();
    bye ->
      io:format("[~p] Bye bye!~n", [self()])
  end.
```

Le processus `child_two`, développé sur le même modèle que `child_one` traitera les messages suivants :

* `{hello, Pid}` : Une fois ce message reçu, `child_two` démarrera un échange consistant à compter jusqu'à dix entre lui et `child_one`. Pour cela il enverra à `Pid` le message `{count, 1, self()}`.
* `{count, N, Pid}` : Ce message sera traité de la même manière que le fait `child_one`. Il y a cependant une petite différence. En effet, si le message reçu est `{count, 10, Pid}`, alors `child_two` mettra fin à sa boucle d'attente en envoyant au préalable le message `bye` à `Pid`.

`chold_two.erl` :
```
-module(child_two).
-export([create/0]).

create() ->
  io:format("[~p] Je suis ~p, je viens de naitre!~n", [self(), ?MODULE]),
  io:format("[~p] Je me place en attente de messages...~n", [self()]),
  loop().

loop() ->
  receive
    {hello, Pid} ->
      io:format("[~p] Je viens de recevoir le PID ~p~n", [self(), Pid]),
      io:format("[~p] Nous allons compter chacun notre tour~n", [self()]),
      io:format("[~p] jusqu'à 10 en faisant un pause de 2 seconds~n", [self()]),
      io:format("[~p] entre chaque compte~n", [self()]),
      Pid ! {count, 1, self()},
      loop();
    {count, 10, Pid} ->
      io:format("[~p] Et 10 !~n", [self()]),
      io:format("[~p] J'envoie un message à ~p pour lui dire de se terminer.~n", [self(), Pid]),
      io:format("[~p] Et moi même je me termine.~n", [self()]),
      Pid ! bye;
    {count, N, Pid} ->
      io:format("[~p] ~p...~n", [self(), N]),
      timer:sleep(2000),
      Pid ! {count, N+1, self()},
      loop()
  end.
```

> Afin d'éviter un déroulement trop rapide des échanges, j'ai placé une pause de 2 secondes entre le moment ou un processus reçoit un message `{count, N, Pid}` et le moment ou il y _répond_.

Voici un schéma qui symbolise les échanges entre les trois processus :

![](../images/09-father_and_sons.jpg)

Et voilà le résultat de l'execution :

```
1> c(father).
{ok,father}
2> c(child_one).
{ok,child_one}
3> c(child_two).
{ok,child_two}
4> self().
<0.32.0>
5> Father = spawn(father, run, []).
[<0.55.0>] Je suis le processus père.
[<0.55.0>] J'ai créé deux processus : <0.56.0> et <0.57.0>
[<0.56.0>] Je suis child_one, je viens de naitre!
[<0.57.0>] Je suis child_two, je viens de naitre!
<0.55.0>
[<0.55.0>] Puis je passe l'identifiant de <0.57.0> à <0.56.0>
[<0.56.0>] Je me place en attente de messages...
[<0.57.0>] Je me place en attente de messages...
[<0.55.0>] J'ai terminé, je peux mourir!
[<0.56.0>] Je viens de recevoir le PID <0.57.0>
[<0.56.0>] Je lui envoi un message de bienvenu avec mon PID
[<0.56.0>] Puis je me remet en attente de message
[<0.57.0>] Je viens de recevoir le PID <0.56.0>
[<0.57.0>] Nous allons compter chacun notre tour
[<0.57.0>] jusqu'à 10 en faisant un pause de 2 seconds
[<0.57.0>] entre chaque compte
[<0.56.0>] 1...
[<0.57.0>] 2...
6> process_info(Father).
undefined
[<0.56.0>] 3...
[<0.57.0>] 4...
[<0.56.0>] 5...
[<0.57.0>] 6...
[<0.56.0>] 7...
[<0.57.0>] 8...
[<0.56.0>] 9...
[<0.57.0>] Et 10 !
[<0.57.0>] J'envoie un message à <0.56.0> pour lui dire de se terminer.
[<0.57.0>] Et moi même je me termine.
[<0.56.0>] Bye bye!
```

Dans cet exemple, nous démarrons le _père_ dans un processus. En effet, si nous faisons un appel direct à la fonction `father:run/0` nous restons dans le processus du shell, et le _père_ ne pourra donc pas se terminer avant que les _fils_ n'aient terminé leurs échanges, Le seul moyen d'arréter le processus du shell étant de stopper la BEAM.

Lors de l'execution de cet exemple, pendant que les deux processus `child_one` et `child_two` _comptaient_, nous avons executé la commande `erlang:process_info/1` afin de montrer que le processus `father` était terminé.

## Processus nommé

L'un des principale problème, quand on _manipule_ des processus, vient du fait que nous jouons avec des IDs. Et même s'il est possible de _récupérer_ un ID de processus via la fonction `c:pid/3`, cela reste assez fastidieux :

Erlang : 
```
1> spawn(fun() -> receive X -> io:format("Got ~p~n", [X]) end end).
<0.38.0>
5> pid(0,38,0) ! "hello".
Got "hello"
"hello"
```

Elixir :
```
iex(1)> spawn(fn() ->
...(1)> receive do
...(1)> m -> IO.puts "Got #{Macro.to_string(quote do: unquote(m))}"
...(1)> end
...(1)> end)
#PID<0.47.0>
iex(4)> send :c.pid(0,47,0), "Hello"
Got "Hello"
"Hello"
```

De plus, la récupération de l'ID n'est possible que si nous connaissons ce que nous devons récupérer. 

Pour résoudre ce problème, il est possible d'enregistrer un processus en lui donnant un nom, via la fonction `erlang:register/2` d'Erlang ou `Process.register/2` d'Elixir :

Erlang :
```
1> Pid = spawn(fun() -> receive X -> io:format("Got ~p~n", [X]) end end).
<0.34.0>
2> register(process, Pid).
true
3> spawn(fun() -> process ! "Hello" end).
Got "Hello"
<0.37.0>
```

Nous avons ici créé un processus d'ID `<0.34.0>` que nous avons enregstré sous le nom `process`. Puis, via un second processus, nous lui avons envoyé un message.

Avec Elixir, nous faisons la même chose de la façon suivante :

```
iex(1)> pid = spawn(fn() ->
...(1)> receive do
...(1)> m -> IO.puts "Got #{Macro.to_string(quote do: unquote(m))}"
...(1)> end
...(1)> end)
#PID<0.47.0>
iex(2)> Process.register(pid, :process)
true
iex(3)> spawn(fn() -> send(:process, "Hello") end)
Got "Hello"
#PID<0.50.0>
```

Si nous avons besoin de récupérer l'identifiant d'un processus enregistré, nous pourrons utilisé `erlang:whereis/1` (ou `Process.whereis/1` pour Elixir) en lui passant en paramètre l'atome correspondant au nom du processus enregistré. Il est également possible de _désenregistrer_ un processus via `erlang:unregister/1` (ie `Process.unregister/1`).

## Information sur les processus actifs

Un peu plus haut dans ce chapitre, nous avons utilisé `erlang:process_info/1` afin de vérifier si un processus était terminé. Dans ce cas, cette fonction nous à renvoyé comme réponse : `undefined`. Voyons quel serait le résultat pour un processus toujours vivant :

```
2> process_info(Child).
[{current_function,{prim_eval,'receive',2}},
 {initial_call,{erlang,apply,2}},
 {status,waiting},
 {message_queue_len,0},
 {messages,[]},
 {links,[]},
 {dictionary,[]},
 {trap_exit,false},
 {error_handler,error_handler},
 {priority,normal},
 {group_leader,<0.25.0>},
 {total_heap_size,376},
 {heap_size,376},
 {stack_size,9},
 {reductions,55},
 {garbage_collection,[{min_bin_vheap_size,46422},
                      {min_heap_size,233},
                      {fullsweep_after,65535},
                      {minor_gcs,0}]},
 {suspending,[]}]
```

Avec Elixir, nous obtiendrons le même résultat via la commande `Process.info/1` :

```
iex(2)> Process.info(child)
[current_function: {:prim_eval, :receive, 2},
 initial_call: {:erlang, :apply, 2}, status: :waiting, message_queue_len: 0,
 messages: [], links: [], dictionary: [], trap_exit: false,
 error_handler: :error_handler, priority: :normal, group_leader: #PID<0.25.0>,
 total_heap_size: 986, heap_size: 376, stack_size: 9, reductions: 159,
 garbage_collection: [min_bin_vheap_size: 46422, min_heap_size: 233,
  fullsweep_after: 65535, minor_gcs: 1], suspending: []]
```

Nous ne détaillerons pas l'ensemble des clés, mais nous pouvons tout de même nous attarder sur les suivantes :

* `status` nous donne l'état du processus. Dans le cas présent, la valeur est à `waiting`, ce qui indique que le processus est en attente de message. Si jamais le processus était en train de travailler, la valeur serait `running`.
* `message_queue_len` nous indique le nombre de message qui ont été envoyé mais non traité par le processus.
* `messages` nous donne la liste des messages envoyés mais non traités.
* `links` nous donne la liste des processus auquel le processus courant est _lié_. Nous reviendrons sur ce point en particulier un peu plus loin dans ce chapitre.
* `dictionnary` nous donne le contenu du dictionnaire du processus. En effet, chaque processus possède un dictionnaire dans lequel il peut stocker des valeurs (via `put/2`), les récupérer (via	`get/1`), les effacer (via `erase/1`)... Cependant, sauf dans le cas d'exemples, son utilisation est déconseillé, raison pour laquelle je n'en parlerai pas plus.
* `trap_exit` permet d'indiquer que nous souhaitons que le processus intercepte les signaux de type `exit` sous forme de message. Nous reverrons ce paramétrage un peu plus loin dans ce chapitre.
* `priority` nous donne la priorité du processus entre `low`, `normal`, `high` et `max`. Vous pouvez modifier cette priorité en utilisant la fonction `erlang:process_flag/2` avec le flag `priority`. Attention cependant, car vous risquez d'avoir des comportements innatendu.

En complément de cette fonction, il est interressant de connaitre :

* `erlang:processes()`/`Process.list()` : Donne la liste des `Pid` des processus actifs.
* `erlang:registered()`/`Process.registered()` : Donne la list des nom des processus _enregistrés_.

Avec tout cela, nous somme maintenant en mesure d'écrire un petit code permettant de lister les processus actifs : 

`ps.erl` :
```
-module(ps).
-export([graph/1]).

graph(File) ->
  Links = links(erlang:processes(), []),
  {ok, IO} = file:open(File, [write]),
  write(IO, "graph G {~n"),
  write(IO, "rankdir=LR;~n"),
  write(IO, "splines=ortho;~n"),
  write(IO, "node[shape=box];~n"),
  write(IO, "remincross=true;~n"),
  lists:foreach(fun({N1, N2}) ->
        write(IO, "\"~s\" -- \"~s\"~n", [N1, N2])
    end, Links),
  write(IO, "}"),
  file:close(IO).

links([], Links) -> Links;
links([Pid|Rest], Links) ->
  NodeName = pid_name(Pid),
  NodeLinks = pid_links(Pid),
  links(Rest, add_links(NodeName, NodeLinks, Links)).

add_links(_, [], Links) -> Links;
add_links(Node1, [Node2|Rest], Links) ->
  case found(Node1, Node2, Links) of
    true -> add_links(Node1, Rest, Links);
    false -> add_links(Node1, Rest, Links ++ [{Node1, Node2}])
  end.

pid_name(Pid) ->
  Infos = erlang:process_info(Pid),
  case lists:keyfind(registered_name, 1, Infos) of
    {registered_name, Name} -> Name;
    false -> [Name|_] = io_lib:format("~p", [Pid]), Name
  end.

pid_links(Pid) ->
  Infos = erlang:process_info(Pid),
  case lists:keyfind(links, 1, Infos) of
    {links, Links} -> [pid_name(X) || X <- Links, is_pid(X)];
    false -> []
  end.
  
found(Node1, Node2, Links) ->
  case lists:dropwhile(fun(X) ->
          X =/= {Node1, Node2} andalso X =/= {Node2, Node1}
      end, Links) of
    [] -> false;
    X when length(X) > 0 -> true
  end.

write(IO, Str) ->
  write(IO, Str, []).
write(IO, Str, Args) ->
  file:write(IO, io_lib:format(Str, Args)).
```

Pour utilisez ce script, dans un shell, appelez la fonction `ps:graph/1` avec en paramètre un nom de fichier. En sortie vous aurez donc un fichier _graphviz_[^GRAPHVIZ], qui, une fois compilé, donnera un graphique du type :

![](../images/09-graph.png)

Nous retrouvons ici tous les processus actifs avec, pour ceux qui sont enregistrés, leur nom, et l'ID pour les autres.

Sur ce schéma, il y a des liens entre les processus. Ces liens, obtenus via le contenu du champ `links` de la fonction `erlang:process_info/1` seront expliqué un peu plus loins dans ce chapitre.

Il est également possible d'obtenir la même représentation en utilisant l'application `observer` (onglet _Applications_) d'Erlang. Pour cela, executez `observer:start()` dans un shell.

![](../images/09-observer-graph.png)

> `observer` est un outil écrit avec `wxWidgets`. Pour pouvoir l'utiliser, vous devez donc avoir compilé Erlang avec le support de `wxWidgets`. Pour plus d'informations sur le sujet, référez vous au chapitre 2.

[^GRAPHVIZ]: [http://graphviz.org/](http://graphviz.org/)

## Exemple d'utilisation

Afin de terminer sur un cas concret, voyons le comportement de la BEAM en comparant les résultats de deux executions, l'une séquentielle et l'autre parallèle. Pour cela, nous allons créer un petit programme qui va calculer l'ensemble des MD5 d'une liste de fichiers.

Dans un premier temp, nous allons écrire une fonction qui va travailler séquetiellement :

`para.erl` :
```
hash_seq(Path) ->
  [hash_file(X) || X <- filelib:wildcard(filename:join(Path, "*.md"))].
```

Et nous comparerons le résultat dans le cas d'une execution parallèle :

`para.erl` :
```
hash_para(Path) ->
  X = [spawn(?MODULE, hash_file_para, [self(), X])
       || X <- filelib:wildcard(filename:join(Path, "*.md"))],
  hash_para_loop([], 0, length(X)).

hash_para_loop(Res, Max, Max) -> Res;
hash_para_loop(Res, Num, Max) ->
  receive
    Data ->
      hash_para_loop(Res ++ [Data], Num + 1, Max)
  end.

hash_file_para(From, File) ->
  From ! hash_file(File).
```

Dans la version _parallèle_, nous démarrons autant de processus qu'il n'y a de fichiers et nous attendons que chaque processus nous renvoie sont résultat.

Afin de pouvoir comparer les deux versions, nous allons utiliser la fonction `timer:tc/3`. Cette fonction prend en paramètre un module, une fonction et un tableau de paramètres. Elle exécute la fonction du module donné avec les paramètre passés et nous revoie en retour un tuple dont le premier élément correspond au temps passé entre le début et la fin de l'execution, et le second est le terme renvoyé par la fonction.

Cependant, le temps calculé par `timer:tc/3` correspond au temps réèl. Or il peut varier fortement d'une execution à l'autre. Afin d'avoir un résultat plus probant, nous allons executer 5000 fois chaque version et nous comparerons les temps moyens.

Pour cela nous utiliserons donc la fonction `avg/4` suivante :

`test.erl` :
```
avg(M, F, A, N) when N > 0 ->
  L = avg_loop(M, F, A, N, []),
  Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / length(L)),
  io:format("Moyenne: ~b mics~n", [Avg]).

avg_loop(_M, _F, _A, 0, List) ->
  List;
avg_loop(M, F, A, N, List) ->
  {T, _Result} = timer:tc(M, F, A),
  avg_loop(M, F, A, N - 1, [T|List]).
```

Cette fonction prend en paramètre un module, une fonction, une liste de paramètre et un nombre d'execution. Dans notre cas, nous l'utiliserons ainsi (avec `X = 5000`):

```
avg(para, hash_seq, [Path], X),
...
avg(para, hash_para, [Path], X).
```

Afin de rendre la démonstration plus visuelle, nous allons également utiliser l'outil `observer` d'Erlang. Ce dernier nous permet (onglet _Load Charts_) de visualiser, via des graphs, la quantité de CPU, mémoire et IO utilisés.

Voici le résultat de l'execution dans le shell :

```
1> observer:start().
ok
2> c(para).
{ok,para}
3> c(test).
{ok,test}
4> test:demo("../../chapitres", 5000).
Test séquentiel:
Moyenne: 4697 mics
Test parallèle:
Moyenne: 1740 mics
ok
5> 4697/1740.
2.699425287356322
```

Nous pouvons déjà constater que pour le test séquentielle, le temps moyen d'execution est de 4697 microsecondes, et de 1740 microsecondes dans le cas d'une execution parallèle, soit ~2,70 fois plus rapide.

Mais ce résultat est interressant si nous le correllons avec les graphiques obtenus avec `observer` :

![](../images/09-observer.png)

Sur le graphique du haut, nous voyons, dans le cas de la version parallèle, une occupation de l'ensemble des CPUs, alors que la version séquentiel n'en a utilisé qu'un. Concernant les IOs (courbe en bas à droite), il y a bien entendu un usage plus important dans la version parallèle. Enfin, concernant la consomation mémoire (en bas à gauche), elle est quasiment identique entre les deux versions.

## Gestions des erreurs

Maintenant que nous somme en mesure de créer des processus, interrogeons nous sur leur comportement en cas d'erreur. 

Comme exemple, faisons jouer deux processus au _ping pong_ :

`pingpong.erl` :
```
-module(pingpong).
-export([start/0, ping/0, pong/0]).

start() ->
  register(pong, spawn(?MODULE, pong, [])),
  register(ping, spawn(?MODULE, ping, [])),
  timer:sleep(2000),
  ping ! ping.

ping() ->
  receive
    _ ->
      io:format("[~p] Ping!~n", [self()]),
      timer:sleep(2000),
      pong ! pong,
      ping()
  end.

pong() ->
  receive
    _ ->
      io:format("[~p] Pong!~n", [self()]),
      timer:sleep(2000),
      ping ! ping,
      pong()
  end.
```

Si nous executons ce programme, nous aurons le droit à une infinité de _Ping!_, _Pong!_. Nous pouvons simuler une erreur de l'un des deux processus en utilisant la commande `exit/2`. Cette fonction est identique à celle que nous avons vu au chapitre 9, à la différence qu'elle prend en premier paramètre un identifiant de processus. Donc `exit(Pid, Message)` revient au même que d'executer `exit(Message)` depuis le processus d'identifiant `Pid`.

Si nous executons un `exit` sur le processus `ping` nous avons deux cas possibles :

```
1> c(pingpong).
{ok,pingpong}
2> pingpong:start().
ping
[<0.40.0>] Ping!
[<0.39.0>] Pong!
3> exit(pid(0,40,0), "stop").
true
4>
=ERROR REPORT==== 12-May-2014::16:04:46 ===
Error in process <0.39.0> with exit value: {badarg,[{pingpong,pong,0,[{file,"pingpong.erl"},{line,26}]}]}
4> registered().
[user_drv,standard_error,error_logger,global_group,
 erl_prim_loader,standard_error_sup,init,kernel_safe_sup,
 user,rex,inet_db,kernel_sup,code_server,
 global_name_server,application_controller,file_server_2]
```

Ici, la commande `exit/2` a été executé pendant la pause de 2 seconde entre le moment ou le processus `pong` écrit _Pong!_ à l'écran et le moment ou il envoie son message à `ping`. Donc au moment ou le processus envoie son message à `ping`, celui ci est déjà _mort_, ce qui entraine donc l'erreur car nous essyons d'envoyer un message à un processus qui n'existe plus.

Si maintenant nous executons la commande `exit/2` pendant que le processus `ping` est en _pause_ :

```
5> pingpong:start().
ping
[<0.44.0>] Ping!
[<0.43.0>] Pong!
[<0.44.0>] Ping!
6> exit(pid(0,44,0), "stop").
true
7> registered().
[user_drv,standard_error,error_logger,global_group,
 erl_prim_loader,standard_error_sup,init,kernel_safe_sup,
 user,pong,rex,inet_db,kernel_sup,code_server,
 global_name_server,application_controller,file_server_2]
```

Dans ce cas, le processus `ping` s'est terminé, mais `pong` est encore vivant et attend de recevoir un nouveau message. Au premier message que recevra `pong` il essayera d'envoyer un message à `ping` ce qui génèrera une erreure du même type que précédement :

```
8> pong ! pong.
[<0.43.0>] Pong!
pong
9>
=ERROR REPORT==== 12-May-2014::16:19:51 ===
Error in process <0.43.0> with exit value: {badarg,[{pingpong,pong,0,[{file,"pingpong.erl"},{line,26}]}]}
```

Nous pouvons imaginer plusieurs comportements cohérents dans lesquels l'erreur ne se produirait pas. Tout dépend du service que nous attendons.



`process_flag(trap_exit, true)`

## Programmation distribué