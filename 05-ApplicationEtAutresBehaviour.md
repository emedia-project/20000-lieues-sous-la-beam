# Applications et autres comportements.

Nous y arrivons enfin ! Après vous en avoir souvent parlé lors du chapitre 4, nous allons enfin voir comment créer des applications avec Erlang et Elixir. Ce chapitre va nous faire plonger dans les profondeurs du framework de développement d'Erlang. C'est une étape indispensable pour appréhender toute la magie du langage.

Afin de ne pas trop vous embrouiller l'esprit, je vais me concentrer sur Erlang. Je ne viendrais à Elixir qu'à la fin, pour vous montrer comme appliquer les principes que nous aurons vus, au petit dernier apparu sur la VM.

## Les comportements

Vous êtes probablement familier de la notion d'objet. Si ce n'est pas le cas, tant mieux. Sinon, il va falloir apprendre à modifier votre vison pour penser en terme de processus et de comportement. Ne partez surtout pas en essayant de vous convaincre qu'un processus est le pendant d'un objet. Car si à première vue, nous pourrions le croire, vous allez rapidement vous rendre compte que c'est bien différent. Pour ce qui est des comportements, voyez cela un peu comme des motifs de conception, des _design pattern_.

Il existe trois types de comportements dans le framework OTP : _application_, _supervisor_ et _worker_. Pour ces derniers, il existe différentes formes que nous verrons plus loin.

Une _application_ est en fait un composant passif qui ne va servir qu'à regrouper les différents composants d'un programme. C'est un peu notre point d'entrée. Un _supervisor_ permet, quant à lui, de contrôler le comportement d'autre processus ; ces processus pouvant eux-mêmes être des superviseurs. Nous pouvons donc construire un arbre de gestion de processus. Dans un superviseur, nous mettons en places des règles permettant de gérer le démarrage, l'arrêt et le redémarrage des processus qui lui sont rattachés. Nous touchons là a un point essentiel d'Erlang. En effet, avec les langages que vous avez l'habitude d'utiliser, si un des composants _plante_, tout le système peut s'écrouler. Avec les notions de processus et de superviseurs d'Erlang, si un composant s'effondre, le système n'est pas déstabilisé. Le superviseur auquel est rattaché le processus va déterminer quelle attitude avoir vis-à-vis de ce _plantage_ et peut, au besoin, redémarrer un ou plusieurs processus afin de remettre l'ensemble dans un état stable.

Pour bien comprendre l'organisation d'une application, le plus simple est de faire un premier exemple.

Nous allons créer une calculatrice utilisant la notation polonaise inverse[^RPN]. Le corps de notre exemple sera géré par un processus de type `gen_server`. Il s'agit là d'un comportement permettant d'implémenter un serveur générique. Ce serveur gérera son état propre et nous fournira des services.

## Mise en place du serveur

Notre serveur va être très basique. Il va se contenter de gérer une simple pile de valeurs et pourra réaliser, à la demande, des opérations simples (addition, multiplication, soustraction, division) sur cette pile.

Dans son état initial, notre serveur contient une liste d'éléments vide. Nous allons lui ajouter les fonctions suivantes :

* `push/1` permettra d'ajouter un élément en haut de la pile.
* `add/0`, `sub/0`, `mul/0` et `divs/0` récupèreront les deux éléments en haut de la pile ; réaliseront l'opération correspondante ; placeront le résultat au sommet de la pile et nous renverront ce résultat.
* `show/0` nous servira à afficher le contenu de la pile.
* `del/0`supprimera l'élément en haut de la pile.

> Pour la division, nous utiliserons `divs` plutôt que `div`, ce dernier étant un opérateur qui ne peut donc pas être utilisé _simplement_.

Commençons par créer un répertoire pour notre projet : `calculatrice`. Nous y téléchargeons `rebar` et nous utilisons le template `simplesrv` de ce dernier pour créer la base de notre serveur :

```
$ mkdir calculatrice
$ cd calculatrice
$ wget https://github.com/rebar/rebar/wiki/rebar
...
$ rebar create template=simplesrv srvid=rpn
==> calculatrice (create)
Writing src/rpn.erl
```

> Nous reviendrons sur l'utilisation de `rebar` au chapitre 8.

Regardons le contenu du fichier `src/rpn.erl` :

```
01 -module(rpn).
02 -behaviour(gen_server).
03 -define(SERVER, ?MODULE).
04 
05 %% ------------------------------------------------------------------
06 %% API Function Exports
07 %% ------------------------------------------------------------------
08 
09 -export([start_link/0]).
10 
11 %% ------------------------------------------------------------------
12 %% gen_server Function Exports
13 %% ------------------------------------------------------------------
14 
15 -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
16          terminate/2, code_change/3]).
17 
18 %% ------------------------------------------------------------------
19 %% API Function Definitions
20 %% ------------------------------------------------------------------
21 
22 start_link() ->
23   gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
24 
25 %% ------------------------------------------------------------------
26 %% gen_server Function Definitions
27 %% ------------------------------------------------------------------
28 
29 init(Args) ->
30   {ok, Args}.
31 
32 handle_call(_Request, _From, State) ->
33   {reply, ok, State}.
34 
35 handle_cast(_Msg, State) ->
36   {noreply, State}.
37 
38 handle_info(_Info, State) ->
39   {noreply, State}.
40 
41 terminate(_Reason, _State) ->
42   ok.
43 
44 code_change(_OldVsn, State, _Extra) ->
45   {ok, State}.
46 
47 %% ------------------------------------------------------------------
48 %% Internal Function Definitions
49 %% ------------------------------------------------------------------
```

Nous avons donc affaire à un module suivant le comportement (_behaviour_) `gen_server` (ligne 2). Pour cela, `rebar` a créé pour nous un certain nombre de fonctions.

La fonction `start_link` est appelée pour démarrer le serveur. Elle ne fait qu'un appel à la fonction `start_link` du module `gen_server` en lui passant en paramètre :

* le nom d'enregistrement du serveur : `{local, ?SERVER}`. L'atome `local` permet de préciser que le serveur est _local_. Le nom est donné par la macro `?SERVER`. Nous accèderons donc à ce serveur via ce nom.
> La macro `?SERVER` est définie ligne 3 et correspond au nom du module. En effet, la macro `?MODULE` (qui existe de manière implicite) contient le nom du module courant (`rpn` dans le cas présent). 
* Le nom du module sur lequel vont se faire les appels de _callbacks_ : `?MODULE`.
* Les arguments à passer lors de la création du module. Ici il s'agit une liste vide (`[]`).
* Une liste d'options qui est également vide, par défaut.

L'appel de la fonction `gen_server:start_link/4` va déclencher un appel de la fonction `?MODULE:init/1` (ligne 29). Celle-ci recevra en paramètre les arguments passés à la fonction `gen_server:start_link/4` et renverra un tuple indiquant si cette initialisation s'est correctement déroulée. Pour cela elle renverra soit `{ok, Status}`, où `Status` contiendra l'état initial du serveur ; soit `{error, Reason}`, où `Reason` indiquera la raison de l'échec de l'initialisation.

Dans le serveur que nous voulons mettre en place, l'état initial du serveur est une liste vide. Or c'est justement ce qui est envoyé par `gen_server:start_link/4` à `?MODULE:init/1`, qui se contente de le renvoyer comme état initial (ligne 30).

Admettons que vous doutiez de ce que je viens de vous raconter. Pour vous en convaincre, je vous propose de modifier le serveur en remplaçant la fonction `?MODULE:start_link/0` par ceci :

```
start_link() ->
  error_logger:info_msg("dans ~s:start_link/0", [?MODULE]),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
```

Remplacez ensuite la fonction `?MODULE:init/1` par ceci :

```
init(Args) ->
  error_logger:info_msg("dans ~s:init/1, Args = ~p", [?MODULE, Args]),
  {ok, Args}.
```

Si vous démarrez le shell, que vous compiler le module et que vous le démarrez, vous devriez être rassuré :

```
$ cd src
$ erl
1> c(rpn).
{ok,rpn}
2> rpn:start_link().
{ok,<0.40.0>}
3>
=INFO REPORT==== 3-Jan-2014::15:24:22 ===
dans rpn:start_link/0
=INFO REPORT==== 3-Jan-2014::15:24:22 ===
dans rpn:init/1, Args = []
```

Maintenant que notre serveur est initialisé, voyons comment l'appeler. Pour cela nous pouvons utiliser `gen_server:call/2` pour faire des appels synchrones ou `gen_server:cast/2` pour des appels asynchrones.

Pour ces deux fonctions, nous passerons comme premier paramètre le nom d'enregistrement avec lequel le serveur a été référencé (`?SERVER`) et en second paramètre, les données à lui transmettre. 

Dans le cas d'un appel synchrone, cet appel sera intercepté par le _callback_ `?MODULE:handle_call/3`. Pour un appel asynchrone, c'est `?MODULE:handle_cast/2` qui recevra le message.

Le _callback_ `?MODULE:handle_call/3` reçoit trois paramètres :

* Les données envoyées par l'appelant, correspondant au second paramètre passé lors de l'appel à `gen_server:call/2`.
* Un tuple contenant le _pid_ du client, et un tag unique.
* L'état courant du serveur.

Pour le _callback_ `?MODULE:handle_cast/2`, les paramètres sont les mêmes, à l'exception des informations (tuple `{Pid, Tag}`) du client.

Dans le cadre de notre serveur `rpn`, nous n'utiliserons que des appels synchrones. Pour ajouter une valeur au sommet de la liste, nous déclarerons donc le _callback_ :

```
handle_call({push, Value}, _, State) ->
  NewState = [Value|State],
  {reply, NewState, NewState}.
```

Ce _callback_ renvoie un tuple qui peut être de différentes formes, parmi lesquelles : 

* `{reply, Reply, NewState}` ou `Reply` est la valeur qui sera renvoyée à l'appelant et `NewState` est le nouvel état du serveur.
* `{noreply, NewState}`. Dans ce cas, il n'y a pas de retour à l'appelant, et seul l'état du serveur peut être modifié (`NewState`).
* `{stop, Reason, Reply, NewState}`. Dans ce cas, l'appelant reçoit `Reply` et la fonction `?MODULE:terminate/2` est appelée en lui passant en paramètre `Reason` et `NewState`.

Pour notre serveur, nous renvoyons au client le nouvel état du serveur, soit le contenu de la pile mise à jour.

Pour ajouter un élément dans la pile, le client fera donc l’appel suivant :

```
gen_server:call(Name, {push, Value}).
```

Afin d'éviter que cela soit trop fastidieux, nous ajouterons dans le module `rpn` la fonction publique :

```
push(Value) ->
  gen_server:call(Name, {push, Value}).
```

Ainsi, l'ajout d'un élément dans la pile se fera plus simplement.

Nous ferons de même pour l'ensemble des fonctions de gestion de la pile et de calculs. Nous aboutissons alors au code suivant : 

```
-module(rpn).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([push/1, show/0, del/0, add/0, sub/0, mul/0, divs/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

push(Value) ->
  gen_server:call(?SERVER, {push, Value}).

show() ->
  gen_server:call(?SERVER, show).

del() ->
  gen_server:call(?SERVER, del).

add() ->
  gen_server:call(?SERVER, add).

sub() ->
  gen_server:call(?SERVER, sub).

mul() ->
  gen_server:call(?SERVER, mul).

divs() ->
  gen_server:call(?SERVER, divs).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  {ok, Args}.

handle_call({push, Value}, _From, State) ->
  NewState = [Value|State],
  {reply, NewState, NewState};
handle_call(show, _From, State) ->
  {reply, State, State};
handle_call(del, _From, State) ->
  [_|NewState] = State,
  {reply, NewState, NewState};
handle_call(add, _From, State) ->
  {V1, V2, State1} = get_top(State),
  NewState = [Result = V1+V2|State1],
  {reply, Result, NewState};
handle_call(sub, _From, State) ->
  {V1, V2, State1} = get_top(State),
  NewState = [Result = V1-V2|State1],
  {reply, Result, NewState};
handle_call(mul, _From, State) ->
  {V1, V2, State1} = get_top(State),
  NewState = [Result = V1*V2|State1],
  {reply, Result, NewState};
handle_call(divs, _From, State) ->
  {V1, V2, State1} = get_top(State),
  NewState = [Result = V1/V2|State1],
  {reply, Result, NewState};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_top([V1|List]) -> 
  get_second(V1, List).
get_second(V1, [V2|List]) ->
  {V1, V2, List}.
```

Notre serveur étant prêt, nous pouvons le tester :

```
1> c(rpn).
{ok,rpn}
2> rpn:start_link().
{ok,<0.40.0>}
3> rpn:show().
[]
4> rpn:push(2).
[2]
5> rpn:push(4).
[4,2]
6> rpn:add().
6
7> rpn:show().
[6]
8> rpn:push(4).
[4,6]
9> rpn:mul().
24
```

> Dans les faits, le comportement `gen_server` est beaucoup plus riche que ce qui est présenté ici. Nous aurons l'occasion de voir cela quand nous aborderons le multiprocessing au chapitre 10. En attendant n'hésitez pas à consulter sa documentation[^GEN_SERVER_DOC].

## L'application et le superviseur

Ce que nous avons pourrait parfaitement nous suffire. Mais essayer donc de faire une division par 0 :

```
2> rpn:push(0).
[0]
3> rpn:push(0).
[0,0]
4> rpn:divs().

=ERROR REPORT==== 3-Jan-2014::16:53:34 ===
** Generic server rpn terminating
** Last message in was divs
** When Server state == [0,0]
** Reason for termination ==
** {badarith,[{rpn,handle_call,3,[{file,"src/rpn.erl"},{line,76}]},
              {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,585}]},
              {proc_lib,init_p_do_apply,3,
                        [{file,"proc_lib.erl"},{line,239}]}]}
** exception exit: {{badarith,[{rpn,handle_call,3,
                                    [{file,"src/rpn.erl"},{line,76}]},
                               {gen_server,handle_msg,5,
                                           [{file,"gen_server.erl"},{line,585}]},
                               {proc_lib,init_p_do_apply,3,
                                         [{file,"proc_lib.erl"},{line,239}]}]},
                    {gen_server,call,[rpn,divs]}}
     in function  gen_server:call/2 (gen_server.erl, line 180)
```

Facile, me direz-vous, il suffit de gérer cela au niveau du serveur. Cependant le cas présenté ici est relativement simple. Mais qu'en serait-il avec une application plus complexe ? Il se peut que le _crash_ d'un processus nécessite une remise en état de toute ou partie du système, en fonction de la criticité ou des dépendances qu'a le processus en défaut avec les autres composants. C'est là que la notion de superviseur prend tout son intérêt. En effet, un superviseur va prendre en charge un sous-arbre de processus et en gérer le fonctionnement.

Pour le moment nous allons rester simples. Et nous allons donc créer une application avec un superviseur chargé de gérer notre serveur.

Pour cela nous allons utiliser la commande `create-app` de `rebar` :

```
$ ./rebar create-app appid=calc
==> calculatrice (create-app)
Writing src/calc.app.src
Writing src/calc_app.erl
Writing src/calc_sup.erl 
```

> Vous pouvez également utiliser la commande `create` avec le template `simpleapp`. 

`rebar` a généré pour nous trois fichiers. Le fichier implémentant le comportement `application` (`calc_app.erl`), le fichier de ressources de cette application (`calc.app.src`) et un superviseur (`calc_sup.erl`).

Le fichier `calc_app.erl` est très simple :

```
01 -module(calc_app).
02
03 -behaviour(application).
04
05 %% Application callbacks
06 -export([start/2, stop/1]).
07
08 %% ===================================================================
09 %% Application callbacks
10 %% ===================================================================
11
12 start(_StartType, _StartArgs) ->
13   calc_sup:start_link().
14
15 stop(_State) ->
16   ok.
```

La fonction `start`, appelée au démarrage de l'application, se contente de démarrer le superviseur. Vous pouvez y ajouter toutes les opérations nécessaires à l'initialisation de l'application. En cas de besoin, vous pouvez lui passer des données (via `StartArgs`, qui est ici ignoré). Le paramètre `StartType`, lui aussi ignoré, indique la méthode de démarrage de l'application.

Le fichier `calc.app.src` contient des informations de ressources pour notre application :

```
01 {application, calc,
02  [
03   {description, ""},
04   {vsn, "1"},
05   {registered, []},
06   {applications, [
07                   kernel,
08                   stdlib
09                  ]},
10   {mod, { calc_app, []}},
11   {env, []}
12  ]}.
```

Ce fichier renferme un unique tuple spécifiant que nous avons affaire à une `application`, nommée `calc`. Le dictionnaire qui vient ensuite permet de renseigner des informations propres à notre application :

* Nous avons tout d'abord une description (ligne 3) et une information de version (ligne 4).
* `registered` donne la liste de l'ensemble des processus propres à l'application.
* `applications` permet de lister l'ensemble des applications qui doivent être démarrées avant l'application courante.
* `mod` permet de décrire les paramètres passés à l'application (et donc reçu via la variable `StartArgs`).
* `env` permet de spécifier des informations d'environnement propres à l'application.
* Il y a un paramètre qui n'est pas présent ici : `modules`. Celui-ci sert à référencer l'ensemble des modules rentrant dans la composition de l'application.

Nous allons modifier le fichier `calc.app.src` en y ajoutant une description et une version et nous allons y référencer les modules et processus de notre application. Nous obtenons ceci :

```
01 {application, calc,
02  [
03   {description, "Ma calculatrice Erlang"},
04   {vsn, "0.0.1"},
05   {modules, [calc_app, calc_sup, rpn]},
06   {registered, [rpn]},
07   {applications, [
08                   kernel,
09                   stdlib
10                  ]},
11   {mod, { calc_app, []}},
12   {env, []}
13  ]}.
```

> Si vous voulez comprendre comment fonctionne le passage d'arguments à l'application, vous pouvez vous amuser à modifier la ligne 11 du fichier de ressources de la façon suivante :
> ```{mod, { calc_app, [hello, world]},```
> Dans le fichier `calc_app.erl`, ajoutez, au début de la fonction `start`, quelque chose du genre :
> ```io:format("_StartArgs = ~p~n", [_StartArgs])```
> Lors du démarrage de l'application, vous devriez voir s'imprimer, sur la sortie standard, la liste des paramètres passés, tel que vous les aurez définis dans le fichier de ressources.

Le fichier du superviseur (`calc_sup.erl`) est lui aussi très simple :

```
01 -module(calc_sup).
02 -behaviour(supervisor).
03
04 %% API
05 -export([start_link/0]).
06
07 %% Supervisor callbacks
08 -export([init/1]).
09
10 %% Helper macro for declaring children of supervisor
11 -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
12
13 %% ===================================================================
14 %% API functions
15 %% ===================================================================
16
17 start_link() ->
18   supervisor:start_link({local, ?MODULE}, ?MODULE, []).
19
20 %% ===================================================================
21 %% Supervisor callbacks
22 %% ===================================================================
23
24 init([]) ->
25   {ok, { {one_for_one, 5, 10}, []} }.
```

La fonction `start_link` est très proche de celle que nous avons vue lors de la création du `gen_server`. Et en effet, tout comme `gen_server/start_link/4`, la fonction `supervisor:start_link/3` va déclencher l'appel de la fonction `?MODULE:init/1`. Le troisième paramètre (ici une liste vide), sera passé tel quel à la fonction `init`.

La fonction `?MODULE:init/1` va donc servir à initialiser notre superviseur. En général, vous n'y mettrez pas plus de choses que ce que nous avons ici. En effet, les bonnes habitudes veulent que le domaine de responsabilité soit réduit à son rôle de supervision, le reste étant laissé aux bons soins des workers. Cependant, ne sous-estimons pas les informations retournées par cette fonction. Elles permettent de décrire très finement la manière dont le superviseur va gérer les processus dont il a la charge.

Le retour de la fonction `init` se décompose de la façon suivante :

```
{ok,{{RestartStrategy,MaxR,MaxT},ChildSpecs}}
```

* `RestartStrategy` donne la stratégie de redémarrage des processus dont le superviseur à la charge. Cette stratégie peut être de 4 types :
    * `one_for_one` : si un processus doit être redémarré, lui et lui seul est affecté.
    * `one_for_all` : si un processus doit être redémarré, tous les autres processus rattachés au superviseur sont arrêtés avant d'être tous redémarrés.
    * `rest_for_one` : si un processus doit être redémarré, tous les processus déclarés après lui (dans la liste `ChildSpecs`) sont arrêtés avant d'être tous redémarrés.
    * `simple_one_for_one`: il s'agit là d'une version simplifiée de `one_for_one` utilisée dans le cas ou les processus sont rattachés dynamiquement, et sont tous du même type.
* `MaxR` et `MaxT` servent à gérer les cas d'erreurs qui seraient jugés irrémédiables. Ainsi si le superviseur doit effectuer `MaxR` redémarrages de processus pendant un intervalle de temps de `MaxT` secondes, alors il considèrera qu'il y a un problème irrémédiable, et il arrêtera tous les processus qui lui sont rattachés, avant de s'arrêter lui même.
* `ChildSpecs` est une liste d'éléments décrivant les processus supervisés. Dans le superviseur mis en place par `rebar`, cette liste est vide. Chaque élément de cette liste est défini via un tuple contenant, dans l'ordre, les informations suivantes :
    * L'identifiant du processus.
    * La fonction permettant de démarrer le processus. Il s'agit ici d'un tuple précisant le module et la fonction à appeler.
    * Le type de processus qui peut être :
        * `permanent`, dans ce cas, il sera toujours redémarré en cas de problème.
        * `temporary` ; ce type de processus n'est jamais redémarré.
        * `transient` ; il n'est redémarré que s'il s'est terminé de manière _anormale_.
    * La manière dont le processus doit s'arrêter. Ce paramètre peut avoir comme valeur :
        * `brutal_kill` : dans ce cas, le processus est tué quand il a besoin d'être arrêté.
        * Un entier, représentant une valeur de _timeout_ : dans ce cas, le superviseur envoie un message d'arrêt au processus et attend pendant le temps déterminé. Si au bout de cette période le processus n'a pas déclaré s'être arrêté, le superviseur le tue.
        * `infinity` : dans ce cas, le superviseur attend que le processus se soit arrêté de lui-même.
    * Le type du processus, qui peut être `worker` ou `supervisor`.
    * Le nom du module définissant les callbacks du processus.

Comme vous pouvez le voir, nous avons à notre disposition la macro `?CHILD` pour nous aider à remplir la liste des spécifications de processus.

Dans l'exemple que nous sommes en train de développer, il n'y a qu'un seul processus à superviser. Nous allons donc simplement modifier la fonction `init` du superviseur en référençant ce processus :

```
24 init([]) ->
25   {ok, { {one_for_one, 5, 10}, [
26     ?CHILD(rpn, worker)
27   ]} }.
```

Notre application est maintenant prête. Nous pouvons la tester. Pour cela nous allons la compiler :

```
$ ./rebar compile
==> calculatrice (compile)
Compiled src/calc_app.erl
Compiled src/calc_sup.erl
Compiled src/rpn.erl
```

Puis nous démarrons un shell Erlang, et nous démarrons notre application via la fonction `application:start/1` :

```
$ erl -pa ebin
1> application:start(calc).
ok
```

Nous pouvons vérifier que notre application tourne bien en utilisant la fonction `application:which_applications/0` :

```
2> application:which_applications().
[{calc,"Ma calculatrice Erlang","0.0.1"},
 {stdlib,"ERTS  CXC 138 10","1.19.4"},
 {kernel,"ERTS  CXC 138 10","2.16.4"}]
```

Nous retrouvons dans la liste résultat l'identifiant (`calc`) avec la description et la version que nous avons renseignée dans le fichier `calc.app.src`.

Nous pouvons maintenant utiliser notre serveur `rpn` :

```
3> rpn:push(2).
[2]
4> rpn:push(4).
[4,2]
5> rpn:mul().
8
```

Si maintenant nous faisons tomber notre serveur ; le superviseur devrait le redémarrer automatiquement :

```
6> rpn:push(0).
[0,8]
7> rpn:push(0).
[0,0,8]
8> rpn:divs().

=ERROR REPORT==== 5-Jan-2014::15:15:10 ===
** Generic server rpn terminating
** Last message in was divs
** When Server state == [0,0,8]
** Reason for termination ==
** {badarith,[{rpn,handle_call,3,[{file,"src/rpn.erl"},{line,76}]},
              {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,585}]},
              {proc_lib,init_p_do_apply,3,
                        [{file,"proc_lib.erl"},{line,239}]}]}
** exception exit: {{badarith,[{rpn,handle_call,3,
                                    [{file,"src/rpn.erl"},{line,76}]},
                               {gen_server,handle_msg,5,
                                           [{file,"gen_server.erl"},{line,585}]},
                               {proc_lib,init_p_do_apply,3,
                                         [{file,"proc_lib.erl"},{line,239}]}]},
                    {gen_server,call,[rpn,divs]}}
     in function  gen_server:call/2 (gen_server.erl, line 180)
9> application:which_applications().
[{calc,"Ma calculatrice Erlang","0.0.1"},
 {stdlib,"ERTS  CXC 138 10","1.19.4"},
 {kernel,"ERTS  CXC 138 10","2.16.4"}]
10> rpn:show().
[]
```

Le superviseur a bien fait son travail.

Tout cela est bien beau, me direz-vous, mais nous avons tout de même perdu l'état du processus. Eh oui, il ne faut quand même pas trop compter sur la magie. Si vous avez besoin d'avoir un état persistant, il vous appartiendra de le gérer. Pour cela, il y a plusieurs écoles. L'une d'entre elle consiste à séparer les choses en ayant un processus sans aucune intelligence, qui va gérer l'état (la pile dans notre exemple), et un second qui procurera les services et prendra donc les risques. Avec une telle organisation, si le processus en charge d'effectuer les opérations tombe, avec une stratégie `one_to_one`, la pile ne sera pas touchée. Pour voir comment gérer de la persistance, vous pouvez faire un tour aux chapitres 11 et 12.

## Les autre types de _workers_

Lors de la mise en place de notre calculatrice, nous avons utilisé un _worker_ de type `gen_server`. Il existe deux autres comportements qu'il est également intéressant de connaitre.

### gen_event

Comme son nom le laisse judicieusement penser, ce comportement va servir pour gérer des évènements. Grâce à lui, nous allons pouvoir créer des _handlers_ auxquels nous enverrons des notifications.

Pour mieux comprendre, imaginons que nous souhaitons mettre en place un système de log. Chaque fois que nous voudrons logger une information, nous enverrons une notification au gestionnaire. 

Partons du cas le plus simple pour le moment, à savoir, logger les messages sur la sortie standard. Voici le code de notre _handler_ : 

```
01 -module(term_logger_event).
02 -behaviour(gen_event).
03
04 -export([
05   init/1,
06   handle_event/2,
07   handle_call/2,
08   handle_info/2,
09   code_change/3,
10   terminate/2
11   ]).
12
13 init(Args) ->
14   {ok, Args}.
15
16 handle_event({error, Message}, State) ->
17   io:format("[ERROR] ~p~n", [Message]),
18   {ok, State};
19 handle_event({warning, Message}, State) ->
20   io:format("[WARNING] ~p~n", [Message]),
21   {ok, State};
22 handle_event({debug, Message}, State) ->
23   io:format("[DEBUG] ~p~n", [Message]),
24   {ok, State};
25 handle_event({info, Message}, State) ->
26   io:format("[INFO] ~p~n", [Message]),
27   {ok, State}.
28
29 handle_call(_Request, State) ->
30   {ok, ok, State}.
31
32 handle_info(_Info, State) ->
33   {ok, State}.
34
35 code_change(_OldVsn, State, _Extra) ->
36   {ok, State}.
37
38 terminate(_Args, _State) ->
39   ok.
```

> Il n'existe pas de template dans `rebar` pour la création de gestionnaire d'évènements.

Comme vous pouvez le remarquer, le code est très proche de ce que nous aurions pu écrire pour un `gen_server`. Au point que l'on serait légitimement en droit de se demander qu'est-ce qui les différentie. En fait, la principale différence vient du fait que nous n'avons pas, ici, affaire à un serveur, mais à un gestionnaire. Et que, contrairement à un serveur, il n'est pas autonome. En effet, pour pouvoir l'utiliser, nous allons devoir le référencer avant de pouvoir lui envoyer des messages. Pour cela, nous commencerons par utiliser la méthode `gen_event:add_handler/3`. Cette fonction prend en paramètre :

* Un atome qui servira de référence pour le gestionnaire d'évènement.
* Un atome correspondant au nom du module ayant le comportement `gen_event`.
* Une liste de paramètres à passer au gestionnaire lors de son initialisation.

Donc, pour notre exemple, nous ferons l'appel suivant :

```
gen_event:add_handler(logger_manager, term_logger_event, []).
```

Maintenant que notre gestionnaire est référencé, nous pouvons lui envoyer des notifications via la fonction `gen_event:notify/2`. Cette fonction prend en paramètre l'atome de référence du gestionnaire (`logger_manager`) et le message à lui envoyer. Dans notre cas, quatre messages sont acceptés : `{error, Message}`, `{warning, Message}`, `{debug, Message}` et `{info, Message}`. 

Afin de faciliter l'utilisation de notre gestionnaire, nous pouvons créer un module proposant une interface plus _sexy_ à nos utilisateurs :

```
01 -module(logger).
02
03 -export([
04   start/0,
05   add_logger/1,
06   add_logger/2,
07   error/1,
08   warning/1,
09   debug/1,
10   info/1
11   ]).
12
13 start() ->
14   gen_event:start({local, logger_manager}).
15
16 add_logger(Logger) ->
17   add_logger(Logger, []).
18
19 add_logger(Logger, Args) ->
20   gen_event:add_handler(logger_manager, Logger, Args).
21
22 error(Message) ->
23   gen_event:notify(logger_manager, {error, Message}).
24
25 warning(Message) ->
26   gen_event:notify(logger_manager, {warning, Message}).
27
28 debug(Message) ->
29   gen_event:notify(logger_manager, {debug, Message}).
30
31 info(Message) ->
32   gen_event:notify(logger_manager, {info, Message}).
```

Nous avons maintenant un logger facile à utiliser :

```
1> c(logger).
{ok,logger}
2> c(term_logger_event).
{ok,term_logger_event}
3> logger:start().
{ok,<0.46.0>}
4> logger:add_logger(term_logger_event).
ok
5> logger:info("This is an info message").
[INFO] "This is an info message"
ok
6> logger:debug("This is a debug message").
[DEBUG] "This is a debug message"
ok
```

Mais avec tout cela, il n'est pas certain que nous ayons gagné quoi que ce soit par rapport à l'utilisation d'un `gen_server`. C'est un peu vrai. Mais imaginez que vous souhaitiez ajouter la possibilité de logger dans un fichier. Avec un `gen_server`, il faudrait modifier le code de ce dernier. Dans le cas d'un `gen_event`, il suffit de créer un nouveau gestionnaire d'évènement, de le référencer, et le tour est joué. Vous pourrez ainsi créer autant de gestionnaires que vous le souhaitez. 

Pour nous en rendre compte, ajoutons de quoi logger dans un fichier. Avant cela, vous aurez certainement noté que, comme le `gen_server`, un `gen_event` possède un état, représenté par la variable `State` dans notre code. Nous n'en avions pas eu besoin. Nous allons l'utiliser ici pour stocker le descripteur du fichier de log :

```
01 -module(file_logger_event).
02 -behaviour(gen_event).
03
04 -export([
05   init/1,
06   handle_event/2,
07   handle_call/2,
08   handle_info/2,
09   code_change/3,
10   terminate/2
11   ]).
12
13 init([File]) ->
14   {ok, Fd} = file:open(File, [append]),
15   {ok, Fd}.
16
17 handle_event({error, Message}, Fd) ->
18   io:format(Fd, "[ERROR] ~p~n", [Message]),
19   {ok, Fd};
20 handle_event({warning, Message}, Fd) ->
21   io:format(Fd, "[WARNING] ~p~n", [Message]),
22   {ok, Fd};
23 handle_event({debug, Message}, Fd) ->
24   io:format(Fd, "[DEBUG] ~p~n", [Message]),
25   {ok, Fd};
26 handle_event({info, Message}, Fd) ->
27   io:format(Fd, "[INFO] ~p~n", [Message]),
28   {ok, Fd}.
29
30 handle_call(_Request, Fd) ->
31   {ok, ok, Fd}.
32
33 handle_info(_Info, Fd) ->
34   {ok, Fd}.
35
36 code_change(_OldVsn, Fd, _Extra) ->
37   {ok, Fd}.
38
39 terminate(_Args, Fd) ->
40   file:close(Fd).
```

Nous pouvons maintenant offrir à nos utilisateurs la possibilité de logger sur la sortie standard, dans un fichier, ou les deux :

```
1> c(logger).
{ok,logger}
2> c(term_logger_event).
{ok,term_logger_event}
3> c(file_logger_event).
{ok,file_logger_event}
4> logger:start().
{ok,<0.52.0>}
5> logger:add_logger(term_logger_event).
ok
6> logger:add_logger(file_logger_event, ["my_logger.log"]).
ok
7> logger:info("This is an info message").
ok
[INFO] "This is an info message"
8>
BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
       (v)ersion (k)ill (D)b-tables (d)istribution
a
$ cat my_logger.log
[INFO] "This is an info message"
```

Pour avoir quelque chose de parfaitement utilisable, l'idéal serait de tout référencer au sein d'une application et de permettre de paramétrer le logger (_handlers_ à utiliser, niveau de log, chemin des fichiers de log...) via les paramètres de l'application. Je vous laisse le plaisir de cet exercice. L'objectif serait d'arriver à faire quelque chose comme ça :

```
$ erl -pa ebin
1> logger:info("Ca va planter").
** exception throw: "application logger not started"
     in function  logger:info/1 (src/logger.erl, line 31)
2> application:start(logger).
ok
3> logger:info("Maintenant c'est bon").
[INFO] "Maintenant c'est bon"
ok
```

Pour mettre en place cette application, nous allons simplement créer une application (`logger`). Dans cette application, nous utiliserons les possibilités offertes par le système de configuration (le fichier `.app`) pour spécifier le ou les _handlers_ à utiliser.

Nous commençons donc pas créer l'application et nous modifions le fichier `logger.app.src` de la façon suivante :

```
{application, logger,
 [
  {description, "My Erlang logger"},
  {vsn, "1"},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { logger_app, []}},
  {env, [
    {handlers, [
      {file_logger, ["my_log_file.log"]},
      {stdout_logger, []}
    ]}
  ]}
 ]
}.
```

Ce que vous devez noter ici est l'apparition de la clé `env`. Nous l'utilisons afin de créer des informations d'environnement pour notre application. Dans le cas présent, nous spécifions une entrée `handlers` renvoyant une liste de modules (`file_logger` et `stdout_logger`) suivis d'un tableau de paramètres à passer lors de l'initialisation de ces modules.

> Pour des questions de lisibilité, j'ai renommé `term_logger_event` en `stdout_logger` et `file_logger_event` en `file_logger`. Pour le reste, le code est strictement le même que celui que nous avons mis en place ci-dessus. Il faudra donc modifier le nom des fichiers et des modules.

Dans le fichier `logger_app.erl` généré par `rebar`, nous ne modifierons rien. Dans le superviseur, par contre, nous démarrerons un `worker` que nous appellerons `logger_srv`, en utilisant la stratégie `one_for_one`. Nous devons donc créer ce worker.

Pour le moment, nous avons systématiquement créé des workers qui utilisent un comportement. Dans le cas présent, nous allons créer un worker spécifique utilisant donc aucun _behaviour_. Le code sera le suivant :

`src/logger_srv.erl` :
```
01 -module(logger_srv).
02
03 -export([
04   start_link/0,
05   add_logger/1,
06   add_logger/2
07   ]).
08
09 start_link() ->
10   R = gen_event:start({local, logger_manager}),
11   {ok, LH} = application:get_env(logger, handlers),
12   [start_logger(H) || H <- LH],
13   R.
14
15 add_logger(Logger) ->
16   add_logger(Logger, []).
17
18 add_logger(Logger, Args) ->
19   gen_event:add_handler(logger_manager, Logger, Args).
20
21 start_logger({Module, Params}) ->
22  add_logger(Module, Params).
```

Nous retrouvons dans ce code la fonction `start_link/0`. C'est elle qui sera appelée lors du démarrage par le superviseur. Dans cette fonction, nous commençons pas démarrer le gestionnaire d'évènements (ligne 10). Ensuite nous récupérons, via `application:get_env/2`, le contenu de l'entrée `handlers` déclaré dans l'environnement de l'application `logger`. Nous avons donc dans `LH` (ligne 11) un tableau d'atomes donc chacun donne le handler de logging à démarrer et les paramètres : `[{file_logger, ["my_log_file.log"]}, {stdout_logger, []}]`. Nous passons chaque élément de ce tableau à la fonction `start_logger` (ligne 12) qui servira de relais pour traiter le tuple et envoyer le nom du module, et le tableau de paramètres, à la fonction `add_logger` qui se chargera d'ajouter le handler à notre gestionnaire d'évènement (ligne 19).

Afin de faciliter l'utilisation de notre application, nous pouvons maintenant créer un module facilitant l'accès aux fonctions de logging :

`src/logger.erl`
```
-module(logger).

-export([
  error/1,
  warning/1,
  debug/1,
  info/1
  ]).

error(Message) ->
  case started() of
    ok -> gen_event:notify(logger_manager, {error, Message});
    _ -> throw("application logger not started")
  end.

warning(Message) ->
  case started() of
    ok -> gen_event:notify(logger_manager, {warning, Message});
    _ -> throw("application logger not started")
  end.

debug(Message) ->
  case started() of
    ok -> gen_event:notify(logger_manager, {debug, Message});
    _ -> throw("application logger not started")
  end.

info(Message) ->
  case started() of
    ok -> gen_event:notify(logger_manager, {info, Message});
    _ -> throw("application logger not started")
  end.

started() ->
  case application:get_application(logger_app) of
    {ok, _} -> ok;
    _ -> error
  end.
```

### gen_fsm

Ce comportement va nous permettre de créer des machines à états finis. Il existe de multiples exemples d'automate fini. Si la nostalgie de votre jeunesse vous a poussé à faire lire à vos enfants des livres dont vous êtes le héros[^LIVRE_JEU], alors vous leur avez mis dans les mains un modèle d'automate fini. Les jeux d'aventure en mode texte de notre enfance en sont un autre. Et plus sérieusement, nous pouvons citer comme exemple la commande en ligne, les distributeurs de billets, les portes automatiques ou les systèmes de traitement du langage...

Contrairement au comportement `gen_event` il existe un template `rebar` pour `gen_fsm`. Cependant, l'organisation d'un module avec ce comportement étant _tellement_ spécifique qu'il ne sert pas a grand-chose.

Pour illustrer le propos, je vous propose de mettre en place un automate simulant une porte automatique à code. Dans l'état initial, la porte est fermée (état _close_). Pour ouvrir la porte, nous entrons un code. Si le code est bon, la porte s'ouvre (état _opened_) pour une durée de 10 secondes ; si le code est faux, la porte reste fermée (_close_). Si on entre le code d'ouverture de la porte, alors qu'elle est déjà ouverte, elle reste dans son état, mais réinitialise le compteur de temps d'ouverture. Si la porte est fermée, et qu'on essaye de la fermer, il ne se passe rien. A cela, nous ajouterons le fait que, la demande d'ouverture doit être synchrone. Il n'est pas question d'attendre sans savoir si la porte va s'ouvrir ou non. A l'inverse, pour la fermeture, nous pouvons le faire de manière asynchrone, la porte se fermant _quand elle veut_.

Voici un diagramme d'état qui présente le niveau d'implémentation complet de notre machine à états[^TH_TMT] :

![](../images/image002.png)

La première chose à faire est de démarrer notre _worker_. Pour cela nous allons utiliser la fonction `gen_fsm:start_link/4`. Cette fonction est en tout point semblable à celle de `gen_server`. Comme cette dernière, elle va appeler `?MODULE:init/1` :

```
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, {close, 1234}, []).
  
init({StateName, StateValue}) ->
  {ok, StateName, StateValue}.
```

Dans `?MODULE:start_link/0`, nous appelons donc `gen_fsm:start_link/4` en lui passant en troisième paramètre un tuple qui sera transmis à `?MODULE:init/1`. Ce tuple correspond, pour la première valeur, a un atome donnant le nom de l'état initial du système et l'état. J'utilise ici l'état, pour y stocker une simple valeur correspondant au code autorisant l'ouverture de la porte.

Nous avons ensuite besoin de créer un fonction permettant d'ouvrir la porte. Cette fonction recevra en paramètre le code saisi par l'utilisateur et enverra un évènement, de manière synchrone, à la machine, via la fonction `gen_fsm:sync_send_event/2` :

```
open(Pin) ->
  gen_fsm:sync_send_event(?SERVER, {open, Pin}).
```

Côté machine a états, nous avons deux possibilités. La première concerne la cas ou la porte est déjà ouvert, dans ce cas nous réinitialisons le compteur de temps. 

```
opened({open, _Pin}, _From, State) ->
  io:format("Door is already open...~n"),
  {reply, open, opened, State, 10000};
```

Vous noterez que le retour est un tuple qui peut avoir la forme suivante :

* `{reply, Reply, NextStateName, NewStateData, Timeout}` ou `Reply` correspond à ce qui sera renvoyé à l'utilisateur, `NextStateName` est un atome donnant le nom du nouvel état, `NewStateData` contient les données de la machine, dans le nouvel état et `Timeout` est le temps pendant lequel la machine doit rester dans le nouvel état. Une fois ce temps écoulé, la machine fera un appel asynchrone sur l'état coutant avec comme avènement : `timeout`. Le `Timeout` est optionnel. Il peut également prendre la valeur `hibernate`, ce qui aura pour effet de mettre la machine en attente jusqu'à l'arrivée du prochain message.
* `{next_state, NextStateName, NewStateData, Timeout}`. Ici le couple `reply, Reply` est remplacé par un `next_state`. Dans ce cas la machine va directement passer dans l'état `NextStateName` (avec comme données `NewStateData`).
* `{stop, Reason, Reply, NewStateData}`. Ceci va entrainer l'appel de la fonction `?MODULE:terminate/2` en lui passant en paramètre `Reply` et `NewStateData`. Si elle est renseignée, la valeur `Reply` sera renvoyée à l'utilisateur.

Si la porte est fermée, nous vérifions le code et nous modifions l'état en fonction de la validité de ce code :

```
close({open, Pin}, _From, State) ->
  if
    Pin =:= State ->
      io:format("Door open (for 10s)~n"),
      {reply, open, opened, State, 10000};
    true ->
      io:format("Wrong PIN, door is closed !~n"),
      {reply, close, close, State}
  end;
```

Pour la fermeture de la porte, l'appel se fait de manière asynchrone en utilisant `gen_fsm:send_event/2` :

```
close() ->
  gen_fsm:send_event(?SERVER, close).
```

Là aussi, nous agirons en fonction de l'état courant de la machine :

```
close(close, State) ->
  io:format("Door already closed!~n"),
  {next_state, close, State};

opened(close, State) ->
  io:format("You closed the door...~n"),
  {next_state, close, State};
```

Pour ces fonctions, le résultat est un tuple qui peut avoir la forme :

* `{next_state, NextStateName, NewStateData, Timeout}`. Ici aussi, `Timeout` est optionnel et peut prendre la valeur `hybernate`.
* `{stop, Reason, NewStateData}`.

Le code complet de notre module sera donc le suivant :

```
-module(door).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% API Function Exports
-export([
  start_link/0,
  open/1,
  close/0
  ]).

%% gen_fsm Function Exports
-export([
  init/1,
  close/2,
  close/3,
  opened/2,
  opened/3,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4
  ]).

%% API Function Definitions

start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, {close, 1234}, []).

open(Pin) ->
  gen_fsm:sync_send_event(?SERVER, {open, Pin}).

close() ->
  gen_fsm:send_event(?SERVER, close).

%% gen_fsm Function Definitions

init({StateName, StateValue}) ->
  {ok, StateName, StateValue}.

%% For async call
close(close, State) ->
  io:format("Door already closed!~n"),
  {next_state, close, State};
close(_Event, State) ->
  {next_state, close, State}.

opened(close, State) ->
  io:format("You closed the door...~n"),
  {next_state, close, State};
opened(_Event, State) ->
  io:format("~p : closing door...~n", [_Event]),
  {next_state, close, State}.

%% For sync call
close({open, Pin}, _From, State) ->
  if
    Pin =:= State ->
      io:format("Door open (for 10s)~n"),
      {reply, open, opened, State, 10000};
    true ->
      io:format("Wrong PIN, door is closed !~n"),
      {reply, close, close, State}
  end;
close(_Event, _From, State) ->
  {reply, {error, invalid_message}, close, State}.

opened({open, _Pin}, _From, State) ->
  io:format("Door is already open...~n"),
  {reply, open, opened, State, 10000};
opened(_Event, _From, State) ->
  {reply, {error, invalid_message}, close, State}.

%% ...
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
```

Et voici ce que cela donnera à l'utilisation :

```
1> c(door).
{ok,door}
2> door:start_link().
{ok,<0.40.0>}
3> door:close().
Door already closed!
ok
4> door:open(4321).
Wrong PIN, door is closed !
close
5> door:open(1234).
Door open (for 10s)
open
6> door:open(1234).
Door is already open...
open
timeout : closing door...
7> door:close().
Door already closed!
ok
8> door:open(1234).
Door open (for 10s)
open
9> door:close().
You closed the door...
ok
```

## Et Elixir ?

Elixir utilise les mêmes principes de comportement qu'Erlang. Et vous avez donc le droit, respectivement, à `Application.Behaviour`, `GenServer.Behaviour` et `GenEvent.Behaviour`. Il n'y a (pour le moment) pas d'équivalent pour `gen_fsm`.


Pour déclarer un comportement dans un module, nous utiliserons la fonction `use` :

```
defmodule MyServer do
  use GenServer.Behaviour

  # ...
end
```

Pour le reste, il suffit de traduire les fonctions et callbacks en Elixir.

Pour voir cela, nous allons recoder notre calculatrice en Elixir. Nous commençons par créer une nouvelle application :

```
mix new calc
* creating README.md
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/calc.ex
* creating lib/calc
* creating lib/calc/supervisor.ex
* creating test
* creating test/test_helper.exs
* creating test/calc_test.exs

Your mix project was created successfully.
You can use mix to compile it, test it, and more:

    cd calc
    mix test

Run `mix help` for more commands.
```

Dans le répertoire `calc`, nous avons un fichier `lib/calc.ex` qui utilise le comportement `Application` :

`lib/calc.ex`
```
defmodule Calc do
  use Application.Behaviour

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    Calc.Supervisor.start_link
  end
end
```

Nous retrouvons dans ce fichier la fonction `start` identique à celle que nous avions dans le module `calc_app` en Erlang. Cette fonction, comme nous pouvions nous y attendre, se contente de démarrer le superviseur. Le code de ce dernier se trouve dans `lib/calc/supervisor.ex` :

```
defmodule Calc.Supervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    children = [
      # Define workers and child supervisors to be supervised
      # worker(Calc.Worker, [arg1, arg2, arg3])
    ]

    # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
    # for other strategies and supported options
    supervise(children, strategy: :one_for_one)
  end
end
```

Nous avons, ici aussi, quelque chose qui ressemble beaucoup au code de `calc_sup.erl`. Vous noterez cependant la différence dans la fonction `init` qui propose ici d'utiliser une fonction `supervise` pour définir la liste des `workers` supervisés, et la stratégie utilisée. La liste des `workers` est donnée via `children`. Cette liste est composée des résultats de l'appel à la fonction `worker` pour chaque `worker`. Dans notre cas, nous allons créer un `GenServer` que nous définirons dans un module `Calc.RPN`. Nous devons donc modifier le fichier `lib/calc/supervisor.ex` de la façon suivante :

```
defmodule Calc.Supervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    children = [
      worker(Calc.RPN, [])
    ]
    supervise(children, strategy: :one_for_one)
  end
end
```

Il ne nous reste plus qu'à créer le `GenServer`. Pour cela, `mix` ne propose aucune aide, il faudra donc tout écrire à la main. Voici le code que nous aurons :

`lib/calc/rpn.ex`
```
defmodule Calc.RPN do
  use GenServer.Behaviour

  def start_link() do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def push(value) do
    :gen_server.call(__MODULE__, {:push, value})
  end

  def show() do
    :gen_server.call(__MODULE__, :show)
  end

  def del() do
    :gen_server.call(__MODULE__, :del)
  end

  def add() do
    :gen_server.call(__MODULE__, :add)
  end

  def sub() do
    :gen_server.call(__MODULE__, :sub)
  end

  def mul() do
    :gen_server.call(__MODULE__, :mul)
  end

  def divs() do
    :gen_server.call(__MODULE__, :divs)
  end

  def init(args), do: {:ok, args}

  def handle_call({:push, value}, _from, state) do
    new_state = [value|state]
    {:reply, new_state, new_state}
  end
  def handle_call(:show, _from, state) do
    {:reply, state, state}
  end
  def handle_call(:del, _from, state) do
    [_|new_state] = state
    {:reply, new_state, new_state}
  end
  def handle_call(:add, _from, state) do
    [v1, v2|rest] = state
    new_state = [result = v1+v2|rest]
    {:reply, result, new_state}
  end
  def handle_call(:sub, _from, state) do
    [v1, v2|rest] = state
    new_state = [result = v1-v2|rest]
    {:reply, result, new_state}
  end
  def handle_call(:mul, _from, state) do
    [v1, v2|rest] = state
    new_state = [result = v1*v2|rest]
    {:reply, result, new_state}
  end
  def handle_call(:divs, _from, state) do
    [v1, v2|rest] = state
    new_state = [result = v1/v2|rest]
    {:reply, result, new_state}
  end
  def handle_call(_requesr, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end
  def handle_info(_info, state) do
    {:noreply, state}
  end
end
```

Ici aussi, nous retrouvons un code très proche de celui que nous avons écrit pour Erlang.

Vous aurez probablement noté que, lors de la génération du projet, `mix` ne nous a généré aucun fichier `.app`. Cependant, si nous compilons notre application, via la commande `mix compile`, nous n'avons pas d'erreur. Mieux, lors de cette étape `mix` a créé pour nous un fichier `.app`. 

L'application étant compilée, nous pouvons tester.

```
$ iex -S mix
iex(1)> :application.which_applications()
[{:calc, 'calc', '0.0.1'}, {:mix, 'mix', '0.13.2-dev'},
 {:iex, 'iex', '0.13.2-dev'}, {:elixir, 'elixir', '0.13.2-dev'},
 {:stdlib, 'ERTS  CXC 138 10', '2.0'}, {:kernel, 'ERTS  CXC 138 10', '3.0'}]
iex(2)> Calc.RPN.push 2
[2]
iex(3)> Calc.RPN.push 4
[4, 2]
iex(4)> Calc.RPN.mul
8
```

Nous pouvons vérifier que le superviseur fait bien son travail en cas de plantage du serveur :

```
iex(5)> Calc.RPN.push 0
[0, 8]
iex(6)> Calc.RPN.push 0
[0, 0, 8]
iex(7)> Calc.RPN.divs

=ERROR REPORT==== 29-Apr-2014::20:51:21 ===
** Generic server 'Elixir.Calc.RPN' terminating
** Last message in was divs
** When Server state == [0,0,8]
** Reason for termination ==
** {badarith,[{'Elixir.Calc.RPN',handle_call,3,
                                 [{file,"lib/calc/rpn.ex"},{line,66}]},
              {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,580}]},
              {proc_lib,init_p_do_apply,3,
                        [{file,"proc_lib.erl"},{line,239}]}]}
** (exit) {{:badarith, [{Calc.RPN, :handle_call, 3, 
[file: 'lib/calc/rpn.ex', line: 66]}, {:gen_server, :handle_msg, 5, 
[file: 'gen_server.erl', line: 580]}, {:proc_lib, :init_p_do_apply, 3, 
[file: 'proc_lib.erl', line: 239]}]}, {:gen_server, :call, [Calc.RPN, :divs]}}
    (stdlib) gen_server.erl:182: :gen_server.call/2
iex(7)> :application.which_applications()
[{:calc, 'calc', '0.0.1'}, {:mix, 'mix', '0.13.2-dev'},
 {:iex, 'iex', '0.13.2-dev'}, {:elixir, 'elixir', '0.13.2-dev'},
 {:stdlib, 'ERTS  CXC 138 10', '2.0'}, {:kernel, 'ERTS  CXC 138 10', '3.0'}]
iex(8)> Calc.RPN.show
[]
```

## Aller plus loin...

Il reste beaucoup a dire sur les comportement. Nous aurons largement l'occasion de revoir certains points dans les chapitres suivantes. En attendant, vous avez maintenant les bases nécessaires pour commencer à créer des applications.

[^RPN]: [http://fr.wikipedia.org/wiki/Notation_polonaise_inverse](http://fr.wikipedia.org/wiki/Notation_polonaise_inverse)

[^GEN_SERVER_DOC]: [http://www.erlang.org/doc/man/gen_server.html](http://www.erlang.org/doc/man/gen_server.html)

[^LIVRE_JEU]: [http://fr.wikipedia.org/wiki/Livre-jeu](http://fr.wikipedia.org/wiki/Livre-jeu)

[^TH_TMT]: Merci @Thiasmathias
