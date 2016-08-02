# Distribuer ses modules

Au chapitre 6, nous avons vu comment écrire des modules, et il est temps de voir comment les distribuer et les réutiliser. Pour cela nous allons repartir de l'exemple mis en place au chapitre  précédent. Nous avions développé un module (`mathematics`) contenant une fonction permettant de calculer un élément de la suite de Fibonacci. Nous allons voir comment organiser notre module de façon à pourvoir le réutiliser. Ensuite nous créerons un second module, utilisant le premier.

## Organisation des sources

Quelque soit le(s) langage(s) que vous utilisez habituellement, vous le savez, chacun à son système de packaging et de distribution propre. Je pense ici à des choses comme Rubygems pour Ruby, NPM pour Nodes, pip pour Python, PPM pour Perl ou Maven pour Java. Certains en ont même plusieurs -- sinon cela serait trop simple ;)

Et bien Erlang et Elixir ne dérogent pas à cette règle. Pour Elixir nous utiliserons `Mix`, fournit en standard. Pour Erlang, nous utiliserons `Rebar`, un outil externe initialement développé par Basho.

Jusqu'à présent, nous avons abordé Elixir et Erlang en suivant des chemins parallèles, voire entremêlés. Cela ne va pas être possible ici. En effet, bien que `Mix` et `Rebar` aient certaines similitudes, ils ont suffisamment de différences pour nous obliger à les aborder séparément.

### Mix

Elixir vient avec son outil de gestion de packages : `Mix`. Cet utilitaire comprend toutes les fonctionnalités que vous êtes en droit d'attendre d'un outil de packaging. Pour vous en convaincre, taper la commande `mix help` dans votre terminal :

```
$ mix help
mix                 # Run the default task (current: mix run)
mix archive         # Archive this project into a .ez file
mix clean           # Clean generated application files
mix cmd             # Executes the given command
mix compile         # Compile source files
mix deps            # List dependencies and their status
mix deps.clean      # Remove the given dependencies' files
mix deps.compile    # Compile dependencies
mix deps.get        # Get all out of date dependencies
mix deps.unlock     # Unlock the given dependencies
mix deps.update     # Update the given dependencies
mix do              # Executes the tasks separated by comma
mix escriptize      # Generates an escript for the project
mix help            # Print help information for tasks
mix local           # List local tasks
mix local.install   # Install a task or an archive locally
mix local.rebar     # Install rebar locally
mix local.uninstall # Uninstall local tasks or archives
mix new             # Create a new Elixir project
mix run             # Run the given file or expression
mix test            # Run a project's tests
iex -S mix          # Start IEx and run the default task
```

Nous allons commencer par créer un nouveau projet. Pour cela nous utiliserons la commande `new` de `mix`, suivie du nom du projet. Ce dernier, doit obligatoirement avoir la forme `/[a-z][a-z0-9_]*/`. Soit, il commence obligatoirement par une lettre minuscule, et ne contient que des lettres (minuscules) des chiffres ou des underscore (`_`). En ce qui nous concerne, le nom est tout trouvé :

```
$ mix new mathematics
* creating README.md
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/mathematics.ex
* creating lib/mathematics
* creating lib/mathematics/supervisor.ex
* creating test
* creating test/test_helper.exs
* creating test/mathematics_test.exs

Your mix project was created successfully.
You can use mix to compile it, test it, and more:

    cd mathematics
    mix compile
    mix test

Run `mix help` for more information.
```

Vous devez maintenant avoir un répertoire `mathematics` avec le contenu suivant :

```
mathematics
├── .gitignore
├── README.md
├── lib
│   ├── mathematics
│   │   └── supervisor.ex
│   └── mathematics.ex
├── mix.exs
└── test
    ├── mathematics_test.exs
    └── test_helper.exs
```

Comme vous pouvez le voir, la structure d'un projet Elixir est très simple. Nous avons, dans le répertoire `lib` les fichiers sources du projet. Le fichier `mix.exs` est le fichier de description que l'on peut comparer à un `rakefile` Ruby ou un `setup.py` Python. Le répertoire `test` contiendra les sources pour la suite de tests unitaires.

Elixir part du principe que nous allons développer une _application_ OTP. Pour le moment, nous allons nous contenter d'un simple module.

> OTP signifie _Open Telecom Platform_. Sous ce terme se cache l'ensemble des composants utiles pour développer avec Erlang (compilateur, interpréteur, framework et librairies, etc...) et les principes sous-jascents. Il faut se souvenir qu'avant 1998, date à laquelle il a été placé en open source, Erlang était un produit propriétaire d'Ericsson, utilisé pour ses propres besoins.

Nous aborderons la notion d'application dans le prochain chapitre. Pour le moment, nous allons nous contenter de supprimer les fichiers `mathematics.ex` et `mathematics/supervisor.ex` du répertoire `lib` :

```
$ rm -rf lib/*
```

Nous pouvons maintenant ajouter le fichier `mathematics.ex`, créé au chapitre précédent, dans le répertoire `lib`. Voici le contenu de ce fichier :

```
defmodule Mathematics do
  def fibonacci(0), do: 0
  def fibonacci(1), do: 1
  def fibonacci(n) when is_integer(n) and n > 1 do
    fibonacci(n-1) + fibonacci(n-2)
  end
end
```

Voyons maintenant le fichier `mix.exs`. 

```
defmodule Mathematics.Mixfile do
  use Mix.Project

  def project do
    [ app: :mathematics,
      version: "0.0.1",
      elixir: "~> 0.11.2",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [mod: { Mathematics, [] }]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1" }
  #
  # To specify particular versions, regardless of the tag, do:
  # { :barbat, "~> 0.1", github: "elixir-lang/barbat.git" }
  defp deps do
    []
  end
end
```

Ce fichier est un fichier source Elixir décrivant un module (`Mathematics.Mixfile` dans notre cas). Il comprend trois fonctions :

* `project` permettant de donner des informations sur le projet.
* `application` donnant des informations sur l'_application_.
* `deps` pour la gestion des dépendances.

Vous l'aurez certainement remarqué, la fonction `deps` est privée. En fait elle est appelée par la fonction `project` pour affecter la valeur rattachée à la clé `deps` du dictionnaire renvoyé. Nous y reviendrons plus tard dans ce chapitre, quand nous aborderons la gestion de dépendances.

Comme nous l'avons vu, `Mix` génère par défaut un projet pour la création d'une application. Comme ce n'est pas ce que nous allons faire ici, nous devons modifier le corps de la fonction `application` et renvoyer une liste vide : 

```
def application do
  []
end
```

Vérifions maintenant que notre projet est fonctionnel. Pour cela nous allons commencer par le compiler :

```
$ mix compile
Compiled lib/mathematics.ex
Generated mathematics.app
```

Lors de la compilation, `mix` a créé un répertoire `_build` :

```
_build
└── shared
    └── lib
        └── mathematics
            ├── .compile.elixir
            ├── .compile.lock
            └── ebin
                ├── Elixir.Mathematics.beam
                └── mathematics.app
```

Nous retrouvons le fichier `Elixir.Mathematics.beam` que nous avons obtenu lorsque nous avions compilé _à la main_ notre module, au chapitre précédent. Notez également la présence du fichier `mathematics.app`. Il s'agit du fichier application. Encore une fois, je laisse les explications sur ce fichier de côté. Les fichiers `.compile.*` sont spécifiques à `Mix` et nous pouvons donc les laisser de côté. 

Pour vérifier que notre module fonctionne bien nous allons utiliser le shell Elixir en lui demandant de charger les fichiers compilés. La VM Erlang a, de ce point de vue là, un fonctionnement que l'on pourrait comparer au principe du `classpath` de Java. En effet, pour charger un fichier de bytecode, il faut, lors de l'appel à la VM, lui préciser l'emplacement des fichiers à charger. Avec le shell Erlang (`erl`) cela se fait en utilisant l'option `-pa` suivi du chemin du répertoire contenant les fichiers `beam`. Cette option pouvant être répétée autant de fois que nécessaire :

```
erl -pa path/to/beam/files -pa other/path ...
```

Si vous regardez l'aide d'`iex` vous ne trouverez pas d'équivalent à cette option. En fait il faut utiliser `--erl` qui permet de passer des options directement à Erlang. Dans notre cas, nous aurons donc ceci :

```
$ iex --erl "-pa _build/shared/lib/mathematics/ebin"
Interactive Elixir (0.11.2) 
    - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> Mathematics.fibonacci(10)
55
```

Notre projet est prêt. Nous lui ajouterons des tests unitaires un peu plus tard. En attendant, comme nous allons l'utiliser comme dépendance pour un second module, je vous propose de placer tout cela dans un dépot `git`.

> Les sources de ce livre se trouvent ici : http://TODO. Le répo de cet exemple est `mathematics-ex`, car nous allons créer un projet similaire pour Erlang, que nous appélerons `mathematics-erl`, afin d'éviter toute confusion.

### Rebar

Contrairement à `mix`, `rebar` ne vient pas automatiquement avec Erlang. Et il faudra donc l'installer à part. Dans les faits, rebar est un _script_ Erlang que vous pouvez télécharger et installer _où bon vous semble_. Cependant vous constaterez que très souvent, `rebar` est fournit dans les sources des projets. C'est ce que nous allons faire ici aussi.

Nous allons appeler la version Erlang de notre module `mathematics-erl`. Commençons donc par créer un répertoire portant ce nom et téléchageons `rebar` dedans :

```
$ mkdir mathematics-erl
$ cd mathematics-erl
$ wget https://github.com/rebar/rebar/wiki/rebar
$ chmod 755 rebar
```

`rebar` fonctionne un peu sur le même modèle que `mix`. Nous allons donc lui passer des commandes afin d'effectuer certaines tâches pour la création, compilation ... Nous pouvons lister les commandes disponibles en utilisant l'option `-c` :

```
$ ./rebar -c
clean                                Clean
compile                              Compile sources

create      template= [var=foo,...]  Create skel based on template and vars
create-app  [appid=myapp]            Create simple app skel
create-node [nodeid=mynode]          Create simple node skel
list-templates                       List available templates

doc                                  Generate Erlang program documentation

check-deps                           Display to be fetched dependencies
get-deps                             Fetch dependencies
update-deps                          Update fetched dependencies
delete-deps                          Delete fetched dependencies
list-deps                            List dependencies

generate    [dump_spec=0/1]          Build release with reltool
overlay                              Run reltool overlays only

generate-upgrade  previous_release=path  Build an upgrade package

generate-appups   previous_release=path  Generate appup files

eunit       [suite=foo]              Run eunit [test/foo_tests.erl] tests
ct          [suites=] [case=]        Run common_test suites in ./test

xref                                 Run cross reference analysis

help                                 Show the program options
version                              Show version information
```

Nous retrouvons ici certaines commandes très similaires à ce que l'on a vu avec `mix`. La commande `create-app` pourrait laisser penser qu'elle permet de créer une nouvelle application. Dans les faits, ce n'est pas _aussi simple_. Et si l'on y regarde de plus prés, on constate que `rebar` fonctionne avec un mécanisme de template. En effet, la commande `create` prend en paramètre un nom de template qui permet de générer différents types de fichiers. Vous pouvez demander à voir la liste de ces templates avec la commande `list-templates` :

```
./rebar list-templates
==> mathematics-erl (list-templates)
Available templates:
  * simplesrv: priv/templates/simplesrv.template (escript)
  * simplenode: priv/templates/simplenode.template (escript)
  * simplemod: priv/templates/simplemod.template (escript)
  * simplefsm: priv/templates/simplefsm.template (escript)
  * simpleapp: priv/templates/simpleapp.template (escript)
  * ctsuite: priv/templates/ctsuite.template (escript)
  * basicnif: priv/templates/basicnif.template (escript)
```

Ces templates, fournis en standard avec `rebar`, peuvent être complétés par d'autres que vous pouvez créer vous-même. En attendant, vous aurez probablement noté la présence du template `simplemod` qui, comme son nom l'indique, permet de générer des fichiers pour un module. Nous nous approchons de ce que nous cherchons. Pour le vérifier, voyons ce que génère `rebar` avec ce template :

```
$ ./rebar create template=simplemod modid=mathematics
==> mathematics-erl (create)
Writing src/mathematics.erl
Writing test/mathematics_tests.erl
```

Comme vous pouvez le voir, c'est sans fioriture, `rebar` ayant généré seulement deux fichiers. Le premier, dans `src`, contenant un squelette de module minimaliste. Le second est un fichier de test que nous reverrons un peu plus loin. En attendant, puisque cela _semble_ correspondre à ce dont nous avons besoin, remplaçons le fichier `src/mathematics.erl` par celui que nous avons développé au chapitre précédent :

```
-module(mathematics).

-export([fibonacci/1]).

fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) when is_integer(N), N > 1 ->
  fibonacci(N-1) + fibonacci(N-2).
```

Nous avons maintenant l'organisation suivante pour notre projet `mathematics-erl` :

```
mathematics-erl
├── rebar
├── src
│   └── mathematics.erl
└── test
    └── mathematics_tests.erl
```

Essayons de compiler le projet via la commande `compile` de `rebar` :

```
$ ./rebar compile
==> mathematics-erl (compile)
```

Si vous regardez à nouveau le contenu de l'arborescence, vous ne constaterez aucune différence après la compilation. Ceci vient du fait que nous n'avons pas d'_application_. En effet, sans application, pas de compilation. Souvenez-vous, lorsque nous avons mis en place le projet Elixir, lors de la compilation, `mix` a généré un fichier `mathematics.app`. C'est un des apports de `mix` : il génère une application si elle n'est pas présente. `Rebar` n'est pas aussi complaisant que `mix`, nous devons donc créer le fichier d'application nous même. Pour cela nous ajoutons un fichier `mathematics.app.src` dans les sources de notre projet. Pour cela nous pouvons nous inspirer de celui généré par `mix` dans notre version Elixir :

```
{application,mathematics,
 [{registered,[]},
  {description,"mathematics"},
  {vsn,"0.0.1"},
  {modules,['Elixir.Mathematics']},
  {applications,[kernel,stdlib,elixir]}]}.
```

Nous verrons en détail ce type de fichier plus tard. Nous allons nous contenter de le copier en de le modifier légèrement de façon à ce qu'il match avec la version Erlang. Pour cela nous allons supprimer la référence au module `Elixir.Mathematics` et nous supprimerons également l'utilisation d'`elixir` dans la liste des `applications` :

```
{application,mathematics,
 [{registered,[]},
  {description,"mathematics"},
  {vsn,"0.0.1"},
  {modules,[]},
  {applications,[kernel,stdlib]}]}.
```

Si maintenant nous relançons la compilation, nous obtenons la sortie suivante :

```
$ ./rebar compile
==> mathematics-erl (compile)
Compiled src/mathematics.erl
```

C'est déjà beaucoup plus encourageant. Et en effet, nous voyons qu'est apparu un répertoire `ebin` contenant `mathematics.app` et `mathematics.beam` :

```
mathematics-erl
├── ebin
│   ├── mathematics.app
│   └── mathematics.beam
├── rebar
├── src
│   ├── mathematics.app.src
│   └── mathematics.erl
└── test
    └── mathematics_tests.erl
```

Nous pouvons tester notre module :

```
$ erl -pa ebin
1> mathematics:fibonacci(10).
55
```

Notre module est prêt et fonctionnel. Tout comme nous avons créé un repo git pour `mathematics-ex`, créez un dépôt pour `mathematics-erl`.

## Tests unitaires

Il ne viendrait à l'idée de personne de diffuser du code sans fournir une couverture de tests unitaires complète. De même que personne n'aurait osé faire le travail à l'envers, comme nous vennons de le faire, et aurait donc tout fait en TDD. Ici c'est trop tard. Mais mieux vaut tard que jamais. Mettons donc en place des tests unitaires.

### Elixir

Lors de la génération de notre projet avec `mix`, ce dernier a créé un fichier de test minimaliste (`test/mathematics_test.exs`) :

```
defmodule MathematicsTest do
  use ExUnit.Case

  test "the truth" do
    assert(true)
  end
end
```

Le contenu est assez explicite. Chaque test commence par un bloc `test` suivi d'une chaine d'identification (`"the truth"` dans le cas présent). Le corps du test contenant des assertions mises en place via la fonction `assert`, cette dernière prenant en paramètre une expression booléenne qui sera soit vraie pour un test qui passe, soit fausse en cas d'échec. Il existe, bien heureusement, d'autres types d'assertion possible dans le module `ExUnit`[^EXUNIT_ASSERTIONS].

[^EXUNIT_ASSERTIONS]: [http://elixir-lang.org/docs/master/ExUnit.Assertions.html](http://elixir-lang.org/docs/master/ExUnit.Assertions.html)

`test` est en fait une _macro_. Mais vous pouvez très bien la remplacer par une fonction. Pour cela, la seule obligation est que le nom de votre fonction commence obligatoirement par `test_`. Nous pouvons donc réécrire le fichier `test/mathematics_test.exs` de la façon suivante :

```
defmodule MathematicsTest do
  use ExUnit.Case

  def test_the_truth(_) do
    assert(true)
  end
end
```

Nous garderons la notation utilisant la macro `test`.

Supprimons le test par défaut (`the truth`) et remplaçons-le par deux vrais tests :

```
defmodule MathematicsTest do
  use ExUnit.Case

  test "Test suite" do
    assert(0 == Mathematics.fibonacci(0))
    assert(1 == Mathematics.fibonacci(1))
    assert(1 == Mathematics.fibonacci(2))
    assert(2 == Mathematics.fibonacci(3))
    assert(3 == Mathematics.fibonacci(4))
    assert(5 == Mathematics.fibonacci(5))
    assert(8 == Mathematics.fibonacci(6))
    assert(13 == Mathematics.fibonacci(7))
    assert(21 == Mathematics.fibonacci(8))
    assert(34 == Mathematics.fibonacci(9))
    assert(55 == Mathematics.fibonacci(10))
  end

  test "Random test" do
    n = :random.uniform(10)
    assert(Mathematics.fibonacci(n) == Mathematics.fibonacci(n-1) + Mathematics.fibonacci(n-2))
  end
end
```

Le premier tests `"Test suite"` nous permet de vérifier que la fonction `fibonacci` renvoie le résultat espéré pour les 10 premiers termes de la suite. Le second (`"Random test"`) est tout aussi simple et permet de vérifier que pour un terme choisis au hasard, il est bien égale à la somme des deux précédents.

Pour exécuter la suite de tests, il suffit d'appeler la commande `test` de `mix` :

```
$ mix test
..

Finished in 0.07 seconds (0.07s on load, 0.00s on tests)
2 tests, 0 failures
```

Nos tests sont passés. Voyons maintenant comment se comporte `ExUnit` dans le cas d'un test qui échoue. Pour cela nous allons modifier le premier test (`"Test suite"`) en forcer une erreure lors du calcul du quatrième terme en remplaçant le résultat attendu (`2`) par `3`. Si nous relançons le test, nous avons le résultat suivant :

```
mix test
.

  1) test Test suite (MathematicsTest)
     ** (ExUnit.ExpectationError)
                  expected: 3
       to be equal to (==): 2
     at test/mathematics_test.exs:8



Finished in 0.08 seconds (0.07s on load, 0.01s on tests)
2 tests, 1 failures
```

`Mix` nous indique donc que le test de la ligne 8 a échoué et que le résultat attendu n'est pas le bon.

> Dans cet exemple, nous avons utilisé la suite `ExUnit`[^EXUNIT_DOC] fournie avec Elixir. Il existe d'autre frameworks de tests. Vous pouvez par exemple regarder le projet `Amrita`[^AMRITA].

[^EXUNIT_DOC]: [http://elixir-lang.org/getting_started/ex_unit/1.html](http://elixir-lang.org/getting_started/ex_unit/1.html)

[^AMRITA]: [http://amrita.io/](http://amrita.io/)

### Erlang

Lors de l'utilisation du template `simplemod`, `rebar` a généré un fichier `test/mathematics_tests.erl`. Si vous en regardez le contenu, vous constaterez qu'il est relativement succinct :

```
-module(mathematics_tests).
-include_lib("eunit/include/eunit.hrl").
```

Il n'y a même pas de test par défaut.

Tout comme Elixir vient avec `ExUnit`, Erlang vient avec `EUnit`[^EUNIT_DOC]. Si vous explorez différents projets Erlang, vous constaterez qu'il y a deux écoles : l'une qui sépare les tests unitaires et l'autre qui les place dans le module à tester. Dans ce qui suis, nous allons séparer les tests ; libre à vous de changer en fonction de vos goûts.

[^EUNIT_DOC]: [http://www.erlang.org/doc/apps/eunit/chapter.html](http://www.erlang.org/doc/apps/eunit/chapter.html)

Pour ajouter un test dans le fichier `test/mathematics_tests.erl`, il suffit de créer des fonctions dont le nom se termine par `_test`. Dans ces fonctions, nous utiliserons les assertions d'`EUnit`[^EUNIT_ASSERTIONS]. Si nous voulons ajouter le test `the truth` nous modifierons donc le fichier de la façon suivante :

[^EUNIT_ASSERTIONS]: [http://www.erlang.org/doc/apps/eunit/chapter.html#Assert_macros](http://www.erlang.org/doc/apps/eunit/chapter.html#Assert_macros)

```
-module(mathematics_tests).
-include_lib("eunit/include/eunit.hrl").

the_truth_test() ->
  ?assert(true).
```

Nous pouvons donc mettre en place des tests identiques à ce que nous avons fait pour la version Elixir :

```
-module(mathematics_tests).
-include_lib("eunit/include/eunit.hrl").

suite_test() ->
  ?assertEqual(0, mathematics:fibonacci(0)),
  ?assertEqual(1, mathematics:fibonacci(1)),
  ?assertEqual(1, mathematics:fibonacci(2)),
  ?assertEqual(2, mathematics:fibonacci(3)),
  ?assertEqual(3, mathematics:fibonacci(4)),
  ?assertEqual(5, mathematics:fibonacci(5)),
  ?assertEqual(8, mathematics:fibonacci(6)),
  ?assertEqual(13, mathematics:fibonacci(7)),
  ?assertEqual(21, mathematics:fibonacci(8)),
  ?assertEqual(34, mathematics:fibonacci(9)),
  ?assertEqual(55, mathematics:fibonacci(10)).

random_test() ->
  N = random:uniform(10),
  ?assertEqual(mathematics:fibonacci(N),
               mathematics:fibonacci(N-1) + 
               mathematics:fibonacci(N-2)).
```

Pour exécuter nos tests, nous allons utiliser la commande `eunit` de `rebar`. 

```
$ ./rebar eunit
==> mathematics-erl (eunit)
  All 2 tests passed.
```

Nos tests sont passés ; vous pouvez mettre à jour le repo git.

> Si vous y regardez de plus près, vous noterez que `rebar` propose également la commande `ct` pour lancer `common_test`[^COMMON_TEST]. Il s'agit là d'un autre framework de tests, que je vous laisse le soin de découvrir par vous-même. 

[^COMMON_TEST]: [http://www.erlang.org/doc/apps/common_test/basics_chapter.html](http://www.erlang.org/doc/apps/common_test/basics_chapter.html)

Tout comme dans l'exemple avec Elixir, voyons comment ce comporte `EUnit` lorsqu'un test échoue :

```
./rebar eunit
==> mathematics-erl (eunit)
mathematics_tests: suite_test...*failed*
in function mathematics_tests:'-suite_test/0-fun-3-'/1 
    (test/mathematics_tests.erl, line 8)
in call from mathematics_tests:suite_test/0 
    (test/mathematics_tests.erl, line 8)
**error:{assertEqual_failed,
         [{module,mathematics_tests},
          {line,8},
          {expression,"mathematics : fibonacci ( 3 )"},
          {expected,3},
          {value,2}]}


=======================================================
  Failed: 1.  Skipped: 0.  Passed: 1.
ERROR: One or more eunit tests failed.
```

Bien qu'un peu plus verbeux que le résultat d'`ExUnit`, nous retrouvons ici aussi toutes les informations nous permettant de savoir quel test a échoué.

## Gestion de dépendances

### Codage de Fibonacci

Maintenant que nous avons des modules, voyons comment les utiliser. Pour cela le plus simple est de créer un nouveau module ayant `mathematics` comme dépendance. Puisque nous avons un module capable de générer des éléments de la suite de Fibonacci, je vous propose de créer un module `coding` permettant de faire un codage de Fibonacci[^CODAGE_FIBONACCI].

[^CODAGE_FIBONACCI]: [http://en.wikipedia.org/wiki/Fibonacci_coding](http://en.wikipedia.org/wiki/Fibonacci_coding)

Le principe de ce codage consiste à utiliser les nombres de la suite de Fibonacci. Ainsi pour coder la valeur 50, nous prenons l'ensemble des éléments de la suite inférieurs ou égaux à 50 ; soit `[1, 2, 3, 5, 8, 13, 21, 34]`. Nous allons parcourir cette liste, dans le sens inverse, et pour chaque valeur rencontrée, si elle est inférieure ou égale au nombre restant à coder, nous allons remplacer la valeur dans la liste par un 1 après l'avoir substitué au nombre restant à coder. Sinon, nous plaçons un 0 dans la liste.

Détaillons la séquence de codage.

> L'étoile (`*`) donne la position courante.

**Etat initial**

`N = 50`

| 1   | 2   | 3   | 5   | 8   | 13  | 21  | 34  |
| :-: | :-: | :-: | :-: | :-: | :-: | :-: | :-: |
|     |     |     |     |     |     |     |  *  |

Ici, nous avons bien `N >= 34` donc nous plaçons la valeur 1 à cette position, et la nouvelle valeur de `N` devient `50 - 34 = 16`.

**Itération 1**

`N = 16`

| 1   | 2   | 3   | 5   | 8   | 13  | 21  | 34  |
| :-: | :-: | :-: | :-: | :-: | :-: | :-: | :-: |
|     |     |     |     |     |     |  *  |  1  |

Ici, `N < 21`, nous codons donc la position courante à 0, nous ne modifions pas `N`. 

**Itération 2**

`N = 16`

| 1   | 2   | 3   | 5   | 8   | 13  | 21  | 34  |
| :-: | :-: | :-: | :-: | :-: | :-: | :-: | :-: |
|     |     |     |     |     |  *  |  0  |  1  |

Vous avez compris le principe. Voici un tableau résumant le codage :

| Itération |  N  | 1   | 2   | 3   | 5   | 8   | 13  | 21  | 34  |
| :-------- | :-: | :-: | :-: | :-: | :-: | :-: | :-: | :-: | :-: |
| Début     | 50  |     |     |     |     |     |     |     |  *  |
| 1         | 16  |     |     |     |     |     |     |  *  |  1  |
| 2         | 16  |     |     |     |     |     |  *  |  0  |  1  |
| 3         | 3   |     |     |     |     |  *  |  1  |  0  |  1  |
| 4         | 3   |     |     |     |  *  |  0  |  1  |  0  |  1  |
| 5         | 3   |     |     |  *  |  0  |  0  |  1  |  0  |  1  |
| 6         | 0   |     |  *  |  1  |  0  |  0  |  1  |  0  |  1  |
| 6         | 0   |  *  |  0  |  1  |  0  |  0  |  1  |  0  |  1  |
| Fin       | 0   |  0  |  0  |  1  |  0  |  0  |  1  |  0  |  1  |

La valeur 50 sera donc codée `[0, 0, 1, 0, 0, 1, 0, 1]`. Nous rajouterons à ce tableau un 1 terminal, ce qui donnera `[0, 0, 1, 0, 0, 1, 0, 1, 1]`. En effet, non seulement la séquence `1, 1` ne peut pas apparaitre lors du codage d'un nombre, mais en plus la plus grande valeur du tableau étant forcement positionnée à `1`, la séquence `1, 1` servira de délimiteur. Ainsi `[0, 1, 1, 1, 1]` code en fait `[0, 1]` (soit 2) et `[1]` (soit 1).

Maintenant que nous avons le principe, nous allons donc créer deux projets. Le premier pour Elixir, que l'on nommera `coding-ex` et qui proposera un module `Coding` exportant une unique fonction `fibonacci`. Cette fonction prenant en paramètre le nombre à coder. Pour Erlang, nous créons un projet `coding-erl` proposant le module `coding` exportant la fonction `fibonacci`. Comme pour la version Elixir, la fonction `coding:fibonacci` d'Erlang prendra en paramètre la valeur du nombre à coder.

Nous ne redétaillerons pas la mise en place des projets, il suffit d'appliquer ce que nous avons vu dans la première partie de ce chapitre. Voyons tout de même le code spécifique des modules.

Elixir: (coding.ex)
```
defmodule Coding do
  def fibonacci(n) when n >= 1 do
    fs = Enum.reverse(gen_suite(n))
    r = [1]
    encode(n, fs, r)
  end

  defp gen_suite(n) do
    gen_suite(n, 2, [])
  end

  defp gen_suite(n, c, s) do
    f = Mathematics.fibonacci(c)
    if n >= f do
      gen_suite(n, c + 1, s ++ [f])
    else
      s
    end
  end

  defp encode(n, [cf|rf], r) when n >= cf do
    encode(n - cf, rf, [1] ++ r)
  end
  defp encode(n, [cf|rf], r) when n < cf do
    encode(n, rf, [0] ++ r)
  end
  defp encode(_, [], r), do: r
end
```

Erlang: (coding.erl) 
```
-module(coding).

-export([fibonacci/1]).

% Public

fibonacci(N) when N >= 1 ->
  FS = lists:reverse(gen_suite(N)),
  R = [1],
  encode(N, FS, R).

% Private

gen_suite(N) ->
  gen_suite(N, 2, []).
gen_suite(N, C, S) ->
  F = mathematics:fibonacci(C),
  if
    N >= F ->
      gen_suite(N, C + 1, S ++ [F]);
    true ->
      S
  end.

encode(N, [CF|RF], R) when N >= CF ->
  encode(N - CF, RF, [1] ++ R);
encode(N, [CF|RF], R) when N < CF ->
  encode(N, RF, [0] ++ R);
encode(_, [], R) -> R.
```

Ces deux codes sont très similaires. Dans la fonction `fibonacci` nous générons les éléments de la suite de Fibonacci dont la plus grand valeur au supérieure ou égale à celle passée en paramètre. Pour cela nous utilisonsla fonction `fibonacci` du module `Mathematics` (respectivement `mathematics`). Par la suite, nous encodons la valeur donnée en paramètre. En retour nous recevons une liste correspondant au codage :

Elixir:
```
iex(3)> Coding.fibonacci(50)
[0,0,1,0,0,1,0,1,1]
```

Erlang:
```
3> coding:fibonacci(50).
[0,0,1,0,0,1,0,1,1]
```

### Gestion de dépendances avec Elixir

Pour Elixir, dans le projet `coding-ex` le fichier `mix.exs` ressemble à ceci :

```
defmodule Coding.Mixfile do
  use Mix.Project

  def project do
    [ app: :coding,
      version: "0.0.1",
      elixir: "~> 0.11.2",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    []
  end

  # Returns the list of dependencies in the format:
  # { :foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1" }
  #
  # To specify particular versions, regardless of the tag, do:
  # { :barbat, "~> 0.1", github: "elixir-lang/barbat.git" }
  defp deps do
    [ ]
  end
end
```

Comme nous l'avons vu précédement lors de la création du module `mathematics`, la liste des dépendances est donnée par la clé `deps` du dictionnaire renvoyé par la fonction `project`. Cette liste est elle même renvoyée par la fonction privée `deps`. `mix` faisant bien les choses, il a détaillé le format attendu pour les dépendances. Cette écriture peut se résumer de la façon suivante :

```
{ :<app name>, <scm>: "<URL>", [options] }
```

Dans notre cas, nous aurons donc :

```
{:mathematics, git: "http://TODO/mathematics-ex.git"}
``` 

La fonction `deps` du fichier `mix.exs` sera donc modifiée de la façon suivante :

```
defp deps do
  [ 
    {:mathematics, git: "http://TODO/mathematics-ex.git"}
  ]
end
```

Si vous avez plusieurs dépendances, il suffit de les rajouter dans la liste renvoyée par `deps`.

La dépendance étant définie, nous pouvons essayer de compiler le projet :

```
$ mix compile
Unchecked dependencies for environment dev:
* mathematics (http://TODO/mathematics-ex.git)
  the dependency is not available, run `mix deps.get`
** (Mix) Can't continue due to errors on dependencies
```

Il va falloir faire un petit effort, car les dépendances ne sont pas _automatiquement_ récupérées. C'est à nous de le faire explicitement, en utilisant la commande `deps.get` :

```
$ mix deps.get
* Getting mathematics (http://TODO/mathematics-ex.git)
Cloning into '/examples/coding-ex/deps/mathematics'...
remote: Counting objects: 10, done.
remote: Compressing objects: 100% (6/6), done.
remote: Total 10 (delta 0), reused 0 (delta 0)
Checking connectivity... done
* Compiling mathematics
Compiled lib/mathematics.ex
Generated mathematics.app
```

Maintenant, nous pouvons compiler :

```
$ mix compile
Compiled lib/coding.ex
Generated coding.app
```

Nous terminons en ajoutant un test :

(coding_test.ex):

```
defmodule CodingTest do
  use ExUnit.Case

  test "simple" do
    assert([0,0,1,0,0,1,0,1,1] == Coding.fibonacci(50))
  end
end
```

Pour nous executons ces tests :

```
$ mix test
.

Finished in 0.07 seconds (0.07s on load, 0.00s on tests)
1 tests, 0 failures
```

Si jamais vous souhaitez utiliser le shell pour faire des tests, il faudra, en plus du répertoire contenant les `beam` du projet `coding-ex`, passer à Erlang le chemin vers les `beam` des dépendances. Or, vous remarquerez qu'à l'issue de la commande `deps.get`, `mix` a créé un répertoire `deps` contenant les dépendances. Lors de la compilation, les dépendances sont compilées dans un sous répertoire du répertoire de compilation (`_build`) : 

```
coding-ex
├── README.md
├── _build
│   └── shared
│       └── lib
│           ├── coding
│           │   └── ebin
│           │       ├── Elixir.Coding.beam
│           │       └── coding.app
│           └── mathematics
│               └── ebin
│                   ├── Elixir.Mathematics.beam
│                   └── mathematics.app
├── deps
│   └── mathematics
│       ├── README.md
│       ├── lib
│       │   └── mathematics.ex
│       ├── mix.exs
│       └── test
│           ├── mathematics_test.exs
│           └── test_helper.exs
├── lib
│   └── coding.ex
├── mix.exs
├── mix.lock
└── test
    ├── coding_test.exs
    └── test_helper.exs
```

Donc, pour lancer le shell, et avoir accès à nos modules, nous utiliserons la commande :

```
iex --erl "-pa _build/shared/lib/mathematics/ebin -pa _build/shared/lib/coding/ebin"
```

Dans le cas présent nous avons seulement deux modules, c'est donc assez simple. Mais imaginer la taille de la commande à taper si nous avons plusieurs disaines de modules. Heureusement il possible de remplacer tout cela par une simple commande :

```
iex -S mix
```

### Gestion des dépendances avec Erlang

Pour gérer les dependances avec `rebar` nous allons devoir créer un fichier indiquant comment récupérer les dépendances. Ce fichier sera nommé `rebar.config` et sera placé à la racine de notre projet. 

`rebar.config` est un fichier, au format Erlang, contient un ensemble de paramètres à passer à `rebar`. Cela peut être des options de compilation, mais également des informations sur les dépendances.

Dans votre projet `coding-erl` nous allons ajouter un fichier `rebar.config` avec le contenu suivant : 

```
{erl_opts, [debug_info]}.
{cover_enabled, true}.
{deps, [
  {mathematics, ".*", 
   {git, "http://TODO/mathematics-erl.git", "master"}}
]}.
```

Si vous consultez la documentation de `rebar`, vous constaterez qu'il existe un grand nombre d'options[^REBAR_OPTIONS]. Dans notre fichier, nous en utilisons trois :

* `erl_opts` qui prend, comme valeur, un tableau d'options qui sera passé au compilateur Erlang[^ERLANG_COMPILER]. 
* `cover_enabled` qui permet d'activer le _code coverage_ lors de l'exécution de la suite de tests unitaires.
* `deps` permettant de définir les dépendances.

[^REBAR_OPTIONS]: [https://github.com/rebar/rebar/wiki/Rebar-commands#options](https://github.com/rebar/rebar/wiki/Rebar-commands#options)

[^ERLANG_COMPILER]: [http://www.erlang.org/doc/man/compile.html](http://www.erlang.org/doc/man/compile.html)

La gestion des dépendances est relativement riche[^REBAR_DEPS]. Nous définissons une dépendance via un tuple précisant :

* Le nom de l'application de la dépendance (`mathematics` pour nous).
* La version. Il s'agit en fait d'une expression régulière devant matcher avec la version. Ici nous utilisons `".*"` ce qui laisse dire que la version nous importe peu.
* La source. Ici nous utilisons un repo git, sur la branche master. Si vous souhaitez utiliser un tag ou une branche en particulier, vous pouvez remplacer `"master"` par quelque chose comme `{tag, "2.3.2"}` ou `{branch, "my_branch"}`.

[^REBAR_DEPS]: [https://github.com/rebar/rebar/wiki/Dependency-management](https://github.com/rebar/rebar/wiki/Dependency-management)

Tout comme nous avons dû demander à récupérer les dépendances avec `mix`, nous devrons explicitement le faire, aussi, avec `rebar` :

```
$ ./rebar get-deps
==> coding-erl (get-deps)
Pulling mathematics from {git,"http:/TODO/mathematics-erl.git",
                              "master"}
Cloning into 'mathematics'...
Checking connectivity... done
==> Entering directory `/examples/coding-erl/deps/mathematics'
==> mathematics (get-deps)
==> Leaving directory `/examples/coding-erl/deps/mathematics'
```

Puis nous pouvons compiler :

```
$ ./rebar compile
==> Entering directory `/examples/coding-erl/deps/mathematics'
==> mathematics (compile)
Compiled src/mathematics.erl
==> Leaving directory `/examples/coding-erl/deps/mathematics'
==> coding-erl (compile)
Compiled src/coding.erl
```

Il nous reste a écrire un fichier de tests :

(coding_tests.erl):
```
-module(coding_tests).
-include_lib("eunit/include/eunit.hrl").

coding_test_() ->
  {setup,
    fun setup/0, fun teardown/1,
    [
      ?_test(simple())
    ]}.

% Tests

simple() ->
  ?assertEqual([0,0,1,0,0,1,0,1,1], coding:fibonacci(50)).

% Helpers

setup() ->
  ok.

teardown(_) ->
  ok.
```

Ce code est légèrement différente de celui que nous avons vu précédement dans ce chapitre. En effet, nous avons ici une fonction `coding_test_` qui sert de point d'entrée à nos tests. Cette fonction renvoie une tuple dont le premier élément indique que le contenu du tuple sert initialiser (`setup`) pour la suite de tests. Le second élément est la fonction qui sera appelée avant l'execution des différentes fonctions de tests, le troisième indique la fonction qui sera appelée à la fin de l'execution en enfin nous avons un tableau contenant la liste des tests. Cette liste est donnée en utilisant la macro `?_text` prenant ne paramètre une fonction décrivant un test (`simple` dans le cas présent).

Cette écriture de tests et beaucoup plus riche que celle que nous avons vu jusqu'à present. Il est possible d'aller encore plus loin[^EUNIT_FIXTURE].

[^EUNIT_FIXTURE]: [http://www.erlang.org/doc/apps/eunit/chapter.html#Fixtures](http://www.erlang.org/doc/apps/eunit/chapter.html#Fixtures)

Notre test étant écrit, nous pouvons demander a `rebar` de l'executer :

```
$ ./rebar eunit
==> Entering directory `/examples/coding-erl/deps/mathematics'
==> mathematics (eunit)
Compiled src/mathematics.erl
Compiled test/mathematics_tests.erl
  All 2 tests passed.
Cover analysis: 
  /examples/coding-erl/deps/mathematics/.eunit/index.html
==> Leaving directory `/examples/coding-erl/deps/mathematics'
==> coding-erl (eunit)
Compiled src/coding.erl
Compiled test/coding_tests.erl
  Test passed.
Cover analysis: /examples/coding-erl/.eunit/index.html
```

Notez ici deux choses : 

1. Nous avons demandé à avoir un résultat sur la couverture de tests. Elle est disponible dans `.eunit/index.html`.
2. Non seulement `rebar` a exécuté les tests pour le projet courant (`coding-erl`) mais il a également lancé ceux des dépendances (`mathematics`). En soi, c'est plutôt une bonne chose. Cependant, il est possible, dans certaines conditions, que vous ne souhaitiez pas les exécuter. Pour cela il suffit de le préciser lors du lancement de la commande `eunit`, en positionnant le paramètre `skip_deps` à `true` (`skip_deps=true`).

Enfin, si vous voulez faire quelques tests via le shell, il faudra utiliser l'option `-pa` d'`erl`. Contrairement à Elixir, qui compile tout dans des sous-répertoires de `_build`, Erlang génère les `beam` dans un sous répertoire `ebin` de chaque dépendance :

```
coding-erl
├── README.md
├── deps
│   └── mathematics
│       ├── README.md
│       ├── ebin
│       │   ├── mathematics.app
│       │   └── mathematics.beam
│       ├── rebar.config
│       ├── src
│       │   ├── mathematics.app.src
│       │   └── mathematics.erl
│       └── test
│           └── mathematics_tests.erl
├── ebin
│   ├── coding.app
│   ├── coding.beam
│   └── mathematics.app
├── rebar.config
├── src
│   ├── coding.app.src
│   └── coding.erl
└── test
    └── coding_tests.erl
```

Nous lancerons donc le shell avec la commande suivante :

```
erl -pa ebin -pa deps/*/ebin
```

## Utiliser conjointement Erlang et Elixir

Comme cela a été annoncé au début de ce chapitre, nous avons clairement séparer Erlang et Elixir dans ce chapitre. Cependant, Elixir étant un langage au dessus d'Erlang, il est heureux de penser que nous pouvons les _mélanger_. Les modules que nous avons développé dans ce chapitre sont relativement simple. Mais nous pouvons imaginer des cas d'utilisation pour lesquels il pourrait être avantageux d'utiliser un module Erlang dans Elixir, ou inversement. Nous allons donc voir comment, dans le module Elixir `Coding` nous pouvons utiliser le module `mathematics` d'Erlang et comment utiliser le module `Mathematics` d'Elixir dans le module Erlang `coding`.

### Erlang dans Elixir

Utiliser un module Erlang dans un projet Elixir ne demandera aucun effort particulier. Nous déclarerons une dépendance Erlang tout comme une dépendance Elixir. `Mix` se chargant de la récupérer et de la compiler. La seule différence viendra de l'utilisation. En effet, avec Erlang, pour appeler une fonction d'un module nous utilisons la syntaxe :

```
module:fonction(...)
```

`module` est en fait un atome. Donc si nous voulons faire le même appel dans Elixir il suffira d'utiliser la syntaxe :

```
:module.fonction(...)
```

Créons donc un clone de l'arborescence de notre module `Coding` et remplaçons la dépendance au module `Mathematics` d'Elixir par celle au module `mathematics` d'Erlang. Pour cela il suffit de modifier le fichier `mix.exs` en remplaçant la ligne :

```
{:mathematics, git: "http://TODO/mathematics-ex.git"}
```

par

```
{:mathematics, git: "http://TODO/mathematics-erl.git"}
```

Maintenant que nous avons fait ce changement, il faut modifier le code du fichier `coding.ex` en remplaçant l'appel `Mathematics.fibonacci` par `:mathematics.fibonacci`. Nous avons donc le code suivant :

Elixir (coding.ex):
```
defmodule Coding do
  def fibonacci(n) when n >= 1 do
    fs = Enum.reverse(gen_suite(n))
    r = [1]
    encode(n, fs, r)
  end

  defp gen_suite(n) do
    gen_suite(n, 2, [])
  end

  defp gen_suite(n, c, s) do
    f = :mathematics.fibonacci(c)
    if n >= f do
      gen_suite(n, c + 1, s ++ [f])
    else
      s
    end
  end

  defp encode(n, [cf|rf], r) when n >= cf do
    encode(n - cf, rf, [1] ++ r)
  end
  defp encode(n, [cf|rf], r) when n < cf do
    encode(n, rf, [0] ++ r)
  end
  defp encode(_, [], r), do: r
end
```

Nous pouvons maintenant récupérer la dépendance, compiler et tester :

```
$ mix deps.get
* Getting mathematics (http://TODO/mathematics-erl.git)
Cloning into '/examples/03/coding-ex2/deps/mathematics'...
remote: Counting objects: 15, done.
remote: Compressing objects: 100% (12/12), done.
remote: Total 15 (delta 2), reused 0 (delta 0)
Checking connectivity... done.

$ mix compile
* Compiling mathematics
==> mathematics (compile)
Compiled src/mathematics.erl
Compiled lib/coding.ex
Generated coding.app

$ mix test
.

Finished in 0.05 seconds (0.05s on load, 0.00s on tests)
1 tests, 0 failures
```

L'effort a fournir est donc très faible. 

Poussons un peu plus loin en regardant comment utiliser un fichier Erlang dans notre projet, en dehors de toute dépendance. Imaginons en effet que nous ayons récupéré un bout de code Erlang et que nous ne souhaitions pas le _traduire_ en Elixir afin de l'utiliser tel quel dans notre projet. 

Pour l'exemple, imaginons que nous ayons un fichier Erlang dans lequel se trouve la fonction de codage :

Erlang (fib.erl):
```
-module(fib).

-export([encode/3]).

encode(N, [CF|RF], R) when N >= CF ->
  encode(N - CF, RF, [1] ++ R);
encode(N, [CF|RF], R) when N < CF ->
  encode(N, RF, [0] ++ R);
encode(_, [], R) -> R.
```

Si nous souhatons utiliser la fonction `fib:encode` dans le module `Coding`, nous le modifions de la façon suivante :

Elixir (coding.ex):
```
defmodule Coding do
  def fibonacci(n) when n >= 1 do
    fs = Enum.reverse(gen_suite(n))
    r = [1]
    :fib.encode(n, fs, r)
  end

  defp gen_suite(n) do
    gen_suite(n, 2, [])
  end

  defp gen_suite(n, c, s) do
    f = :mathematics.fibonacci(c)
    if n >= f do
      gen_suite(n, c + 1, s ++ [f])
    else
      s
    end
  end
end
```

Donc nous avons supprimé la fonction `encode` et nous avons remplacé son appel par un appel `:fib.encode`. Il faut maintenant faire en sorte que le fichier `fib.erl` sont compilé par `mix`. Pour cela il suffit simplement de le placer dans un répertoire `src` situé à la racine du projet. Nous avons donc l'organisation suivante :

```
.
├── lib
│   └── coding.ex
├── mix.exs
├── src
│   └── fib.erl
└── test
    ├── coding_test.exs
    └── test_helper.exs
```

Pour vérifier que tout fonctionne, il suffit de compiler.

```
$ mix deps.get
* Getting mathematics (http://TODO/mathematics-erl.git)
Cloning into '/examples/03/coding-ex3/deps/mathematics'...
remote: Counting objects: 15, done.
remote: Compressing objects: 100% (12/12), done.
remote: Total 15 (delta 2), reused 0 (delta 0)
Checking connectivity... done.

$ mix compile
* Compiling mathematics
==> mathematics (compile)
Compiled src/mathematics.erl
Compiled src/fib.erl
Compiled lib/coding.ex
Generated coding.app

$ mix test
* Compiling mathematics
==> mathematics (compile)
Compiled src/fib.erl
Compiled lib/coding.ex
Generated coding.app
.

Finished in 0.06 seconds (0.06s on load, 0.00s on tests)
1 tests, 0 failures
```

Ici encore, l'effort a été très faible. Il suffit juste de se souvenir que les fichiers Erlang prennent place dans le répertoire `src`.

### Elixir dans Erlang 

L'utilisation d'Elixir dans Erlang n'est malheureusement pas aussi simple. Pour nous en rendre compte, nous allons utiliser une approche naive.

Nous allons donc commencer par cloner l'arborescence de notre module `coding-erl` dans laquelle nous allons remplacer la dépendance au module `mathematics` par son équivalent Elixir. Pour cela nous allons donc remplacer la dépendance dans le fichier `rebar.config` :

(rebar.config) :
```
{erl_opts, [debug_info]}.
{cover_enabled, true}.
{deps, [
  {mathematics, ".*", 
   {git, "http://TODO/mathematics-ex.git", "master"}}
]}.
```

Il faut également modifier le fichier `coding.erl` et remplacer l'appel à la fonction `mathematics:fibonacci` par celui à la fonction éponyme du module `Mathematics` Elixir :

Erlang (coding.erl):
```
-module(coding).

-export([fibonacci/1]).

% Public

fibonacci(N) when N >= 1 ->
  FS = lists:reverse(gen_suite(N)),
  R = [1],
  encode(N, FS, R).

% Private

gen_suite(N) ->
  gen_suite(N, 2, []).
gen_suite(N, C, S) ->
  F = 'Elixir.Mathematics':fibonacci(C),
  if
    N >= F ->
      gen_suite(N, C + 1, S ++ [F]);
    true ->
      S
  end.

encode(N, [CF|RF], R) when N >= CF ->
  encode(N - CF, RF, [1] ++ R);
encode(N, [CF|RF], R) when N < CF ->
  encode(N, RF, [0] ++ R);
encode(_, [], R) -> R.
```

Cet appel se fait sous la forme `'Elixir.Mathematics':fibonacci`. Comme nous l'avons indiqué au chapitre précédent, un appel de fonction Erlang se fait en utilisant le nom atomique du module suivi du nom de la fonction, séparé par un symbole deux-point. Donc pour respecter la convention de nommage des atome en Erlang, Elixir utilisant des majuscules au debut du nom de ses modules, il faut encadrer ce nom entre deux simple quotes. De plus, Elixir compile ses modules dans un name space `Elixir`.

Voyons ce qui se passe, à ce stade, si nous essayons de récupérer la dépendance :

```
$ ./rebar get-deps
==> coding-erl2 (get-deps)
Pulling mathematics from {git,"http://TODO/mathematics-ex.git",
                              "master"}
Cloning into 'mathematics'...
ERROR: Dependency dir /examples/03/coding-erl2/deps/mathematics 
  failed application validation with reason:
{missing_app_file,"/examples/03/coding-erl2/deps/mathematics"}.
ERROR: 'get-deps' failed while processing /examples/03/coding-erl2: 
  rebar_abort
```

`rebar` nous a renvoyé un erreur car il n'a pas trouvé de fichier _application_ dans la dépendance. Nous l'avons vu plus haut dans ce chapitre, `mix` génère ce fichier s'il n'est pas présent, dans le cas présent nous n'en avons donc pas ce qui n'est pas acceptable pour `rebar`.

Pour résoudre ce premier problème, nous allons donc indiquer à `rebar` qu'il faut récupérer la dépendance _tel quelle_. Pour cela nous ajoutons l'options `raw` à ma dépendance :

```
{mathematics, ".*", 
 {git, "http://TODO/mathematics-ex.git", "master"}, 
 [{raw, true}]}
```

Avec une telle déclaration, la dépendance sera récupéré mais `rebar` ne la compilera pas. Nous allons donc ajouter une dépendance a Elixir :

(rebar.config):
```
{erl_opts, [debug_info]}.
{cover_enabled, true}.
{deps, [
  {elixir, ".*", 
   {git, "git://github.com/elixir-lang/elixir", 
    {tag, "v0.12.5"}}},
  {mathematics, ".*", 
   {git, "http://TODO/mathematics-ex.git", "master"}, 
   [{raw, true}]}
]}.
```

Si nous relançons la récupération des dépendances et la compilation nous nous retrouvons avec une installation compléte d'Elixir dans le répertoire `deps/elixir`. Nous pouvons l'utiliser pour compiler le module `mathematics`. Pour cela, il faut invoquer `mix` situé dans le répertoire `deps/elixir/bin`. Il faudra également ajouter le répertoire `bin` dans notre `PATH` afin d'avoir accès au binaires `elixir` et `elixir` utilisé par `mix`.

`rebar` propose un mécanisme de _hook_ permettant d'appeler un script à une étape choisie du processus de création de notre package (`get-deps`, `compile`, `clean` ...). Nous allons utiliser ce mécanisme pour compiler notre dépendance en utilisant un script. Ce script prendra en paramètre :

* Une action. Sachant que pour générer notre module nous faisons une récupération de dépendance, une compilation et un execution de tests, nous coderons les même actions dans notre script.
* Le chemin vers les dépendances. Pour cela, `rebar` nous aide en mettant à notre disposition la variable d'environnement `REBAR_DEPS_DIR` qui contient le chemin vers le répertoire de dépendances.
* Les modules à compiler.


Notre script ressemblera donc à ceci :

(build_ex.sh) :
```
#!/bin/sh

ACTION=$1 ; shift
MODULES=$*

for MODULE in $MODULES ; do
  echo "==> $MODULE ($ACTION)"
  case $ACTION in
    get-deps)
      cd "$REBAR_DEPS_DIR/$MODULE"
      PATH=$PATH:"$REBAR_DEPS_DIR/elixir/bin" mix deps.get 
      ;;
    compile)
      cd "$REBAR_DEPS_DIR/$MODULE"
      PATH=$PATH:"$REBAR_DEPS_DIR/elixir/bin" mix compile 
      ;;
    clean)
      cd "$REBAR_DEPS_DIR/$MODULE"
      PATH=$PATH:"$REBAR_DEPS_DIR/elixir/bin" mix clean
      ;;
    *)
      echo "Unknow action $ACTION"
      ;;
  esac
done
```

Nous placons ce script à la racine du projet et nous l'appellons dans un _hook_ du fichier de configuration `rebar` :

(rebar.config):
```
{erl_opts, [debug_info]}.
{cover_enabled, true}.
{deps, [
  {elixir, ".*", 
   {git, "git://github.com/elixir-lang/elixir", 
    {tag, "v0.12.5"}}},
  {mathematics, ".*", 
   {git, "http://TODO/mathematics-ex.git", 
    "master"}, 
   [{raw, true}]}
]}.
{post_hooks, [
  {'get-deps', "./build_ex.sh get-deps mathematics"},
  {compile, "./build_ex.sh compile mathematics"},
  {clean, "./build_ex.sh clean mathematics"}
]}.
```

> Nous utilisons ici un `post_hook`, les actions seront donc executées après celle de notre projet. Ainsi la compilation se fera dans l'ordre suivant : les dépendances en premier, les sources de notre projet, puis le code du module Elixir. Si vous préférez que le code du module Elixir soit compilé avant le code de notre porjet, vous pouvez utilise un `pre_hook`. Dans le cas présent cela n'a pas beaucoup d'importance.

Nous avons donc un appel pour chaque action. Nous pouvons donc relancer la génération de notre projet :

```
$ ./rebar get-deps
==> coding-erl2 (get-deps)
Pulling elixir from {git,"git://github.com/elixir-lang/elixir",
                         {tag,"v0.12.5"}}
Cloning into 'elixir'...
Pulling mathematics from {git,"http://TODO/mathematics-ex.git",
                              "master"}
Cloning into 'mathematics'...
==> mathematics (get-deps)
All dependencies up to date
==> elixir (get-deps)
==> mathematics (get-deps)

$ ./rebar compile
==> elixir (compile)
==> elixir (compile)
Compiled src/elixir_parser.yrl
Compiled src/elixir_bitstring.erl
...
Compiled lib/iex/server.ex
Generated iex.app
==> coding-erl2 (compile)
Compiled src/coding.erl
==> mathematics (compile)
Compiled lib/mathematics.ex
Generated mathematics.app
```

Dans notre exemple, nous avons utilisé un script shell pour faire la compilation du module Elixir. Bien entendu, à moins travailler avec un shell (via Cygwin par exemple), cela ne fonctionnera pas sous Windows. Pour pallier à ce problème, nous pouvons spécifier le un système dans le _hook_. Ainsi nous pour assurer la postabilité de notre example, nous devrions écrite un script pour Windows (`build_ex.bat` par exemple) et modifier la configuration de `rebar` de la façon suivante :

```
{post_hooks, [
  {"linux", 'get-deps', "./build_ex.sh get-deps mathematics"},
  {"linux", compile, "./build_ex.sh compile mathematics"},
  {"linux", clean, "./build_ex.sh clean mathematics"},
  {"windows", 'get-deps', "./build_ex.bat get-deps mathematics"},
  {"windows", compile, "./build_ex.bat compile mathematics"},
  {"windows", clean, "./build_ex.bat clean mathematics"}
]}.
```

Maintenant que notre projet est compilé, nous pouvons voir comment exécuter les tests. Puisque nous avons choisis de travailler par essaie/erreur, voyons donc ce qui se passe si nous executons les tests :

```
> ./rebar eunit
==> elixir (eunit)
  There were no tests to run.
==> coding-erl2 (eunit)
Compiled test/coding_tests.erl
coding_tests:9: coding_test_...*failed*
in function 'Elixir.Mathematics':fibonacci/1
  called as fibonacci(2)
in call from coding:gen_suite/3 (src/coding.erl, line 17)
in call from coding:fibonacci/1 (src/coding.erl, line 8)
in call from coding_tests:'-simple/0-fun-0-'/1 (test/coding_tests.erl, line 15)
**error:undef


=======================================================
  Failed: 1.  Skipped: 0.  Passed: 0.
ERROR: One or more eunit tests failed.
ERROR: eunit failed while processing /examples/03/coding-erl2: rebar_abort
```

Comme nous pouvions nous y attendre, nous avons le droit à un message d'erreur. Dans le cas présent, il s'agit de la fonction `'Elixir.Mathematics':fibonacci/1` qui n'a pas été trouvée. En effet, le modules `Mathematics` ont été compilé par `mix` qui a donc placé le résultat de la compilation dans le répertoire `deps/mathematics/_build`. Or `rebar` ne connait pas cet emplacement. Nous allons donc devoir lui indiquer le chemin pour y accéder. Pour cela nous disposons du paramètre `lib_dirs` dans `rebar` permettant de donner des chemins _alternatifs_ pour retrouver et charger des modules. Nous devons donc ajouter les lignes suivantes dans le fichier `rebar.config` :

```
{lib_dirs, [
  "deps/mathematics/_build/dev/lib"
]}.
```

Avec ce simple ajout, nos tests passent :

```
$ ./rebar eunit
==> elixir (eunit)
  There were no tests to run.
==> coding-erl2 (eunit)
  Test passed.
```

Terminons en voyant comment nous pouvons travailler conjointement avec du code Erlang et Elixir. Pour ce travail, nous avons vu qu'avec Elixir il suffit de placer le code Erlang dans un répertoire `src`. Bien etendu, cela ne sera pas aussi simple ici. En effet, `rebar` ne _connait_ pas Elixir et tout vomme nous avons du utiliser un artefact pour utiliser une dépendance Elixir, nous allons devoir faire appel à un élément extérieur. Pour cela il existe un _plugin_ `rebar` permettant de compiler du code Elixir dans un projet Erlang : `rebar_elixir_plugin`[^REBAR_EX_PLUGIN].

[^REBAR_EX_PLUGIN]: [https://github.com/yrashk/rebar_elixir_plugin](https://github.com/yrashk/rebar_elixir_plugin)

Pour voir comment l'utiliser, utilisons la même démarche que celle que nous avons vu lors de l'utilisation d'Erlang avec Elixir. Partons donc en imaginant que la fonction d'encodage se trouve dans un module Elixir. Nous avons donc ceci :

Erlang (coding.erl):
```
-module(coding).

-export([fibonacci/1]).

% Public

fibonacci(N) when N >= 1 ->
  FS = lists:reverse(gen_suite(N)),
  R = [1],
  'Elixir.Fib':encode(N, FS, R).

% Private

gen_suite(N) ->
  gen_suite(N, 2, []).
gen_suite(N, C, S) ->
  F = 'Elixir.Mathematics':fibonacci(C),
  if
    N >= F ->
      gen_suite(N, C + 1, S ++ [F]);
    true ->
      S
  end.
```

Ce module faisant appel la la fonction `encode` du module Elixir `Fib` :

Elixir (fib.ex):
```
defmodule Fib do
  def encode(n, [cf|rf], r) when n >= cf do
    encode(n - cf, rf, [1] ++ r)
  end
  def encode(n, [cf|rf], r) when n < cf do
    encode(n, rf, [0] ++ r)
  end
  def encode(_, [], r), do: r
end
```

Nous devons maintenant modifier le fichier `rebar.config` de façon à pouvoir utiliser `rebar_elixir_plugin`. Pour cela il faut comment par ajouter la dépendance. Il faut ensuite indiquer à `rebar` que nous utilisons un plugin via l'option `plugins`. Nous utilisons  l'option `src_dirs` pour préciser ou se trouve le fichier Elixir à compiler (`fib.ex`). Afin de nous simplifier la vie, nous les placerons dans le répertoire `src`. Anfin, comme nous allons demander a `rebar` de compiler du code Elixir, il faut lui préciser ou se trouve les librairies à charger. Pour cela nous allons ajouter le chemin vers la dépendance `elixir` via `lib_dirs`, tout comme nous l'avons fait pour pouvoir utiliser le module `mathematics`. Notre fichier `rebar.config` sera donc le suivant :

(rebar.config):
```
{erl_opts, [debug_info]}.

{deps, [
  {rebar_elixir_plugin, ".*",
    {git, "git://github.com/yrashk/rebar_elixir_plugin"}},
  {elixir, ".*",
    {git, "git://github.com/elixir-lang/elixir",
      {tag, "v0.12.5"}}},
  {mathematics, ".*",
    {git, "http://git.lejeun.es/glejeune/mathematics-ex.git", "master"},
    [{raw, true}]}
]}.

{plugins, [rebar_elixir_compiler, rebar_exunit]}.

{src_dirs, [
  "src"
]}.

{lib_dirs, [
  "deps/elixir/lib",
  "deps/mathematics/_build/dev/lib"
]}.

{post_hooks, [
  {'get-deps', "./build_ex.sh get-deps mathematics"},
  {compile, "./build_ex.sh compile mathematics"},
  {clean, "./build_ex.sh clean mathematics"}
]}.
```

Il ne nous reste plus qu'à récupérer la nouvelle dépendance, lancer la compilation et tester :

```
$ ./rebar get-deps
==> elixir (get-deps)
==> coding-erl3 (get-deps)
Pulling rebar_elixir_plugin from {git,"git://github.com/yrashk/rebar_elixir_plugin"}
Cloning into 'rebar_elixir_plugin'...
==> mathematics (get-deps)
All dependencies up to date
==> rebar_elixir_plugin (get-deps)

$ ./rebar compile
==> rebar_elixir_plugin (pre_compile)
==> rebar_elixir_plugin (compile)
Compiled src/rebar_exunit.erl
Compiled src/rebar_elixir_compiler.erl
==> elixir (pre_compile)
==> elixir (compile)
==> elixir (compile)
==> coding-erl3 (pre_compile)
Compiled src/fib.ex
==> coding-erl3 (compile)
Compiled src/coding.erl
==> mathematics (compile)

$ ./rebar eunit
==> rebar_elixir_plugin (pre_eunit)
==> rebar_elixir_plugin (eunit)
  There were no tests to run.
==> elixir (pre_eunit)
==> elixir (eunit)
  There were no tests to run.
==> coding-erl3 (pre_eunit)
==> coding-erl3 (eunit)
  Test passed.
```
