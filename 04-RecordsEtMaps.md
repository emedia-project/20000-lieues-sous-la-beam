# Records et Maps

Les types que nous avons vus jusqu'à maintenant sont _simples_ et, à l'exception des listes et tuples, ils ne nous permettent pas de créer de systèmes de stockages complexes. La manipulation des listes et tuples, principalement quand ils se _mélangent_, n'est pas toujours très simple à utiliser. Pour faciliter cela, Erlang et Elixir proposent le type `record`.

Avec Erlang 17, nous avons vu arriver les `maps`. Les `maps` sont une digression du type `record`, rendant ce dernier plus générique et permettant de se rapprocher d'un modèle de table de hachage. Dans un souci constant de compatibilité, Elixir a bien entendu repris à son compte les `maps`.

## Les records

### Erlang

Comme vous allez le voir, les `record`s sont en fait du sucre syntaxique pour faciliter l'utilisation de tuples. Pour nous en rendre compte, commençons par créer un `record`. Pour cela, avec Erlang, nous utilisons le mot clé `-record` via lequel nous allons nommer notre `record` et préciser la liste de ses membres :

```
-record(my_record, {
  key1, key2, key3
}).
```

Dans cet exemple nous avons un `record` nommé `my_record` contenant trois _clés_ (`key1`, `key2` et `key3`). Nous pouvons maintenant utiliser ce record de la façon suivante :

```
2> #my_record{}. 
#my_record{key1 = undefined,key2 = undefined,
           key3 = undefined}
```

Nous avons créé un `record` _vide_ ; toutes les clés ont donc comme valeur l'atome `undefined`. Nous pouvons préciser une valeur pour toutes ou partie des clés :

```
3> #my_record{ key1 = un_atome, key3 = "un chaine" }.
#my_record{key1 = un_atome,key2 = undefined,
           key3 = "un chaine"}
```

Nous avons affecté les valeurs pour les clés `key1` et `key3`. `key2`, pour laquelle nous n'avons affecté aucune valeur, prend donc par défaut la valeur `undefined`. Nous aurions très bien pu affecter la valeur par défaut lors de la déclaration du `record` :

```
-record(my_record, {
  key1 = atome, 
  key2 = 0, 
  key3 = "chaine"
}).
```

Dans ce cas, la même affectation aurait donné le résultat suivant :

```
3> #my_record{ key1 = un_atome, key3 = "un chaine" }.
#my_record{key1 = un_atome,key2 = 0,
           key3 = "un chaine"}
```

Il est également possible de spécifier le type de valeur attendu pour une clé :

```
-record(my_record, {
    key1 = atom     :: atom(),
    key2            :: integer(),
    key3 = "chaine" :: string()
}).
```

Nous avons ici un record avec des valeurs par défaut pour les clés `key1` et `key3` et pour lequel nous spécifions, pour chaque clé, le type. Cependant, cette précision est purement documentaire. En effet, Erlang ne dira rien si vous affectez une valeur de type différent :

```
3> #my_record{ key2 = <<"Hello">> }.
#my_record{key1 = atom,key2 = <<"Hello">>,key3 = "chaine"}
```

Pour pouvoir manipuler les valeurs des clés, nous devons stocker le contenu du `record` dans une variable :

```
4> MyRecord = #my_record{ key1 = un_atome, key3 = "un chaine" }.
#my_record{key1 = un_atome,key2 = undefined,
           key3 = "un chaine"}
```

> Peut-être avez vous essayé de créer un record dans le shell, sans succès. En effet, ce n'est pas possible. Pour utiliser un record dans le shell, il faut le décrire dans un fichier et charger ce fichier via la commande `rr`. Pour pouvoir tester les exemples de ce paragraphe, créez un fichier `record.hrl` contenant la déclaration du `record`, puis, dans le shell, tapez la commande `rr("record.hrl").`

Par la suite nous utiliserons la forme `<variable>#<nom du record>.<nom de la clé>` pour récupérer la valeur de la clé `<nom de la clé>` du `record` `<nom du record>` stocké dans la variable `<variable>` :

```
5> LaChaine = MyRecord#my_record.key3.
"un chaine"
```

Nous pouvons également récupérer cette valeur en utilisant les principes de pattern matching :

```
6> #my_record{ key3 = LaChaine1 } = MyRecord.
#my_record{key1 = un_atome,key2 = undefined,
           key3 = "un chaine"}
7> LaChaine1.
"un chaine"
```

Cette méthode de récupération de valeurs dans un record sera surtout utilisée lorsque nous voudrons récupérer la valeur d'une clé dans une fonction : 

```
un_fonction(#my_record{key3 = Chaine}) ->
  % Utilisation de la valeur stockée dans Chaine
```

Jusqu'à maintenant, vous n'avez peut-être toujours pas perçu le lien avec les tuples. Pour voir cela, si vous avez chargé le record dans le shell (via `rr`) et créé la record `MyRecord`, voyons ce qui se passe si nous _déchargeons_ les informations sur le `record` (via la commande `rf`) : 

```
8> MyRecord.
#my_record{key1 = un_atome,key2 = undefined,
           key3 = "une chaine"}
9> rf(my_record).
ok
10> MyRecord.
{my_record,un_atome,undefined,"une chaine"}
```

Après le _déchargement_, quand nous demandons à voir le contenu de `MyRecord`, Erlang nous renvoie un tuple dont le premier élément est le nom du record (`my_record`) et les éléments suivants sont les valeurs des clés, dans leur ordre de déclaration dans le `record`. En effet :

```
11> MyRecord2 = {my_record, un_autre_atome, undefined, 
11> "une autre chaine"}.
{my_record,un_autre_atome,undefined,"une autre chaine"}
12> rr("record.hrl").
[my_record]
13> MyRecord2.
#my_record{key1 = un_autre_atome,key2 = undefined,
           key3 = "une autre chaine"}
```

Un tuple sera donc systématiquement _transformé_ en `record` si le premier élément est un atome correspondant au nom d'un `record` connu. De même un `record` pourra toujours être manipulé comme un tuple : 

```
14> {Name, Key1, Key2, Key3} = MyRecord2.
#my_record{key1 = un_autre_atome,key2 = undefined,
           key3 = "une autre chaine"}
15> Name.
my_record
16> Key1.
un_autre_atome
17> Key2.
undefined
18> Key3.
"une autre chaine"
```

Nous avons vu comment récupérer la valeur associée à une clé. Si maintenant nous souhaitons mettre à jour un record, il faudra utiliser le principe de pattern matching :

```
1> rr("record.hrl").
[my_record]
2> MyRecord = #my_record{key1 = atome, key2 = 123, key3 = "chaine"}.
#my_record{key1 = atome,key2 = 123,key3 = "chaine"}
3> MyRecord1 = MyRecord#my_record{key2 = 567}.
#my_record{key1 = atome,key2 = 567,key3 = "chaine"}
```

Nous avons ici `MyRecord1` qui contient le même contenu que `MyRecord` à l'exception de la valeur de la clé `key2` que nous avons modifié.

Le gros avantage des `record`s, par rapport aux tuples, prend tout son sens lorsque nous avons besoin de stocker manipuler un grand nombre de valeurs dans un tuple, l'accès par _clé_ rend la manipulation beaucoup plus souple.

### Elixir

Avec Elixir, la création d'un `record` se fait en utilisant le mot clé `defrecord` :

```
defrecord MyRecord, key1: :atome, key2: 0, key3: "chaine" do
end
```

Là où le nom d'un record est un atome en Erlang, il est ici nommé en commençant par une majuscule. De plus, il y a _obligatoirement_ une valeur par défaut, les clés étant données sous forme d'une liste de clé/valeur. Notez également la présence du bloc (`do`...`end`) sur lequel nous reviendrons un peu plus tard.

L'utilisation d'un `record` se fait en l'initialisant via le mot clé `new` :

```
iex(2)> my_record = MyRecord.new
MyRecord[key1: :atome, key2: 0, key3: "chaine"]
```

Nous pouvons, lors de la création du `record`, spécifier les valeurs de certaines clés :

```
iex(3)> my_record = MyRecord.new(key1: :an_atome, key3: "une chaine") 
MyRecord[key1: :an_atome, key2: 0, key3: "une chaine"]
```

Elixir n'étant pas contraint par le single assigment, l'accès et la mise à jour d'un record se font de façon beaucoup plus simple qu'avec Erlang :

```
iex(4)> my_record.key2
0
iex(5)> my_record = my_record.key2(123)
MyRecord[key1: :atome, key2: 123, key3: "chaine"]
iex(6)> my_record
MyRecord[key1: :atome, key2: 123, key3: "chaine"]
```

Donc, pour accéder à la valeur d'une clé nous utilisons l'écriture `<variable>.<nom de la clé>`. Et pour modifier la valeur d'une clé, nous utilisons l'écriture `<variable>.<nom de la clé>(<nouvelle valeur>)`. Notez cependant que lors de l'affectation d'une nouvelle valeur, le `record` n'est pas modifié, mais que cet appel renvoie la version modifiée du `record` :

```
iex(4)> my_record.key2(123)
MyRecord[key1: :atome, key2: 123, key3: "chaine"]
iex(5)> my_record
MyRecord[key1: :atome, key2: 0, key3: "chaine"]
iex(6)> my_record = my_record.key2(123)
MyRecord[key1: :atome, key2: 123, key3: "chaine"]
iex(7)> my_record
MyRecord[key1: :atome, key2: 123, key3: "chaine"]
```

Le bloc présent lors de la création d'un `record` n'est pas là par hasard. En effet, il va nous permettre de créer, dans le `record`, des fonctions. Ces fonctions peuvent être de _tout type_ :

```
defrecord MyRecord, key1: :atome, key2: 0, key3: "chaine" do
  def hello(who) do
    "Hello #{who}"
  end
end
```

Nous pouvons utiliser ces fonctions comme nous le faisons pour des fonctions déclarées dans des modules :

```
iex(2)> MyRecord.hello("World")
"Hello World"
```

Heureusement, leur intérêt ne se limite pas à cela ; et ces fonctions prennent tout leur sens quand elles vont nous permettre de manipuler le contenu du `record`. Pour cela il faudra déclarer la fonction en prenant en compte le fait qu'elle recevra en premier paramètre le record :

```
defrecord MyRecord, key1: :atome, key2: 0, key3: "chaine" do
  def hello(who) do
    "Hello #{who}"
  end
  def hello2(record) do
    "Hello #{record.key3}"
  end
end
```

Dans ce cas, nous pouvons utiliser la fonction `hello2` de deux manières. Soit directement en lui passant un `record` :

```
iex(2)> rec = MyRecord.new(key3: "World")
MyRecord[key1: :atome, key2: 0, key3: "World"]
iex(3)> MyRecord.hello2(rec)
"Hello World"
```

Nous avons donc explicitement passé en paramètre le `record` créé précédemment. L'autre solution consiste à appeler la fonction `hello2` à partir d'un `record` existant :

```
iex(2)> rec = MyRecord.new(key3: "World")
MyRecord[key1: :atome, key2: 0, key3: "World"]
iex(3)> rec.hello2
"Hello World"
```

Le passage de paramètre est alors implicite.

C'est également dans le bloc que nous pourrons spécifier le type de chaque clé du `record`. Pour cela nous utilisons `record_type` :

```
defrecord MyRecord, key1: :atome, key2: 0, key3: "chaine" do
  record_type key1: atom, key2: pos_integer, key3: String.t
end
```

Tout comme pour Erlang, ces informations sont purement documentaires, et vous pouvez les transgresser :

```
iex(2)> MyRecord.new(key2: "hello")
MyRecord[key1: :atome, key2: "hello", key3: "chaine"]
```

## Les maps

Les `map`s sont un ajout d'Erlang R17. Alors que nous aurions pu rapprocher les `record`s des structures du C, les `map`s sont plus proche de ce que l'on connait sous le terme de _hachage_ dans d'autres langages. 

Une `map` à une forme d'écriture très proche de celle d'un record :

Erlang :
```
1> Map = #{name => "Greg", age => 40, language => ["Ruby", "Erlang", "Elixir"]}.
```

Elixir :
```
iex(1)> map = %{:name => 'Greg', :age => 40, :language => ['Ruby', 'Erlang', 'Elixir']}
```

> Notez bien la différence de syntaxe entre les deux langages, `#{...}` pour Erlang et `%{...}` pour Elixir.

Bien que l'exemple ci-dessus utilise des atomes pour les clés, vous êtes libre d'utiliser n'importe quel type :

Erlang :
```
1> Map = #{
1>   {erlang, elixir} => easy, 
1>   java => <<"deprecated">>, 
1>   "ruby" => [cool, simple],
1>   #{0 => 'php', [perl, pyhton] => friends} => script
1> }.
#{java => <<"deprecated">>,
  {erlang,elixir} => easy,
  #{0 => php,[perl,pyhton] => friends} => script,
  "ruby" => [cool,simple]}
```

Elixir :
```
iex(1)> map = %{
iex(1)>   {:erlang, :elixir} => :easy,
iex(1)>   :java => "deprecated",
iex(1)>   "ruby" => [:cool, :simple],
iex(1)>   %{0 => :php, [:perl, :pyhton] => :friends} => :script
iex(1)> }
%{:java => "deprecated", {:erlang, :elixir} => :easy,
 %{0 => :php, [:perl, :pyhton] => :friends} => :script,
 "ruby" => [:cool, :simple]}
```

> Avec Elixir, si toutes les clés sont des atomes, nous pouvons simplifier l'écriture en utilisant un liste de type clé/valeur :
> 
>     iex(1)> m = %{name: 'Greg', age: 40, 
>     iex(1)> language: ['Ruby', 'Erlang', 'Elixir']}
>     %{age: 40, language: ['Ruby', 'Erlang', 'Elixir'], name: 'Greg'}

La récupération d'une valeur, associé à une clé, se fait en utilisant les principes de pattern matching avec Erlang. Cependant, il y a une petite différence avec ce que nous avons vu précédemment. En effet, nous n'utiliserons pas l'opérateur `=>` mais `=:` :

```
1> Map = #{name => "Greg", age => 40,
1> language => [erlang, elixir, ruby]}.
#{age => 40,language => [erlang,elixir,ruby],name => "Greg"}
2> #{ name := Name } = Map.
#{age => 40,language => [erlang,elixir,ruby],name => "Greg"}
3> Name.
"Greg"
```

Pour Elixir, le principe est le même, à la différence que nous pouvons _garder_ l'opérateur `=>` :

```
iex(3)> %{:name => name} = map
%{age: 40, language: ['Ruby', 'Erlang', 'Elixir'], name: 'Greg'}
iex(4)> name
'Greg'
```

Nous pouvons également utiliser une notation avec crochets (`[...]`) :

```
iex(1)> map = %{:name => 'Greg', :age => 40,
...(1)> :language => ['Ruby', 'Erlang', 'Elixir']}
%{age: 40, language: ['Ruby', 'Erlang', 'Elixir'], name: 'Greg'}
iex(2)> map[:name]
'Greg'
```

Pour mettre a jour le contenu d'une `map`, avec Erlang, nous utiliserons une notation très proche de celle vue pour les `record`s :

```
4> Map1 = Map#{name => "Bob"}.
#{age => 40,language => [erlang,elixir,ruby],name => "Bob"}
5> Map.
#{age => 40,language => [erlang,elixir,ruby],name => "Greg"}
6> Map1.
#{age => 40,language => [erlang,elixir,ruby],name => "Bob"}
```

Avec Elixir, la méthode est la suivante :

```
iex(5)> map1 = %{map | :name => "bob"}
%{age: 40, language: ['Ruby', 'Erlang', 'Elixir'], name: "bob"}
iex(6)> map
%{age: 40, language: ['Ruby', 'Erlang', 'Elixir'], name: 'Greg'}
iex(7)> map1
%{age: 40, language: ['Ruby', 'Erlang', 'Elixir'], name: "bob"}
```

Les `map`s sont donc très proche des listes de type clé/valeur. Il y a cependant une différence qu'il faut prendre en compte. Avec une liste de type clé/valeur, il est possible d'avoir plusieurs fois la même clé :

Erlang :
```
1> KV = [{one, 1}, {two, 2}].
[{one,1},{two,2}]
2> KV ++ [{one, 3}].
[{one,1},{two,2},{one,3}]
```

Elixir :
```
iex(1)> kv = [one: 1, two: 2]
[one: 1, two: 2]
iex(2)> kv ++ [one: 3]
[one: 1, two: 2, one: 3]
```

Avec les `map`s ceci est impossible. Il faut également noter que les listes de type clé/valeur sont ordonnées ; par défaut dans l'ordre de création des clés. Ce n'est pas le cas des `map`s.

Enfin, sachez que pour chacun des deux langages propose un module pour la manipulation des `map`s (`map` pour Erlang et `Map` pour Elixir). 
