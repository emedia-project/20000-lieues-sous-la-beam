# Bien démarrer

Nous allons donc voir, dans ce chapitre, comment installer Erlang et Elixir _à la main_. Ce type d'installation, outre son aspect une peu rebutante et _old school_ présente un avantage de taille.  Cela permet de s'abstraire des contraintes du packaging et donc de pouvoir bénéficier _au plus tôt_ des dernières évolutions du langage. Si toutefois vous préférez un utiliser un package, pour Erlang je vous conseille vivement d'utiliser les dépôts d'_Erlang Solutions_[^ES_REPOS]. Pour Elixir, si votre distribution ne propose pas de package, vous pourrez toujours vous rabattre sur les versions précompilées disponibles sur GitHub[^EX_RELEASES].

## Installer Erlang

### Linux et OSX

Pour compiler Erlang, nous avons besoin des packages suivants : `gcc`, `g++`, `make`, `libncurses5-dev`, `flex`, `m4`, `openssl`, `unixodbc-dev` et `fop`. Je vous laisse corriger cette liste en fonction de votre distribution, sachant qu’elle correspond exactement aux noms des packages nécessaires sur Ubuntu.

> Dans la suite, nous allons compiler Erlang avec le support de `dtrace`. Pour cela vous aurez besoin du package `systemtap-sdt-dev`. Vous pouvez également opter pour une compilation manuelle, auquel cas je vous recommande l'utilisation de la version de Paul Fox disponible à l'adresse ftp://crisp.dyndns-server.com/pub/release/website/dtrace/

Erlang propose également un _binding_ pour _wxWidgets_[^WXWIDGETS], permettant de réaliser des IHMs. Si vous voulez compiler Erlang avec ce support, vous aurez besoin de _wxWidgets-3.0_. Malheureusement cette version n'est pas (encore) disponible sur Ubuntu 12.04. Il faudra donc le compiler :

```
sudo apt-get install libgtk-3-dev
wget http://downloads.sourceforge.net/project/wxwindows/3.0.0/wxWidgets-3.0.0.tar.bz2
tar xjf wxWidgets-3.0.0.tar.bz2
cd wxWidgets-3.0.0
./configure
make
sudo make install
```

Pour OSX, il faudra avant tout installer Xcode[^XCODE] et les _command line tools_. Pour ce qui est des dépendances, vous pouvez opter pour _brew_[^BREW]. Dans ce cas, il faudra installer les packages `unixodbc` et `fop`. Pour le support de _wxWidgets_, installez également `wxmac`. Vous pouvez bien entendu opter pour une autre solution que _brew_ comme _Mac Ports_[^MACPORTS] par exemple. Avec ce dernier, vous aurez besoin des packages `unixODBC`, `fop` et `wxWidgets-3.0`.

> Je vous conseille également d'installer la dernière version d'`openssl`.

Une fois les dépendances installées, vous pouvez récupérer les sources d'Erlang à l'adresse http://www.erlang.org/download.html. Pour ce livre, j'ai utilisé la version 17.0 car c'est la seule qui permet d'utiliser la dernière version d'Elixir. Si vous souhaitez utiliser une version antérieure, il faudra donc adapter votre installation.

Extrayez le contenu de l'archive et placez-vous dans le répertoire créé :

```
$ tar zxf otp_src_17.0.tar.gz
$ cd otp_src_17.0
```

Nous n'allons pas nous étendre plus que nécessaire sur cette installation. Je ne passerai donc pas en revue les options disponibles pour la compilation, pariant sur le fait que dans 99% des cas, vous ne devriez pas rencontrer de problème. Dans le cas contraire, n'hésitez pas à contacter les listes de discussion Erlang[^ERL-TALK] et Elixir[^EX-TALK]. 

Pour la configuration nous allons donc utiliser les options suivantes :

```
$ ./configure --disable-debug \
              --without-javac \
              --enable-shared-zlib \
              --enable-dynamic-ssl-lib \
              --enable-smp-support \
              --enable-threads \
              --enable-hipe \
              --enable-kernel-poll \
              --with-wx \
              --with-dynamic-trace=dtrace
```

> Sur Mac, il faudra ajouter l'option `--enable-darwin-64bit`.

Une fois la configuration faite, nous pouvons lancer la compilation :

```
$ make
```

Si tout s'est bien passé, il ne reste plus qu'à faire l'installation proprement dite :

```
$ sudo make install
```

Pour pouvoir installer et utiliser Elixir, les outils `erl` et `erlc` doivent être accessibles dans votre `PATH`. Vérifiez que c'est bien le cas en lançant la commande `erl`. Vous devriez alors vous retrouver dans le shell interactif d'Erlang. 

![Le shell Erlang](../images/erl.png)

### _kerl_

Il existe une autre solution pour installer Erlang : _kerl_. Cet outil permet de faciliter l'installation et la compilation de plusieurs versions d'Erlang, un peu sur le même modèle que `rvm`[^RVM] ou `virtualenv`[^VENV]. 

Pour utiliser _kerl_, il faut commencer par l'installer en le récupérant depuis le dépôt GitHub : https://github.com/spawngrid/kerl

Pour cela il suffit de mettre le script `kerl` dans un répertoire accessible depuis votre `PATH`. Une fois cet utilitaire installé, il suffit de créer un fichier `~/.kerlrc` contenant les options de configuration pour la compilation d'Erlang. Si l'on se réfère à ce qui a été fait précédemment, nous aurons donc ceci :

`~/.kerlrc (Linux) : `
```
KERL_BASE_DIR="/home/glejeune/.kerl"
KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac --enable-shared-zlib --enable-dynamic-ssl-lib --enable-smp-support --enable-threads --enable-hipe --enable-kernel-poll --with-wx"
```

`~/.kerlrc (OSX) : `
```
KERL_BASE_DIR="/Users/glejeune/.kerl"
CPPFLAGS="-march=native -mtune=native -O3 -g"
KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac --enable-shared-zlib --enable-dynamic-ssl-lib --enable-smp-support --enable-threads --enable-hipe --enable-kernel-poll --enable-darwin-64bit --with-wx"
```

Pour compiler et installer une version d'Erlang, nous commençons pas récupérer les _releases_ disponibles :

```
$ kerl update releases
Getting the available releases from erlang.org...
The available releases are:
17.0-rc1 17.0-rc2 17.0 R10B-0 R10B-10 R10B-1a R10B-2 R10B-3 R10B-4 
R10B-5 R10B-6 R10B-7 R10B-8 R10B-9 R11B-0 R11B-1 R11B-2 R11B-3 R11B-4 
R11B-5 R12B-0 R12B-1 R12B-2 R12B-3 R12B-4 R12B-5 R13A R13B R13B01 
R13B02-1 R13B02 R13B03 R13B04 R14A R14B R14B01 R14B02 R14B03 R14B04 
R14B_erts-5.8.1.1 R15B R15B01 R15B02 
R15B02_with_MSVCR100_installer_fix R15B03-1 R15B03 
R16A_RELEASE_CANDIDATE R16B R16B01 R16B02 R16B03-1 R16B03
```

Pour compiler la version choisie, nous utilisons la commande `build` en précisant la _release_ à compiler et en lui donnant un nom :

```
$ kerl build 17.0 17.0-wx
```

L'installation se fait en précisant le nom de la _release_ et le répertoire d'installation. Pour ce dernier, la _convention_ veut que l'on se place dans le répertoire `~/.kerl/install`. Vous êtes bien entendu libre de faire l'installation ou vous le souhaitez :

```
$ kerl install 17.0-wx ~/.kerl/install/17.0-wx
```

Une fois l'installation terminée, pour utiliser la version `17.0-wx` il suffit d'exécuter le script `activate` situé dans le répertoire d'installation :

```
. ~/.kerl/install/17.0-wx/activate
```

### Windows

Pour Windows, je ne vous conseille pas de faire l'installation _à la main_. En effet, il n'est pas possible de le compiler avec les outils GNU, nous obligeant à installer Visual Studio C++, ou, à minima, le SDK Microsoft. Heureusement il existe un installeur sur le site http://www.erlang.org, et je vous encourage à l'utiliser plutôt que de vous lancer dans une compilation manuelle. 

Toutefois, si l'aventure vous tente, il faudra installer _Cygwin_[^CYGWIN] (ou _MinGW_[^MINGW]), .Net 4[^DOTNET4], le SDK Microsoft[^MSSDK], _OpenSSL_[^OPENSSL] et _Perl_[^PERL].

Pour installer _OpenSSL_, commencez par installer _Perl_ en faisant bien attention à ce que le répertoire `bin` soit dans votre `PATH` :

```
set PATH=C:\Perl64\bin;%PATH%
```

Une fois _Perl_ installé, décompressez les sources d'_OpenSSL_ et rendez-vous dans le répertoire de décompression. Exécutez les commandes suivantes : 

```
perl Configure VC-WIN64A --prefix=/OpenSSL
ms\do_win64a
nmake -f ms\nt.mak
nmake -f ms\nt.mak install
```

Pour installer _wxWidgets_, récupérez les sources, décompressez-les et rendez-vous dans le sous-répertoire `build\msw`. Exécutez la commande : 

```
nmake TARGET_CPU=amd64 BUILD=release SHARED=0 UNICODE=1 USE_OPENGL=1 ^
USE_GDIPLUS=1 DIR_SUFFIX_CPU= -f makefile.vc
```

Allez ensuite dans le sous-répertoire `contrib\build\stc` et exécutez la même commande que précédemment.

Pour terminer, récupérez et décompressez les sources d'Erlang. Dans le shell _Cygwin_, positionnez la variable d'environnement `ERL_TOP` :

```
ERL_TOP=$HOME/otp_src_17.0
export $ERL_TOP
```

> Modifiez la valeur de la variable `ERL_OTP` en fonction de l'emplacement de vos sources.


Il ne reste plus qu'à faire la configuration puis l'installation :

```
cd $ERL_TOP
eval `./otp_build env_win32 x64`
./otp_build autoconf
./otp_build configure --with-ssl=/OpenSSL
./otp_build boot -a
./otp_build release -a /Erlang
```

Vous avez maintenant votre version compilée d'Erlang dans `/Erlang`

## Installer Elixir

Afin de bénéficier de la toute dernière version, nous allons récupérer les sources directement dans le _repository_ Github du projet. Assurez-vous donc que `git` est bien installé sur votre machine. Sur Linux, il suffit d'installer le package `git`. Sur Mac, il aura été installé via les _command line tools_. Pour Windows, il existe un installeur sur le site http://git-scm.com/downloads.

Récupérez ensuite les sources d'Elixir :

```
$ git clone https://github.com/elixir-lang/elixir.git
```

> N'ayant peur de rien, nous prennons les sources directement dans le *master*. Vous pouvez préférer compiler une branche *stable*. Dans ce cas, utilisez la commande :
> ```
> $ git clone https://github.com/elixir-lang/elixir.git -b <tag>
> ```
> Ou `<tag>` correspond au tag de la version stable à installer.
>
> Cependant, faites attention, car si vous optez pour une autre branche que _master_, il se peut que certains exemples donnés dans dans les chapitres suivants ne fonctionnent pas.

Une fois les sources en notre possession, rendez-vous dans leur répertoire et compilez Elixir avec un simple `make` : 

```
$ cd elixir
$ make
```

La compilation est relativement rapide. Vous pouvez ensuite lancer l'installation :

```
$ sudo make install
```

> Sous Windows, il faudra taper ces commandes dans le shell _Cygwin_, en hométant le `sudo` pour l'installation.

Notez qu'il n'y a pas de script de configuration et que par défaut, l'installation se fera dans `/usr/local`. Si cela ne vous plaît pas, vous pouvez toujours changer cela en surchargeant la valeur du `PREFIX` :

```
$ cd elixir
$ make PREFIX=/your/custom/path
$ sudo make PREFIX=/your/custom/path install
```

Tout comme nous avons besoin d'accéder à `erl` et `erlc` pour Erlang, dans le cas d'Elixir, assurez-vous que les outils `iex`, `elixir` et `elixirc` sont bien pointés par votre `PATH`. Vous pouvez le vérifier en accédant au shell interactif Elixir : 

![Le shell Elixir](../images/iex.png)

## Mise à jour

L'intérêt d'installer Erlang et Elixir _à la main_ est de pouvoir se mettre à jour à chaque changement important. Pour cela, je ne saurais trop vous inciter à vous abonner à la liste Elixir[^EX-CORE] afin d'être informé des changements fréquents. Et si vous souhaitez bénéficier des nouveautés, il vous suffit de mettre à jour les sources, supprimer l'ancienne version puis compiler et installer la nouvelle :

```
$ cd elixir
$ make clean
$ git pull origin master
$ make
$ sudo rm -rf /usr/local/lib/elixir
$ sudo make install
```

Pour Erlang, nous avons fait l'installation à partir des sources disponibles sur le site http://www.erlang.org. Vous pouvez faire la même chose en utilisant le dépôt GitHub[^ERL_GITHUB]. Ceci vous permettra de faire la mise à jour en utilisant la même méthode que celle proposée ci-dessus pour Elixir. Cependant, je vous conseille plutôt d'utiliser _kerl_.

## RTFM

Plus vous avancerez dans l'apprentissage d'Erlang et Elixir, plus vous vous rendrez compte de la richesse de ces langages. Vouloir être exhaustif en un seul ouvrage est tout bonnement impensable. Je vous conseille donc d'avoir sous la main la documentation. En effet, tout au long de ce livre, je prendrai soin de vous faire découvrir Erlang et Elixir, mais je devrai me contenter de rester en surface, de faire l'impasse sur des modules qui pourront vous être d'une aide précieuse. Sachez par exemple que pour tout ce qui concerne la compression, la cryptographie, la création de serveur ou client HTTP, DNS, SSH, ... Il existe dans OTP tout ce que l'on est en droit d'espérer dans langage moderne. 

Pour accéder à la documentation, il existe bien entendu les sites officiels des langages : http://www.erlang.org et http://elixir-lang.org. Mais j'ai également le site http://erldoc.info, créé par mes soins, donnant des liens vers diverses sources de documentation, et offrant également la possibilité d'accéder à la documentation de nombreux modules créés par la communauté.

## Petit tour du propriétaire

### Elixir

Revenons dans le shell interactif pour en découvrir les contours. Et comme toujours, pour bien commencer, intéressons-nous à la documentation. Pour cela saisissez `h()` dans le shell.

![L'aide dans le shell Elixir](../images/iex-help.png)

Je passe sur le début du message envoyé pour ne retenir ici que le fait que nous pouvons accéder à l'aide d'un module spécifique en passant son nom en paramètre de la commande `h()`. Ainsi, en tapant `h(Enum)` nous aurons accès à la documentation du module `Enum`. Nous pouvons également accéder à la documentation d'une fonction particulière d'un module : `h(Enum.all?/2)`. 

Quelques remarques avant de continuer :

* `h` est en fait une fonction du module `IEx.Helpers`. Vous pouvez vous en convaincre en exécutant la commande `IEx.Helpers.h`.
* Le shell interactif supporte la complétion via TAB. Ce qui facilite grandement la découverte des modules et fonctions disponibles.
* Elixir n'oblige pas à mettre les paramètres d'une fonction entre parenthèses, tant qu'il n'y a pas d'ambiguïté. Ainsi vous pouvez utiliser `h Enum` à la place de `h(Enum)`.
* L'accès à la documentation d'une fonction se fait en précisant, après son nom, son arité, séparé du nom par un slash (`/`). Si vous ne donnez pas cette précision, `IEx.Helpers.h` vous renverra, à la suite, la documentation de l'ensemble des fonctions, de l'arité la plus basse à la plus haute.
* Nous avons pu utiliser `h` sans préciser le module auquel cette fonction appartient, car le module `IEx.Helpers` est automatiquement chargé dans le shell. Il en est de même pour le module `Kernel`. Vous pouvez donc accéder à la documentation de ses différentes fonctions directement.

Maintenant que vous avez connaissance du minimum, vous pouvez jouer avec les modules et fonctions et faire votre premier *Hello World!*

```
iex(3)> IO.puts "Hello World!"
Hello World!
iex(4)>
```

### Erlang

Avec Erlang, l'accès à la documentation du shell se fait via la commande `help().`. Cependant, vous n'aurez ici que la liste des commandes spécifiques au shell Erlang.

Nous pouvons tout de même vérifier que tout est opérationnel en faisant un petit test :

```
1> io:format("Hello World~n").
Hello World
ok
2>
```

[^WXWIDGETS]: [https://www.wxwidgets.org/](https://www.wxwidgets.org/)

[^XCODE]: [https://developer.apple.com/xcode/](https://developer.apple.com/xcode/)

[^BREW]: [http://brew.sh/](http://brew.sh/)

[^MACPORTS]: [http://www.macports.org/](http://www.macports.org/)

[^ES_REPOS]: [https://www.erlang-solutions.com/downloads/download-erlang-otp](https://www.erlang-solutions.com/downloads/download-erlang-otp)

[^EX_RELEASES]: [https://github.com/elixir-lang/elixir/releases/](https://github.com/elixir-lang/elixir/releases/)

[^ERL-TALK]: [http://erlang.org/pipermail/erlang-questions/](http://erlang.org/pipermail/erlang-questions/)

[^EX-CORE]: [https://groups.google.com/forum/#!forum/elixir-lang-core](https://groups.google.com/forum/#!forum/elixir-lang-core)

[^EX-TALK]: [https://groups.google.com/forum/#!forum/elixir-lang-talk](https://groups.google.com/forum/#!forum/elixir-lang-talk)

[^RVM]: [https://rvm.io/](https://rvm.io/)

[^VENV]: [http://www.virtualenv.org/](http://www.virtualenv.org/)

[^CYGWIN]: [http://www.cygwin.com](http://www.cygwin.com)

[^MINGW]: [http://sourceforge.net/projects/mingw/files/Installer/mingw-get-inst/](http://sourceforge.net/projects/mingw/files/Installer/mingw-get-inst/)

[^DOTNET4]: [http://www.microsoft.com/download/en/details.aspx?id=17851](http://www.microsoft.com/download/en/details.aspx?id=17851)

[^MSSDK]: [http://www.microsoft.com/download/en/details.aspx?id=8279](http://www.microsoft.com/download/en/details.aspx?id=8279)

[^OPENSSL]: [http://openssl.org/source/](http://openssl.org/source/)

[^PERL]: [http://www.activestate.com/activeperl/downloads](http://www.activestate.com/activeperl/downloads)

[^ERL_GITHUB]: [https://github.com/erlang/otp](https://github.com/erlang/otp)
