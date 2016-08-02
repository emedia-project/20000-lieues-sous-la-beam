# ETS et DETS

* `set` : chaque clé doit être unique. Cela permet de simuler très simplement un système de stckage clé/valeur.
* `ordered_set` : comme son nom le laisse présager, dans ce type de `set` les données sont ordonnées de manière croissante. Cela facilitera les opérations sur des ranges de valeurs.
* `bag` : contrairement au `set`, une telle table peut avoir plusieurs entrées avec la même clé, tant que les valeurs rattachées sonr différentes.
* `duplicated_bag` : agit comme un `bag` mais en autorisant la duplication de valeurs.
