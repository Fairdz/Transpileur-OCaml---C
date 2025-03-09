# Transpileur-OCaml->C
Un transpileur simple qui traduit du code OCaml en code C 

Le transpileur supporte actuellement les types suivant :
- int (entiers) 
- bool (booléens) 
- unit (ou void en C : fonctions qui ne renvoient rien)

Le code OCaml doit être déclaré suivant les spécifications suivantes : 

- Des fonctions écrites sans spécification de type pour les paramètres
- Une fonction main sous la forme ```let () = ...```

Les sous fonctions ne sont pour le moment pas prises en charge. 

# Utilisation

Le transpileur utilise Ocamlc

Le transpileur utilise OCamllex

Le transpileur utilise Menhir : ```opam install menhir```

On compile une première fois en utilisant le makefile : ```make``` ou ```make -f makefile.mak```
Des warning peuvent apparaître lors de la compilation, ne pas en tenir compte.
On traduit ensuite un code écrit dans un fichier ```input.ml``` : ```./transpileur input.ml ```

Vous trouverez un exemple de fichier dont la transpilation fonctionne dans ```exemple.ml```

# Nettoyage 

Le makefile contient deux fonctions de nettoyage :
- ```make clean``` ou ```make -f makefile.mak clean```, pour effacer uniquement les fichiers qui servent lors de la création de l'exécutable, et qui ne servent plus après (recommandé)
- ```make distclean``` ou ```make -f makefile.mak distclean```, pour effacer tous les fichiers non essentiels à la création de l'exécutable
