# Comilateur micro-go

## Sommaire

1. Lexer
2. Parser
3. typechecker
4. Génération de code

## 1. Lexer

### 1.1 Les semi-automatiques

Le lexer n'a pas posé beaucoup de problèmes. Seul le bonus, mettre des points-virgules automatiquement a réellement posé problème.

Nous avons finalement décidé d'ajouter un booléen global en référence (appelé `b`). Celui-ci indique si le dernier token vu fait partie de ces tokens: ⟨ident⟩ ⟨entier⟩ | ⟨chaîne⟩ | true | false | nil | return | ++ |-- | ) | }

Lorsqu'il voit un retour chariot, il va alors faire une disjonction de cas: si `b`, il va ajouter le token SEMI, sinon, il va l'ignorer.

Il est intéressent de noter que ce changement fait que le code suivant ne marche plus:
``` go
if (true) { fmt.Print("test1") }
else { fmt.Print("test2") }
```
Le point-virgule automatique va alors ajouter un point-virgule à la fin du bloc de if, et causer un problème chez le parser.
Après avoir testé sur un compilateur en ligne, cetter erreur semble normale. Donc nous l'avons laissée.


## 2. parser

### 2.1 Conflicts plus complexes

Plusieurs conflicts ont posé problème

#### 2.1.1 Le conflict PSET / SET

Il y avait un conflict à cause de ces deux règles:

```
| separated_nonempty_list(COMA, expr) SET separated_nonempty_list(COMA, expr) {}

| separated_nonempty_list(COMA, IDENT) PSET separated_nonempty_list(COMA, expr) {}
```

En effet, lorsqu'il voit un IDENT, il ne sait pas si le réduire en expr pour créer un SET, ou plutôt le laisser tel quel pour créer un PSET.

Pour régler cela, on a changé la règle du PSET pour qu'il prenne des expr plutôt que des IDENT:
```
separated_nonempty_list(COMA,expr) PSET separated_nonempty_list(COMA,expr) {}
```

Dans le code, on vérifie que ces expr soient bien des Var(Ident).

#### 2.1.2 Le conflict de Bloc

Le conflict le plus malicieu était celui des `SEMI` dans le bloc. On a commencé avec la règle suivante:  
``` 
| BEGIN li = separated_list(SEMI,instr) END {}
| BEGIN li = separated_list(SEMI,instr) SEMI END {} 
```


Mais cela causait un conflict shift/reduce. Il ne savait pas si interpréter le SEMI comme un séparateur, ou comme le dernier SEMI du bloc.  
Nous avons alors cherché dans le parser d'ocaml comment est-ce qu'ils ont réglé ce problème. Nous avons alors trouvé une solution dans ce fichier: https://github.com/ocaml/ocaml/blob/trunk/parsing/parser.mly à la ligne 1260 environ.

```ocaml
(* [separated_or_terminated_nonempty_list(delimiter, X)] recognizes a nonempty
   list of [X]s, separated with [delimiter]s, and optionally terminated with a
   final [delimiter]. Its definition is right-recursive. *)

separated_or_terminated_nonempty_list(delimiter, X):
  x = X ioption(delimiter)
    { [x] }
| x = X delimiter xs = separated_or_terminated_nonempty_list(delimiter, X)
    { x :: xs }
```

On s'est alors inspirés de ce code pour pouvoir régler notre problème.


## 3. typechecker

### 3.1 Les fonctions multiretours

Le problème des fonctions multiretours est apparu très rapidement. Dans type_expr, comment traiter le cas où la fonction renvoie plusieurs choses de types différents ?

Nous avons décidé de changer la structure du code pour que type_expr renvoie une liste de types plutôt qu'un type unique. Cela a entraîné beaucoup de changements et de vérifications supplémentaires.


### 3.2 Quelques vérifications supplémentaires 

### 3.2.1 Les fonctions doivent rendre quelque chose !

Les fonctions doivent contenir un return si elles ont des types de retour !! Nous avons donc ajouté une fonction qui vérifie que chacune des fonctions admet un return. Il accepte les return dans le corps de la fonction, les return dans les conditionnelles s'ils sont présents dans le if et dans le else, et enfin les return dans des sous blocs. 



## 4. Génération de code

### 4.1 Les strings

Elles sont stockées sur le segment des données statiques. Du coup, pour chaque string, on a écrit dans le .data quelque chose de la forme
```asm
label:       .asciiz    "string"
```
Ensuite, dans tr_expr, lorsque nous voyons une string, nous mettons dans $t0 l'adresse associée au label.

### 4.2 Les structures

Nous stockons dans un environnement la liste des champs pour chaque structure.  
Lorsque nous faisons un `new`, nous allouons sur le tas la place pour une nouvelle instance de cette structureen utilisant l'appel système sbrk. Nous mettons dans $t0 l'adresse renvoyée par l'appek système.   

### 4.3 Les appels de fonction

Nous stockons dans fenv les tableaux d'activation de chaque fonction, et le nombre de types de retour.  
Le tableau d'activation commence avec la return adress, et le frame pointer. Ensuite, on y met les adresses de là où il doit mettre les valeurs de retour. Enfin, on a les paramètres et les variables locales.

| $ra | $fp | return | params | variables locales

