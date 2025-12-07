# Comilateur micro-go

## Sommaire
Le projet est organisé en 4 fichiers principaux, en plus d'un dossier test contenant des fichiers go.: 

1. Le lexer : Décrit les caractères autorisés et les tokens pouvant y être associés.
2. Le parser : Définit notre grammaire et lance des erreurs si celle-ci n'est pas respectée.
3. Le typechecker : Vérifie que les règles d'inférence sont bien respectées.
4. Génération de code MIPS : Permet de générer du code ASM depuis un fichier go.




## 1. Lexer

### 1.1 Structure du fichier
Le squelette fourni étant assez bien rempli, il n'y a pas beaucoup de liberté ayant été prise durant cette partie, excepté pour le bonus concernant le semi-automatique traité en 1.2.

Un petit mot sur le fichier en lui même : 
- Nous avons eu recourt aux expressions régulières vu au cours du semestre pour définir les composantes d'un fichier go
```
let digit = ['0'-'9']
let number = digit+
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*
let hexa = ['0'-'9'] | ['a' - 'f'] | ['A' - 'F']
let entier = number | (("0x" | "0X") hexa+)
let car = [' ' - '!'] | ['#' - '['] | [']' - '~']
let echap  = '\\' ['n' '\\' '"']            
let chaine  = '\"' (car | echap)* '\"'
```

Ainsi que quelques fonctions pour s'assurer du bon fonctionnement des tokens, par exemple pour convertir une chaine héxadécimale en entier :

```
let rec hexadecimal_to_int h acc i =
   let hexe_car_to_int c = 
      let code = Char.code c in
         if (102 >= code && code >= 97) then (code - 97 + 10)
         else if (70 >= code && code >= 65) then (code - 65 + 10)
         else if (48 <= code && code <= 57) then (code - 48)
         else raise (Error "ce caractere n'est pas un hexa")
      in
   if String.length h >= i then acc
   else hexadecimal_to_int h (acc*16 + (hexe_car_to_int (String.get h i))) (i+1)
```

### 1.2 Bonus : Les semi automatiques.

Nous avons décidé d'ajouter un booléen global en référence (appelé `b`). Celui-ci indique si le dernier token vu fait partie de ces tokens: ⟨ident⟩ ⟨entier⟩ | ⟨chaîne⟩ | true | false | nil | return | ++ |-- | ) | }

Lorsqu'il voit un retour chariot, il va alors faire une disjonction de cas: si `b`, il va ajouter le token SEMI, sinon, il va l'ignorer.

Il est intéressant de noter que ce changement fait que le code suivant ne marche plus:
``` go
if (true) { fmt.Print("test1") }
else { fmt.Print("test2") }
```
Le point-virgule automatique va alors ajouter un point-virgule à la fin du bloc de if, et causer un problème chez le parser.
Après avoir testé sur un compilateur en ligne, cette erreur est aussi présente dans le langage Go lui même,
nous avons donc conservé ce fonctionnement.


## 2. Le parser

### 2.1  Structure du fichier 
Le fichier ressemble à ce que l'on a pu voir en TD : 
En prenant exemple sur un objet comme ident, on traduirait le code 
```
ident:
  id = IDENT 
    { { loc = $startpos, $endpos; id = id } }  //loc = (int*int) id =id
;
```
Par   G ->...
      | ...
      |IDENT -> { loc , id }
Où G désignerait toute notre grammaire.

Il y a une petite nuance sur instr et instr_desc, dissociation proposée avec le squelette, où instr contient une position et un "idesc" qui comme son nom l'indique décrit l'instruction : si c'est une variable, un for, un if.... Idem pour les expressions.


### 2.2 Conflits complexes à gérer
A la fin de la rédaction de notre code, nous avons pu le tester sur les fichiers go, et plusieurs conflits shift/reduce nous ont posé problème : 

- *Le conflit PSET / SET*

Il y avait un conflict à cause de ces deux règles:

```
| separated_nonempty_list(COMA, expr) SET separated_nonempty_list(COMA, expr) {}

| separated_nonempty_list(COMA, IDENT) PSET separated_nonempty_list(COMA, expr) {}
```

En effet, lorsque le parser voit un IDENT, il ne sait pas si il doit le réduire en expr pour créer un SET, ou plutôt le laisser tel quel pour créer un PSET.

Pour régler cela, nous avons changé la règle du PSET pour qu'il prenne des expr plutôt que des IDENT:
```
separated_nonempty_list(COMA,expr) PSET separated_nonempty_list(COMA,expr) {}
```

Dans le code, on vérifie que ces expr soient bien des Var(Ident).

- *Conflit avec Bloc*

Le conflit le plus subtil était celui des `SEMI` dans le bloc. On a commencé avec la règle suivante:  
``` 
| BEGIN li = separated_list(SEMI,instr) END {}
| BEGIN li = separated_list(SEMI,instr) SEMI END {} 
```

Mais cela causait un conflit shift/reduce. Il ne savait pas si interpréter le SEMI comme un séparateur, ou comme le dernier SEMI du bloc.  
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


## 3. Le typechecker

### 3.1 Structure du fichier
Le type checker est constitué de 3 environnements : 
   - tenv (``type env``)
   - fenv (func env, donc un environnement de type ``(typ list) * (typ list) env``)
   - senv (struct env, donc de type ``(ident*typ)`` env)
Il y a beaucoup de fonctions auxilliaires dans le fichier pour rendre la lecture des fonctions principales telles que type_expr ou type_instr plus simples, pour présenter le fichier il vaudra mieux donc s'intéresser à l'utilité des fonctions qu'au code lui même.

Parmis les fonctions principales du typechecker il y a :
- ``check_typ`` qui vérifie que le type existe bien
- ``type_expr`` qui rend une liste de type et un "expr_typed" : sa signification sera expliquée juste en dessous.
- ``check_expr(e,t)`` qui vérifie que e est bien de type t.
- ``check_instr(i, ret, tenv)``qui vérifie qu'une instruction est bien formée.
- ``get_ast(decls)`` que l'on utilise à la toute fin du fichier pour pouvoir s'en servir sur ``compile.ml``

**Note : Pourquoi expr_typed et pas expr simplement ?**  
Dans la génération de code, lors de certaines opérations telles le print et l'accès des champs de structures, nous avons besoin du type. En effet, on ne doit pas faire la même opération pour afficher un entier, ou pour afficher une chaîne de charactères. Alors, on a décidé de changer l'ast, et d'associer aux expressions le type.


### 3.2 Les fonctions multiretours

Le problème des fonctions multiretours est apparu très rapidement. Dans type_expr, comment traiter le cas où la fonction renvoie plusieurs variables/objets de types différents ?

Nous avons décidé de changer la structure du code pour que type_expr renvoie une liste de types plutôt qu'un type unique. Cela a entraîné beaucoup de changements et de vérifications supplémentaires.


### 3.3 Quelques vérifications supplémentaires 

### 3.2.1 Les fonctions doivent rendre quelque chose !

Les fonctions doivent nécessairement  contenir un return si elles ont des types de retour spécifiés. Nous avons donc ajouté une fonction qui vérifie que chacune des fonctions admet un return. Le typechecker accepte les return dans le corps de la fonction, dans les sous blocs, et les return dans les conditionnelles s'ils sont présents dans le if et dans le else. 

### 3.2.2 Il doit y avoir la fonction main

Le programme doit forcément avoir une fonction main qui n'a pas de paramètres et qui n'a aucun type de retour !!

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

Le problème de cette méthode est qu'on fait beaucoup d'appels à sbrk, et qu'on ne désalloue pas les instances créées.

### 4.3 Les appels de fonction

#### 4.3.1 Les tableaux d'activation

Nous stockons dans fenv les tableaux d'activation de chaque fonction, et le nombre de types de retour.  
Le tableau d'activation commence avec la return adress, et le frame pointer. Ensuite, on y met les adresses de là où il doit mettre les valeurs de retour. Enfin, on a les paramètres et les variables locales.  
Si la fonction ne renvoie qu'une seul valeure, il va mettre cette valeure dans $t0 plutôt qu'empiler l'adresse de retour.

| $ra | $fp | return | params | variables locales

#### 4.3.2 Appelant / appelé

Le code qui alloue sur la pile le tableau d'activation est découpé entre ce qui est fait par l'appelant, et ce qui est fait par l'appelé.  
L'appelant s'occupe de changer le pointeur de pile et d'insérer les paramètres et les adresses de retour. Enfin, il appelle la fonction avec jal  
L'appelé s'occupe de stocker le $ra et l'$fp, et il calcule la nouvelle valeur de $fp.

Le code qui termine la fonction et fait le jump est placé à la fin de la fonction

#### 4.3.3 Problèmes de cette méthode

Le problème de cette méthode est qu'on ne prend pas en considération les différents scopes. Dans le code suivant
```go
func main() {
    var a = 0
    { 
        var a = 2
        fmt.Print(a)
    }
    fmt.Print(a)
}
```
Il n'aura pas le résultat attendu. Il renverra une erreur indiquant le fait que a a déjà été déclaré avant.
