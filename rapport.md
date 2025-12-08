# Compilateur micro-go - Par Niccoló ANGELI et Clara CHEBOUT

## Sommaire
Le projet est organisé en 4 fichiers principaux, en plus d'un dossier test contenant des fichiers go : 

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
   - tenv (``(typ*bool ref*Mgoast.location) ``)
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
Dans la génération de code, lors de certaines opérations telles le print et l'accès des champs de structures, nous avons besoin du type. En effet, on ne doit pas faire la même opération pour afficher un entier, ou pour afficher une chaîne de charactères. Alors, on a décidé de changer l'ast, et d'associer aux expressions leur type.


### 3.2 Les fonctions multiretours

Le problème des fonctions multiretours est apparu très rapidement. Dans type_expr, comment traiter le cas où la fonction renvoie plusieurs variables/objets de types différents ?

Nous avons décidé de changer la structure du code pour que type_expr renvoie une liste de types plutôt qu'un type unique. Cela a entraîné beaucoup de changements et de vérifications supplémentaires.


### 3.3 Quelques vérifications supplémentaires 

-  Les fonctions doivent rendre quelque chose !

Les fonctions doivent nécessairement  contenir un return si elles ont des types de retour spécifiés. Nous avons donc ajouté une fonction qui vérifie que chacune des fonctions admet un return. Le typechecker accepte les return dans le corps de la fonction, dans les sous blocs, et les return dans les conditionnelles s'ils sont présents dans le if et dans le else. 

### 3.2.2 Il doit y avoir la fonction main

Le programme doit forcément avoir une fonction main qui n'a pas de paramètres et qui n'a aucun type de retour !!

## 3.4 Bonus : erreur si variables non utilisées
Il a été décidé de lancer une erreur à la place d'un warning ppour simplifier le code, mais si l'on voit les choses dans le bon angle ça permet d'avoir un code plus propre !

On a transformé tenv pour passer d'un environnement typ à un environnement qui a en plus la location et un booléen en référence qu'on modifie quand on rencontre une variable.

Dans type_expr_var on consulte l'environnement, si la var est trouvée on passe u à true pour signifier qu'elle a été utilisée.
Dans check_seq on regarde l'environnement final contenant toutes les variables locales au bloc et si une variable n'a pas été utilisée, 

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

### 4.4 Assignation dans des variables

Lorsque l'utilisateur écrit 
```go
a, b, c = e1, e2, e3
```
le compilateur va l'écrire comme 
```go
a = e1
b = e2
c = e3
```
Ainsi, il est important de noter que 
```go
x, y = y, x 
``` 
n'aura pas le même résultat qu'en go classique.

## 5 - Exemple concret sur un exemple minimal
Nous allons sur le code ``var.go`` dont le contenu est le suivant :
```go
package main;
import "fmt";

// Résultat attendu 42
func main() {
     var x, y int;
       x = 1;
       y = 6;
       x = x+2;
       y = y*(x+4);
       fmt.Print(y)
};

```
Décrire ce que notre programme permet de faire.

### 5.1 Analyse lexicale
Le lexer emet les tokens package import func et var présents dans le fichier et qui sont des mots clés.
Il crée ensuite les ident des mots non mot clé : IDENT("fmt"), IDENT("main"), IDENT("x")
Il insere aussi automatiquement des points virgules grâce à la partie traitée sur le bonus.
Ici par exemple il lit x=1, b devient donc vrai, donc au passage du saut à la ligne un token semi est généré automatiquement.
Les nombres 1 6 2 4 sont aussi convertis en INT

### 5.2 Analyse synthaxique
Le parser reçoit les tokens du lexer et crée son AST qu'il renverra : 
Ici toutes les règles sont respectées donc pas d'erreur lancée, il peut être intéressant cependant de se pencher rapidement sur la ligne y = y*(x+4), où x+4 devient Binop(Add,x,4) et le tout devient Binop(Mul,y, Binop(Add(x,4)). Les règles de priorité sont bien respectées en cas d'ambiguité par les lignes %left ...

### 5.3 Analyse sémantique 
On parcourt l'AST pour vérifier la cohérence des types.
On initialise fenv et senv
On vérifie le corps de main directement parce qu'il n'y a pas d'autre bloc.
Déclaration var : check_vars ajoute x et y dans l'environnement local au bloc tenv avec le type TInt.
Calculs : x = x+2, type_expr vérifie que x est bien TInt dans tenv, et que 2 est un TInt, et la fonction check_expr_binop valide que l'addition de ces 2 entiers est autorisée par la grammaire et produit un TInt.

Une fois tout le fichier parcouru, on peut renvoyer l'ast avec une information supplémentaire : le type de nos expressions ! (etype)

### 5.4 Généralisation de Code MIPS
On traduit l'ast typé en assembleur mips.
- Pour main, on save $ra et $fp sur la pile, et on deplace $fp vers $sp pour démarrer la pile au bon endroit.
``get_var`` calcule la position de x et y en calculant l'offset par rapport à $fp
- Pour x=1, tr_expr met 1 dans $t0 et on push le résultat.
Ensuite ``tr_adress_lval`` calcule l'adresse mémoire de x dans la pile, on  dépile sa valeur et on la stocke dans $sw à la bonne adressse.
- Arithmétique y*(x+4), on génère le code récursivement.
  x est chargé (lw) depuis la pilme vers $t0
  4 est chargé dans $t0
  Une addition add est effectuée.
  Le résultat est multiplié (mul) avec la valeur de y.
- Print (fmt.Print(y)) : la valeur de y est calculée dans $t0
  Elle est déplacée dans $a0, puis on écrit li v0 1 dans le fichier, et syscall est appelé pour afficher 42.

  

   






