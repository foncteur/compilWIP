# Jalon 1

(O) Pour l'instant, je commence à bosser sur le lexer et le parser. Je regarde les tests dans l'ordre, et je vais essayer de rajouter le minimum de choses pour pouvoir passer le 1er test, puis le 2e, etc... pour minimiser les conflits.
Concernant le lexer, je constate que var\_id, label\_id et type\_con sont identiques, donc je vais renvoyer un token pour "un identifiant commençant par une minuscule".
Pour type\_variable, je fais le choix de renvoyer le token "quote" au début, et je gérerai ça au niveau du parser. Si ça ne marche pas, on aura la possibilité de gérer ça au niveau du lexer à la place.

Note : pour l'instant, un int c'est juste un éventuel - et une suite de chiffres, je ne gère pas encore l'hexadécimal, le binaire ou l'octal (on verra plus tard).

J'ai rajouté pour le moment le minimum de trucs pour avoir des tests qui passent, il y en a déjà 4. To be continued...

2e commit : j'ai rajouté le parsing des types, toujours seulement 4 tests.

3e commit : erreur idiote dans le lexer corrigée, on passe maintenant 10 tests.
Tous les tests de type definition ne passent pas encore, il reste des choses à debug mais il se fait tard :)

4e commit : c'est bon j'ai débugué le parsing des définitions de type !

5e commit : on peut lexer des ints (y compris en hexa, binaire ou octal), des char et des strings. Par contre, on ne vérifie pas encore que les ints sont pas trop grands.

6e commit : je bosse sur les expressions. Pour l'instant je passe sur char. 

(B) Travail du vendredi 25 septembre
REMARQUE : en regardant les résultats de 'make test' pour while j'ai vu qu'il y avait le mot clé 'True' (et donc 'False' j'imagine). Sauf que je ne l'ai pas vu sur le doc donc il faudra gérer ça !!

1er commit : j'ai rajouté le cas des n-uplet pour les expressions (+1 test passé)

2e commit : j'ai rajouté le cas de l'accès à un champ, il a fallu rajouter le lexer dot pour cela (+2 tests passés)

3e commit : j'ai rajouté le cas d'une séquence d'expression, il a fallu rajouter le lexer semicolon pour cela (+0 test passé, c'est curieux !)

4e commit : j'ai rajouté le cas d'une définition locale, il y a fallu rajouter le lexer backslash pour cela (+0 test passé)

5e commit : j'ai rajouté le cas d'une application (+5 tests passés)

6e commit : préparation du cas de l'application infixe d'un opérateur binaire par l'ajout des lexers correspondant

7e commit : ajout du cas de if-then-else, il a fallu rajouter les lexer if et else pour cela (+3 tests passés)

8e commit : ajout du cas de ref, il a fallu rajouter le lexer ref pour cela (+0 test passé)

9e commit : ajout du cas de l'assignation, ajout du lexer assign (+2 tests passés)

10e commit : ajout du cas de la lecture, ajout du lexer read (+2 tests passés)

11e commit : ajout du cas while, ajout du lexer while (+0 test passé)

12e commit : ajout du cas for, ajout du lexer for (+0 test passé)

13e commit : ajout du cas du parenthésage. ATTENTION ce commit a provoqué un conflit avec le cas des n-uplet car il inclut le 1-uplet (équivalent au parenthésage donc). Je résout ce problème dans le commit suivant ! (+1 test passé)

14e commit : résolution du conflit shift/reduce mentionné au 13e commit (+0 test passé, -1 conflit)

15e commit : ajout du cas de l'annotation de type (+1 test passé)