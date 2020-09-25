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
