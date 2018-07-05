# LinkedDataClustering
Research about clustering linked data ressources from a SPARQL request.
Research university career - Master at ESILV, Graduate School of Engineering.
Thomas Raimbault (ESILV - DaVinci Research Center), Yassin HASSAN

Djebali, S. and Raimbault, T. (2015). SimplePARQL : A New Approach Using Keywords over SPARQL to Query the Web of Data. In Proceedings of the 11th International Conference on Semantic Systems, SEMANTICS '15, pages 188-191, New York, NY, USA. ACM

Located at De Vinci Research Center, Pôle Universitaire Léonard de Vinci, Paris, FRANCE

## Exécuter une méthode de classification
Pour exécuter une méthode de classification, lancez le script application_common.R. Puis lancez le script correspondant à la méthode de votre choix :
- MCA/HCPC : `application_mca_hcpc.R`
- FCA/HCPC binaire : `application_fca_hcpc_binary.R`
- FCA/HCPC n-aire : `application_fca_hcpc_ternary.R`
- hclust/DynamicTreeCut binaire : `application_hclust_dynamicTreeCut_binary.R`
- hclust/DynamicTreeCut n-aire : `application_hclust_dynamicTreeCut_ternary.R`

## Afficher le graphe d'un objet
Pour Afficher le graphe d'un object (type Object), lancez le script `network_displayer.R` puis utilisez la méthode `displayObjectNetwork`.
Exemple : `displayObjectNetwork(list_of_subjects[[2]])`

## Liste d'inclusion et d'exclusion
Lors de votre requête SPARQL, vous pouvez préciser quels liens vous désirez conserver (liste d'inclusion) ou si au contraire vous désirez vous débarasser de certain liens indésirables. 
Pour cela, vous devez vous rendre dans le fichier functions.R, dans la fonction RequestNeighborhood. 
Vous y trouverez deux variables :
- `useful_links` : la liste d'inclusion qui vous permettra de sélectionner uniquement les liens qui vous intéresse
- `ignored_links` : la liste d'exclusion, qui n'est utilisées qui si la liste d'inclusion est vide, et qui vous permet de sélectionner les liens dont vous souhaiter vous débarrasser. 

L'utilisation est simple, vous devez ajouter à la liste le nom du lien. 
Par exemple : 
Je veux ignorer les liens contenant les mots "abstract" ou "comment" :
```
  ignored_links <- c("abstract", "comment")
  useful_links <- c()
```
Je veux uniquement les liens contenant le mot "sameAs" :
```
  ignored_links <- c()
  useful_links <- c("sameAs")
```
## SPARQL
### Requête
Pour changer de requête SPARQL, vous devez vous rendre dans le fichier `application_common.R` et modifier la variable `query`.
### End-Point
Pour changer de endpoint SPARQL, vous devez vous rendre dans le fichier `application_common.R` et modifier la variable `endpoint`.
## Classes
### Classe Object
La classe Object représente des objets du graphe. 
### Classe Link
La classe Link représente des liens du graphe. 
