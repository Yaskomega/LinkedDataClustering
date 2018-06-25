# ########################################################################
# Classe Object qui correspond à un noeud du graphe résultat
# @attribute name : la valeur du noeud (soit l'URI, soit le littéral)
# @attribute links : la liste d'objects de type Link qui correspond au voisinage
# ########################################################################
Object <- setClass(
  # Set the name for the class
  "Object",
  
  # Define the slots
  slots = c(
    name = "character",
    links = "list"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    name = "",
    links = NULL
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(FALSE) {
      return("The parameters are not correct.")
    }
    return(TRUE)
  }
)

# ########################################################################
# Classe Link qui correspond aux objets B et C dans un triplet ABC
# @attribute property : un objet de type Object correspondant à la propriété du lien
# @attribute object : un objet de type Object correspondant à la valeur du noeud associé à la propriété
# ########################################################################
Link <- setClass(
  # Set the name for the class
  "Link",
  
  # Define the slots
  slots = c(
    property = "Object",
    object = "Object"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    property = NULL,
    object = NULL
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(FALSE) {
      return("The parameters are not correct.")
    }
    return(TRUE)
  }
)


# ########################################################################
# Fonction qui permet de savoir si une liste d'objets contient
# une occurence de l'object passé en paramètre
# @param list_of_objects : la liste d'object à inspecter
# @param item : l'object à rechercher dans la liste
# @return true si l'occurence est trouvée, false sinon
# ########################################################################
contains <- function (list_of_objects , item){ 
  if(length(list_of_objects) > 0){
    for( i in 1:length(list_of_objects)){
      if(identical(list_of_objects[[i]], item)){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

# ########################################################################
# Fonction qui permet de savoir si une liste d'objets Link contient
# une occurence de l'object passé en paramètre
# @param list_of_link : la liste d'object à inspecter
# @param link : l'object à rechercher dans la liste
# @return true si l'occurence est trouvée, false sinon
# ########################################################################
containsPropertyAndObject <- function (list_of_link , link){ 
  return(contains(list_of_link, link))
}

# ########################################################################
# Fonction qui permet de savoir si une liste d'objets Link contient
# une occurence ayant la même valeur d'attribut property que celui de 
# l'object passé en paramètre
# @param list_of_link : la liste d'object à inspecter
# @param link : l'object contenant l'attribut property à rechercher dans la liste
# @return true si l'occurence est trouvée, false sinon
# ########################################################################
containsProperty <- function (list_of_link , link){
  if(length(list_of_link) > 0){
    for( i in 1:length(list_of_link)){
      if(identical(list_of_link[[i]]@property, link@property)){
        return(TRUE)
      }
    } 
  }
  return(FALSE)
}

