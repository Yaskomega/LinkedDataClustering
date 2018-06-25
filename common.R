# ########################################################################
# Fonction pour récupérer le motif commun à un ensemble d'objet
# @param list_of_subjects : la liste de tous les objets concernés
# @return l'objet de type Object généré et qui correspond au motif commun
# ########################################################################
getCommon <- function(list_of_subjects){
  common <- Object(name = "", links = list())
  
  obj <- list_of_subjects[[1]]  
  
  # for each link in the neighborhood :
  for (i in 1:length(obj@links)){
    
    level = 2 # level of similarity for the current link (2 = identical property and object, 1 = identical property only, 0 = nothing in common)
    for( j in 1:length(list_of_subjects)) {
      if(level > 1 && hasLink(object = list_of_subjects[[j]], property_name = obj@links[[i]]@property@name, object_name = obj@links[[i]]@object@name)){
        
        
      }else if(level > 0 && hasLinkProperty(object = list_of_subjects[[j]], property_name = obj@links[[i]]@property@name)){
        level <- 1
      } else {
        level <- 0
        break
      }
    }
    
    if(level == 2){
      new_pos <- length(common@links) + 1
      common@links[new_pos] <- Link(property = Object(name = obj@links[[i]]@property@name, links = list()), object = Object(name = obj@links[[i]]@object@name, links = list()))    
    } else if(level == 1){
      new_pos <- length(common@links) + 1
      common@links[new_pos] <- Link(property = Object(name = obj@links[[i]]@property@name, links = list()), object = Object(name = "", links = list()))
    }
    
  }
  return(common)
}

# ########################################################################
# Fonction permettant de savoir si un object (type Object) possède un certain lien (type Link)
# @param object : l'objet (type Object) à inspecter
# @param property_name : l'attribut name de l'attribut property de l'objet Link à rechercher
# @param object_name : l'attribut name de l'attribut object de l'objet Link à rechercher
# @return true si l'occurence est trouvée, false sinon
# ########################################################################
hasLink <- function(object, property_name, object_name){
  for (i in 1:length(object@links)){
    #print(paste( object@links[[i]]@property@name," : ", property_name))
    if( identical(object@links[[i]]@property@name, property_name)
        && identical(object@links[[i]]@object@name, object_name) ){
      return(TRUE)
    }
  }
  return (FALSE)
}

# ########################################################################
# Fonction permettant de savoir si un object (type Object) possède un lien (type Link)
# dont l'attribut property est celui spécifié en paramètre
# @param object : l'objet (type Object) à inspecter
# @param property_name : l'attribut name de l'attribut property de l'objet Link à rechercher
# @return true si l'occurence est trouvée, false sinon
# ########################################################################
hasLinkProperty <- function(object, property_name){
  for (i in 1:length(object@links)){
    if( identical(object@links[[i]]@property@name, property_name) ){
      return(TRUE) 
    }
  }
  return (FALSE)
}
