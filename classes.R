Result <- setClass(
  # Set the name for the class
  "Result",
  
  # Define the slots
  slots = c(
    objects = "list"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    links = list()
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

containsPropertyAndObject <- function (list_of_link , link){ 
  return(contains(list_of_link, link))
}

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

# listMatrix is a list containing list, in other words : a matrix
addItemToListInMatrix <-function(listMatrix, item, index){
  newMatrix <- list()
  #For each list in the matrix :
  for(i in 1:length(listMatrix)){
    # If the current list is the wanted list :
    if(i == index){
      # We add the item :
      
      list_tmp <- listMatrix[[i]]
      list_tmp <- append(list_tmp, item)
      newMatrix <- append(newMatrix, list(list_tmp))
    }else{
      newMatrix <- append(newMatrix, listMatrix[i])
    }
  }
  
  return(newMatrix)
}

isLiteral <-function(str){
  if(length(grep("^<.*>$", c(str), value = FALSE)) > 0){
    return(FALSE)
  }
  return(TRUE)
}