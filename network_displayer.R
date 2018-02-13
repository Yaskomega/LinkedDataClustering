# Load package
library(networkD3)

displayObjectNetwork <-function(obj){
  source <- c()
  target <- c()
  name <- c(obj@name)
  carac <- c("object")
  
  if(length(obj@links) > 0){
    # for each link in the neighborhood :
    for (i in 1:length(obj@links)){
      
      if( identical(obj@links[[i]]@property@name %in% name , FALSE) ){
        name <- c(name, obj@links[[i]]@property@name)
        carac <- c(carac, "property")
      }
      
      source <- c(source, match(obj@name,name))
      target <- c(target, match(obj@links[[i]]@property@name,name))
      
      
      if( identical(obj@links[[i]]@object@name %in% name , FALSE) ){
        name <- c(name, obj@links[[i]]@object@name)
        carac <- c(carac, "object")
      }
      
      source <- c(source, match(obj@links[[i]]@property@name,name))
      target <- c(target, match(obj@links[[i]]@object@name,name))  
      
    }  
  }
  
  links=data.frame(
    source=source,
    target=target
  )
  
  nodes=data.frame(
    name=name,
    carac=carac
  )
  
  # The Nodes ID MUST be numeric and start from 0!
  links$source=as.numeric(links$source)-1
  links$target=as.numeric(links$target)-1
  
  # If there's no links :
  if(length(links$source) == 0 && length(links$target) == 0){
    links=data.frame(
      source=c(0),
      target=c(0)
    )
    # The Nodes ID MUST be numeric and start from 0!
    links$source=as.numeric(links$source)
    links$target=as.numeric(links$target)
  }
  
  #plot
  forceNetwork(
    Links=links, 
    Nodes=nodes, 
    NodeID = "name", 
    Group = "carac",
    zoom = TRUE
  )
  
}

#displayObjectNetwork(list_of_subjects[[2]]) 
