# ########################################################################
# Fonction permettant de récupérer le voisinage d'un objet
# @param QueryResult : chaine de caractère correspondant à la valeur de l'attribut name de l'objet
# @param endpoint : SPARQL endpoint
# @return dataframe des résultats correspondant au voisinage
# ########################################################################
RequestNeighborhood<-function(QueryResult, endpoint){
  
  ignored_links <- c("abstract", "comment")
  useful_links <- c()#c("sameAs")
  filter <- ""
  if(length(useful_links) > 0){
    # we create a filter that only selects the useful links
    filter <- paste(filter, " ( ")
    
    for (i in 1:length(useful_links)){
      
      if(i > 1){
        filter <- paste(filter, " || ")
      }
      filter <- paste(filter, "regex (?b,'" , useful_links[i] , "', 'i')", sep = "")
    }
    
    filter <- paste(filter, " ) ")
  } else {
    # if the useful_links list is empty then we use the ignored_links list 
    filter <- paste(filter, " !( ")
    
    for (i in 1:length(ignored_links)){
      
      if(i > 1){
        filter <- paste(filter, " || ")
      }
      filter <- paste(filter, "regex (?b,'" , ignored_links[i] , "', 'i')", sep = "")
    }
    
    filter <- paste(filter, " ) ")
  }
  
  if (grepl("%", QueryResult) ){
    
  }
  else{
    queryvoisinage <- paste("SELECT DISTINCT ?b ?c
                            WHERE{
                            {",
                            QueryResult, "?b ?c.
                            FILTER (", filter,")
                            FILTER isIRI (?c)
                            } UNION
                            {",
                            QueryResult , " ?b ?c.
                            FILTER (", filter,")
                            FILTER isLiteral(?c)
                            FILTER ( lang(?c) = 'en')
                            }
                            }")

    QueryResultFrame <- SPARQL(endpoint, queryvoisinage)  # pour chaque rÃ©sultat de la requete initiale
    
    results <- QueryResultFrame$results # ici nous aurons que les rÃ©sultats de SELECT
  }
  return (results)
}

# ########################################################################
# Fonction permettant de générer une matrice de distance inter-cluster
# @param list_of_subjects : liste des objets 
# @param list_of_clusters : liste des clusters
# @return matrice de distance
# ########################################################################
distBetweenClusters <- function(list_of_subjects, list_of_clusters){
  list_of_common <- list()
  
  for(i in 1:length(list_of_clusters)){
    cluster <- list_of_clusters[[i]]
    list_of_objects <- list()
    for(j in 1:length(cluster)){
      list_of_objects[j] <- list_of_subjects[[ cluster[j] ]]
    }
    common <- getCommon(list_of_objects)
    common@name <- paste("Cluster ", i)
    list_of_common[i] <- common
  }
  
  dist_matrix <- generateDistMatrix(list_of_common)
  return(dist_matrix)
}

# ########################################################################
# Fonction permettant de générer une matrice de distance entre des objet de type Object
# @param list_of_objects : liste des objets 
# @return matrice de distance
# ########################################################################
generateDistMatrix <- function(list_of_objects){
  # ********************************************************
  # BUILDING THE LIST OF LINK AND THE COLUMNS NAMES AND ROWS NAMES
  # ********************************************************
  
  col <- list()
  #row <- c()
  col_to_string <- c()
  row_to_string <- c()
  number_of_columns <- 0
  for (j in 1:length(list_of_objects)){
    # selecting the current subject :
    subject <- list_of_objects[[j]] 
    
    # adding the new row name :
    row_to_string[j] <- subject@name
    
    for (i in 1:length(subject@links)){
      #for (i in 1:4){
      # selecting the current link :
      link <- subject@links[[i]]
      
      # adding the link in the list if not already added
      if(!contains(col, link)){
        col1 <- number_of_columns + 1
        col2 <- number_of_columns + 2
        
        # adding the link in the list (no need to add it twice):
        col[number_of_columns/2 +1] <- link
        
        # building column name :
        col_to_string[col1] <- paste("||" , link@property@name)
        col_to_string[col2] <- paste("||" , link@property@name ,"||", link@object@name)
        
        # increment :
        number_of_columns <- number_of_columns + 2
      }
    }
  }
  
  
  
  # ********************************************************
  # CREATING THE EMPTY MATRIX
  # ********************************************************
  data <- matrix(NA, length(list_of_objects), number_of_columns)
  colnames(data) <- col_to_string
  rownames(data) <-  row_to_string
  
  
  
  
  # ********************************************************
  # BUILDING THE BINARY MATRIX
  # ********************************************************
  
  for (j in 1:length(list_of_objects)){
    # selecting the current subject :
    subject <- list_of_objects[[j]] 
    
    for (i in 1:length(col)){
      # select the current link :
      link <- col[[i]]
      
      col_num <- i*2-1
      if(containsPropertyAndObject(subject@links, link)){
        data[j,col_num] <- 1
        data[j,col_num+1] <- 1
      } else if(containsProperty(subject@links, link)){
        data[j,col_num] <- 1
        data[j,col_num+1] <- 0
      } else {
        data[j,col_num] <- 0
        data[j,col_num+1] <- 0
      }
    }
  }
  binary_matrix <- data
  
  
  
  
  # ********************************************************
  # BUILDING THE DISTANCE MATRIX
  # ********************************************************
  
  dist_matrix = dist(binary_matrix, method = "binary")
  
  
  return(dist_matrix)
}

# ########################################################################
# Fonction permettant de générer une matrice de distance intra-cluster
# @param list_of_subjects : liste de tous les objets résultats (liste de type Object)
# @param cluster : le cluster dont l'on génére la matrice de distance
# @return matrice de distance
# ########################################################################
distInCluster <- function(list_of_subjects, cluster){
  list_of_objects <- list()
  for(i in 1:length(cluster)){
    list_of_objects[i] <- list_of_subjects[[ cluster[i] ]]
  }
  
  dist_matrix <- generateDistMatrix(list_of_objects)
  return(dist_matrix)
}


library("lattice")

# ########################################################################
# Fonction permettant d'afficher une matrice de distance
# @param dist_cluster_matrix : la matrice de distance
# ########################################################################
displayDistMatrix <- function(dist_cluster_matrix){
  dist_cluster_matrix <- as.data.frame(as.matrix(dist_cluster_matrix))
  levelplot(t(dist_cluster_matrix[c(nrow(dist_cluster_matrix):1) , ]), scales=list(x=list(rot=45)), aspect = "iso")
}

# # MCA
# c1 <- c(21, 25, 29, 1, 31, 27, 23)
# c2 <- c(2)
# c3 <- c(6,7,16,3,5,8,9,11,12,13,14,17,18,19,26,28,33,34,35)
# c4 <- c(15)
# c5 <- c(20,24,30,10)
# c6 <- c(22,4,32)
# clusters <- list(c1,c2,c3,c4,c5,c6)
# dist_cluster_matrix <- distBetweenClusters(list_of_subjects, clusters)
# 
# library("lattice")
# dist_cluster_matrix <- as.data.frame(as.matrix(dist_cluster_matrix))
# levelplot(t(dist_cluster_matrix[c(nrow(dist_cluster_matrix):1) , ]), scales=list(x=list(rot=45)), aspect = "iso")
