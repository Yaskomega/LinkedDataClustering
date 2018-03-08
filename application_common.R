# ********************************************************
# SOURCE CODE
# ********************************************************

setwd("D:/Dropbox/Yassin/ESILV/A4/RESEARCH/R/LinkedDataClustering") # To use relative paths, and set the working directory appropriately, use setwd() to point R/RStudio at the directory that contains these files.
#source("functions.R")
source("classes.R")

# installation de la librairie  SPARQL : install.packages("SPARQL")
library(SPARQL)

# installation de la librairie  SPARQL : install.packages("lubridate")
library(lubridate)




# ********************************************************
# SPARQL ENDPOINT
# ********************************************************

endpoint <- "http://dbpedia.org/sparql"
# endpoint <- "http://localhost:8890/sparql"




# ********************************************************
# SPARQL QUERY
# ********************************************************

query <- "select distinct ?g
where {
?g rdfs:label ?y
FILTER(regex(?y, 'Titanic', 'i'))
} LIMIT 30"

query <- "select distinct ?g
where {
?g rdfs:label ?y
FILTER(regex(?y, 'Jaguar', 'i'))
} LIMIT 10"

query <- "PREFIX db: <http://dbpedia.org/resource/>
PREFIX dbo: <http://dbpedia.org/ontology/>
SELECT DISTINCT ?oeuvre
WHERE {
?oeuvre dbo:author ?j.
#?oeuvre rdfs:label ?titre.
?oeuvre rdf:type  dbo:Book.
} LIMIT 10"


query <- "PREFIX db: <http://dbpedia.org/resource/>
PREFIX mo: <http://purl.org/ontology/mo/>
PREFIX foaf:  <http://xmlns.com/foaf/0.1/>
SELECT ?name ?img ?hp ?loc
WHERE {
?a a db:MusicArtist ;
foaf:name ?name ;
foaf:img ?img ;
foaf:homepage ?hp ;
foaf:based_near ?loc .
} LIMIT 10"

query <- "select distinct ?g
where {
?g rdfs:label ?y
FILTER(regex(?y, 'De Gaulle', 'i'))
} LIMIT 50 OFFSET 0"

query <- "PREFIX db: <http://dbpedia.org/resource/>
PREFIX dbo: <http://dbpedia.org/ontology/>
SELECT DISTINCT ?oeuvre ?titre ?isbn ?pages
WHERE {
?oeuvre dbo:author ?j.
?oeuvre dbo:isbn ?isbn.
?oeuvre dbo:numberOfPages ?pages.
?oeuvre rdfs:label ?titre.
?oeuvre rdf:type  dbo:Book.
} LIMIT 10"

# ********************************************************
# EXECUTING THE QUERY AND SELECTING THE RESULT
# ********************************************************

# <time>
time_B_query_exec <- Sys.time()

QueryResult <- SPARQL(endpoint,query)       # Exécution de la requête "query" via le endpoint

# <time>
time_E_query_exec <- Sys.time()

dataFrameResult <- QueryResult$results    # Enregistrer les résultats de la requête dans un data frame

dataFrameResult


# ********************************************************
# CREATING A LIST OF ALL RESULT OBJECTS
# ********************************************************
# <time>
time_B_result_obj_creation <- Sys.time()

#We create a list of result :
list_of_results = list()

#For each result in dataFrameResult :
for (i in 1:length(dataFrameResult[,1])){

  # We create a list of objets :
  list_of_objects = list()

  # For each objects in the result :
  for(j in 1:length(dataFrameResult[i,])){
    list_of_objects[j] <- Object(name=as.character(dataFrameResult[1,j]), links = list())
  }
  
  # We affect the created list of objects to the current result that we create :
  list_of_results[i] <- Result(objects = list_of_objects)
}

#list_of_subjects <- list_of_objects


# <time>
time_E_result_obj_creation <- Sys.time()



# ********************************************************
# Fonction permettant de r�cup�rer le voisinage d'un objet
# ********************************************************
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

    QueryResultFrame <- SPARQL(endpoint, queryvoisinage)  # pour chaque résultat de la requete initiale
    
    results <- QueryResultFrame$results # ici nous aurons que les résultats de SELECT
  }
  return (results)
}
#obj <- list_of_subjects[[1]] 
#results <- RequestNeighborhood(obj@name, endpoint)



isLiteral <-function(str){
  if(length(grep("^<.*>$", c(str), value = FALSE)) > 0){
    return(FALSE)
  }
  return(TRUE)
}

# ********************************************************
# CREATING LINKS OBJECTS FROM NEIGHBORHOOD
# ********************************************************
# <time>
time_B_link_obj_creation <- Sys.time()

#For each result :
for (k in 1:length(list_of_results)){
  #For each objects in the result :
  for (j in 1:length(list_of_results[[k]]@objects)){
    if(!isLiteral(list_of_results[[k]]@objects[[j]]@name)){
      # getting the subject's neighborhood :
      neighborhood <- RequestNeighborhood(list_of_results[[k]]@objects[[j]]@name, endpoint)
      
      # for each link in the neighborhood :
      for (i in 1:length(neighborhood[,1])){
        if(!is.na(neighborhood[i,1]) && !is.na(neighborhood[i,2])){
          # we create a link using the predicate and the object :
          list_of_results[[k]]@objects[[j]]@links[i]  <- Link(property = Object(name=neighborhood[i,1], links = list()), object = Object(name=neighborhood[i,2], links = list()))
        } else if(!is.na(neighborhood[i,1])){
          # we create a link using the predicate and the object :
          list_of_results[[k]]@objects[[j]]@links[i]  <- Link(property = Object(name=neighborhood[i,1], links = list()), object = Object(name=neighborhood[i,2], links = list()))
        }
      }      
    }
  }
}

# <time>
time_E_link_obj_creation <- Sys.time()





# ********************************************************
# CALCULATING TIME
# ********************************************************
time_query_exec <- time_length(interval(start = time_B_query_exec, end = time_E_query_exec), unit = "seconds") 
time_result_obj_creation <- time_length(interval(start = time_B_result_obj_creation, end = time_E_result_obj_creation), unit = "seconds") 
time_link_obj_creation <- time_length(interval(start = time_B_link_obj_creation, end = time_E_link_obj_creation), unit = "seconds")

require(grDevices)

# Create Data
Prop=c(as.numeric(time_query_exec) , as.numeric(time_result_obj_creation) , as.numeric(time_link_obj_creation) )

# You can also custom the labels:
pie(Prop , labels = c("Query execution","Creation of results objects","Creation of link objects"))
