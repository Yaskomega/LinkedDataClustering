# ********************************************************
# SOURCE CODE
# ********************************************************

setwd("D:/Dropbox/Yassin/ESILV/A4/RESEARCH/R/LinkedDataClustering") # To use relative paths, and set the working directory appropriately, use setwd() to point R/RStudio at the directory that contains these files.
#source("functions.R")
source("classes.R")

# installation de la librairie  SPARQL : install.packages("SPARQL")
library(SPARQL)




# ********************************************************
# SPARQL ENDPOINT
# ********************************************************

endpoint <- "http://dbpedia.org/sparql"
# endpoint <- "http://localhost:8890/sparql"




# ********************************************************
# SPARQL QUERY
# ********************************************************

query <- "PREFIX db: <http://dbpedia.org/resource/>
PREFIX dbo: <http://dbpedia.org/ontology/>
SELECT DISTINCT ?oeuvre
WHERE {
?oeuvre dbo:author ?j.
#?oeuvre rdfs:label ?titre.
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




# ********************************************************
# CREATING A LIST OF ALL RESULT OBJECTS
# ********************************************************
# <time>
time_B_result_obj_creation <- Sys.time()

list_of_subjects = list()
for (i in 1:length(dataFrameResult)){
  list_of_subjects[i] <- Object(name=dataFrameResult[1,i], links = list())
}

# <time>
time_E_result_obj_creation <- Sys.time()



# ********************************************************
# Fonction permettant de r�cup�rer le voisinage d'un objet
# ********************************************************
RequestNeighborhood<-function(QueryResult, endpoint){
  
  if (grepl("%", QueryResult) ){
    
  }
  else{
    queryvoisinage <- paste("SELECT DISTINCT ?b ?c
                            WHERE{
                            {",
                            QueryResult, "?b ?c.
                            FILTER ( !( regex (?b, 'abstract', 'i') || regex ( ?b, 'comment', 'i')))
                            FILTER isIRI (?c)
                            } UNION
                            {",
                            QueryResult , " ?b ?c.
                            FILTER ( !( regex (?b , 'abstract', 'i') || regex ( ?b, 'comment', 'i') ))
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




# ********************************************************
# CREATING LINKS OBJECTS FROM NEIGHBORHOOD
# ********************************************************
# <time>
time_B_link_obj_creation <- Sys.time()
for (j in 1:length(list_of_subjects)){
  
  # getting the subject's neighborhood :
  neighborhood <- RequestNeighborhood(list_of_subjects[[j]]@name, endpoint)

  # for each link in the neighborhood :
  for (i in 1:length(neighborhood[,1])){
    
    # we create a link using the predicate and the object :
    list_of_subjects[[j]]@links[i]  <- Link(property = Object(name=neighborhood[i,1], links = list()), object = Object(name=neighborhood[i,2], links = list()))
  }
}
# <time>
time_E_link_obj_creation <- Sys.time()





# ********************************************************
# CALCULATING TIME
# ********************************************************
time_query_exec <- time_E_query_exec - time_B_query_exec
time_result_obj_creation <- time_E_result_obj_creation - time_B_result_obj_creation
time_link_obj_creation <- time_E_link_obj_creation - time_B_link_obj_creation

require(grDevices)

# Create Data
Prop=c(as.numeric(time_query_exec) , as.numeric(time_result_obj_creation) , as.numeric(time_link_obj_creation) )

# You can also custom the labels:
pie(Prop , labels = c("Query execution","Creation of results objects","Creation of link objects"))
