# ********************************************************
# SOURCE CODE
# ********************************************************

setwd("C:/path/to/directory/LinkedDataClustering") # To use relative paths, and set the working directory appropriately, use setwd() to point R/RStudio at the directory that contains these files.
source("common.R")
source("classes.R")
source("functions.R")
source("network_displayer.R")


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
} LIMIT 35 OFFSET 15"

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

list_of_subjects = list()
for (i in 1:length(dataFrameResult)){
  list_of_subjects[i] <- Object(name=dataFrameResult[1,i], links = list())
}

# <time>
time_E_result_obj_creation <- Sys.time()




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
time_query_exec <- time_length(interval(start = time_B_query_exec, end = time_E_query_exec), unit = "seconds") 
time_result_obj_creation <- time_length(interval(start = time_B_result_obj_creation, end = time_E_result_obj_creation), unit = "seconds") 
time_link_obj_creation <- time_length(interval(start = time_B_link_obj_creation, end = time_E_link_obj_creation), unit = "seconds")

require(grDevices)

# Create Data
Prop=c(as.numeric(time_query_exec) , as.numeric(time_result_obj_creation) , as.numeric(time_link_obj_creation) )

# You can also custom the labels:
pie(Prop , labels = c("Query execution","Creation of results objects","Creation of link objects"))
