# ********************************************************
# SOURCE CODE
# ********************************************************

# setwd("C:/path/to/directory/LinkedDataClustering") # To use relative paths, and set the working directory appropriately, use setwd() to point R/RStudio at the directory that contains these files.
#source("functions.R")
#source("application_global.R")

# installation de la librairie  FactoMineR : install.packages("FactoMineR")
# installation de la librairie  dynamicTreeCut : install.packages("dynamicTreeCut")
library(FactoMineR)
library(dynamicTreeCut)




# ********************************************************
# BUILDING THE LIST OF LINK AND THE COLUMNS NAMES AND ROWS NAMES
# ********************************************************

# <time>
time_B_obj_preparing <- Sys.time()

col <- list() # list of all links possibles
#row <- c()
col_to_string <- c()
row_to_string <- c()
number_of_columns <- 0
for (j in 1:length(list_of_subjects)){
  # selecting the current subject :
  subject <- list_of_subjects[[j]] 
  
  # adding the new row name :
  row_to_string[j] <- subject@name
  
  for (i in 1:length(subject@links)){
    #for (i in 1:4){
    # selecting the current link :
    link <- subject@links[[i]]
    
    # adding the link in the list if not already added
    if(!contains(col, link)){
      
      # adding the link in the list (no need to add it twice):
      col[number_of_columns +1] <- link
      
      # building column name :
      col_to_string[number_of_columns + 1] <- paste("||" , link@property@name ,"||", link@object@name)
      
      # increment :
      number_of_columns <- number_of_columns + 1
    }
  }
}

# <time>
time_E_obj_preparing <- Sys.time()




# ***************************************************************************************************************
# ***************************************************************************************************************
#
#
# DBSCAN
#
#
# ***************************************************************************************************************
# ***************************************************************************************************************





# <time>
time_B_matrix_creation <- Sys.time()






# ********************************************************
# CREATING THE EMPTY MATRIX
# ********************************************************
data <- matrix(NA, length(list_of_subjects), length(col))
colnames(data) <- col_to_string
rownames(data) <-  row_to_string




# ********************************************************
# PREPARING MATRIX FOR DBSCAN
# ********************************************************
for (j in 1:length(list_of_subjects)){
  # selecting the current subject :
  subject <- list_of_subjects[[j]] 
  
  for (i in 1:length(col)){
    # select the current link :
    link <- col[[i]]
    
    col_num <- i*2-1
    if(containsPropertyAndObject(subject@links, link)){
      data[j,i] <- 2
    } else if(containsProperty(subject@links, link)){
      data[j,i] <- 1
    } else {
      data[j,i] <- 0
    }
  }
}



# <time>
time_E_matrix_creation <- Sys.time()






# ********************************************************
# USING DBSCAN
# ********************************************************
library(dbscan)

# <time>
time_B_dbscan <- Sys.time()

res <- dbscan(data, eps = .4, minPts = 2)
#res

# <time>
time_E_dbscan <- Sys.time()


# ********************************************************
# CALCULATING TIME
# ********************************************************
time_obj_preparing <- time_length(interval(start = time_B_obj_preparing, end = time_E_obj_preparing), unit = "seconds") 

time_matrix_creation <- time_length(interval(start = time_B_matrix_creation, end = time_E_matrix_creation), unit = "seconds") 

time_dbscan <- time_length(interval(start = time_B_dbscan, end = time_E_dbscan), unit = "seconds") 


require(grDevices)

# Create Data
Prop=c(as.numeric(time_query_exec) , as.numeric(time_result_obj_creation) , as.numeric(time_link_obj_creation),
       as.numeric(time_obj_preparing),as.numeric(time_matrix_creation),
       as.numeric(time_dbscan))

# You can also custom the labels:
pie(Prop , labels = c("Query execution","Creation of results objects","Creation of link objects",
                      "Preparing time of objects" , "Creation of the matrix",
                      "dbscan execution"))