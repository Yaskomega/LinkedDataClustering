# ********************************************************
# SOURCE CODE
# ********************************************************

setwd("D:/Dropbox/Yassin/ESILV/A4/RESEARCH/R/LinkedDataClustering") # To use relative paths, and set the working directory appropriately, use setwd() to point R/RStudio at the directory that contains these files.
#source("functions.R")
#source("application_global.R")

# installation de la librairie  dynamicTreeCut : install.packages("dynamicTreeCut")
library(dynamicTreeCut)




# ********************************************************
# BUILDING THE LIST OF LINK AND THE COLUMNS NAMES AND ROWS NAMES
# ********************************************************

# <time>
time_B_obj_preparing <- Sys.time()

#
# Building row names :
#
row_to_string <- c()
for(i in 1:length(list_of_results)){
  row_to_string[i] <- paste("Result ", i)
}

# List of columns, contains vectors (one for each variable ?x ?y ?z ...), each vector contains links :
list_of_columns <- list()

# List of column names, contains vectors (one for each variable ?x ?y ?z ...), each vector contains links :
list_of_column_names <- list()

for(i in 1:length(list_of_results[[1]]@objects)){
  list_of_columns[i] <- NA
  list_of_column_names[i] <- NA
}

# For each result :
for(i in 1:length(list_of_results)){
  
  # For each object in the result :
  for (j in 1:length(list_of_results[[i]]@objects)){
    
    # selecting the current object :
    object <- list_of_results[[i]]@objects[[j]]
    
    # If the current object has links :
    if(length(object@links) > 0){
      
      # For each link of the current object :
      for (k in 1:length(object@links)){
        
        # selecting the current link :
        link <- object@links[[k]]
        
        # adding the link in the list if not already added
        if(is.na(list_of_columns[[j]]) || !contains(list_of_columns[j] , link)){
          print("NEW LINK TO ADD")
          # adding the link in the list (no need to add it twice):
          # If the concerned list of columns doesn't exist :
          if(is.na(list_of_columns[[j]])){
            print("INITIALIZE LIST OF COLUMNS")
            list_of_columns[j] <- list(link)
          }else{
            list_of_columns <- addItemToListInMatrix(list_of_columns, link, j)
            print("ADDING NEW COLUMN LIST OF COLUMNS")
          }
          
          # adding the name in the list :
          # If the concerned list of name doesn't exist :
          if(is.na(list_of_column_names[[j]])){
            list_of_column_names[j] <- c(paste(j, "||" , link@property@name))
            list_of_column_names[[j]][2] <- paste(j, "||" , link@property@name ,"||", link@object@name)
          }else{
            list_of_column_names <- addItemToListInMatrix(list_of_column_names, paste(j, "||" , link@property@name), j)
            list_of_column_names <- addItemToListInMatrix(list_of_column_names, paste(j, "||" , link@property@name ,"||", link@object@name), j)
          }  
        }
      }  
    }
  }
}

# <time>
time_E_obj_preparing <- Sys.time()



# ***************************************************************************************************************
# ***************************************************************************************************************
#
#
# HCLUST & DYNAMIC TREE CUT
#
#
# ***************************************************************************************************************
# ***************************************************************************************************************





# <time>
time_B_matrix_creation <- Sys.time()





# ********************************************************
# CREATING THE EMPTY MATRIX
# ********************************************************
# Merging all the columns names into one vector :
col_to_string <- c()
for(i in 1:length(list_of_column_names)){
  if(!is.na(list_of_column_names[[i]])){
    col_to_string <- c(col_to_string, as.vector(list_of_column_names[[i]])) 
  }
}

# Creating the empty matrix :
data <- matrix(NA, length(list_of_results), length(col_to_string))
colnames(data) <- col_to_string
rownames(data) <-  row_to_string




# ********************************************************
# BUILDING THE BINARY MATRIX
# ********************************************************

# Fillin the matrix : 
column_name_index <- 0
for(k in 1:length(list_of_results[[1]])){
  if(!is.na(list_of_columns[[k]])){
    for (j in 1:length(list_of_results)){
      # selecting the current object :
      object <- list_of_results[[j]]@objects[[k]]
      
      for (i in 1:length(list_of_columns[[k]])){
        # select the current link :
        link <- list_of_columns[[k]][[i]]
        
        col_num <- i*2-1
        if(containsPropertyAndObject(object@links, link)){
          data[j,col_num + column_name_index] <- 1
          data[j,col_num+1 + column_name_index] <- 1
        } else if(containsProperty(object@links, link)){
          data[j,col_num + column_name_index] <- 1
          data[j,col_num+1 + column_name_index] <- 0
        } else {
          data[j,col_num + column_name_index] <- 0
          data[j,col_num+1 + column_name_index] <- 0
        }
      }
    }  
  }
  column_name_index <- column_name_index + length(list_of_column_names[[k]])
}
binary_matrix <- data




# ********************************************************
# BUILDING THE DISTANCE MATRIX
# ********************************************************

dist_matrix <- dist(binary_matrix, method = "binary")



# <time>
time_E_matrix_creation <- Sys.time()





# ********************************************************
# USING HCLUST TO BUILD THE DENDOGRAM
# ********************************************************
# <time>
time_B_hclust <- Sys.time()

dendogram <- hclust(dist_matrix)

# <time>
time_E_hclust <- Sys.time()

plot(dendogram)






# <time>
time_B_dynamic_cut <- Sys.time()

# ********************************************************
# DYNAMICALLY DEFINING MINIMUM CLUSTER SIZE
# ********************************************************
number_of_results = length(data[,1])
maximum_number_of_cluster = 10
minClusterSize = number_of_results / maximum_number_of_cluster




# ********************************************************
# BUILDING CLUSTERS USING A DYNAMIC CUT
# ********************************************************
clusters <- NULL
#clusters = cutreeDynamic(dendogram, cutHeight = NULL, minClusterSize = 20,distM = as.matrix(dist_matrix), pamStage = FALSE)
#clusters = cutreeDynamic(dendogram, distM = as.matrix(dist_matrix))
#clusters = cutreeDynamic(dendogram, minClusterSize=minClusterSize, method="hybrid",  distM = as.matrix(dist_matrix))
clusters = cutreeDynamic(dendogram, minClusterSize= 1,deepSplit = 4, minGap = 0.05, method="hybrid",  distM = as.matrix(dist_matrix))
#plot(clusters)

# <time>
time_E_dynamic_cut <- Sys.time()




# ********************************************************
# CALCULATING TIME
# ********************************************************
time_obj_preparing <- time_length(interval(start = time_B_obj_preparing, end = time_E_obj_preparing), unit = "seconds") 

time_matrix_creation <- time_length(interval(start = time_B_matrix_creation, end = time_E_matrix_creation), unit = "seconds") 

time_hclust <- time_length(interval(start = time_B_hclust, end = time_E_hclust), unit = "seconds") 

time_dynamic_cut <- time_length(interval(start = time_B_dynamic_cut, end = time_E_dynamic_cut), unit = "seconds") 


require(grDevices)

# Create Data
Prop=c(as.numeric(time_query_exec) , as.numeric(time_result_obj_creation) , as.numeric(time_link_obj_creation),
       as.numeric(time_obj_preparing),as.numeric(time_matrix_creation),
       as.numeric(time_hclust),as.numeric(time_dynamic_cut))

# You can also custom the labels:
pie(Prop , labels = c("Query execution","Creation of results objects","Creation of link objects",
                      "Preparing time of objects" , "Creation of the matrix",
                      "hclust execution" , "DynamicTreeCut execution"))
