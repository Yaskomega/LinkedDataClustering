# ********************************************************
# SOURCE CODE
# ********************************************************

setwd("D:/Dropbox/Yassin/ESILV/A4/RESEARCH/R/LinkedDataClustering") # To use relative paths, and set the working directory appropriately, use setwd() to point R/RStudio at the directory that contains these files.
#source("functions.R")
#source("application_global.R")

# installation de la librairie  FactoMineR : install.packages("FactoMineR")
library(FactoMineR)




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
            #new_index <- length(list_of_columns[j]) + 1
            #list_of_columns[[j]][new_index] <- link 
            list_of_columns <- addLinkToList(list_of_columns, link, j)
            print("ADDING NEW COLUMN LIST OF COLUMNS")
            #list_of_columns[j] <- c(list_of_columns[j] , c(link))
          }

          # adding the name in the list :
          # If the concerned list of name doesn't exist :
          if(is.na(list_of_column_names[[j]])){
            list_of_column_names[j] <- c(paste(j, "||" , link@property@name))
            list_of_column_names[[j]][2] <- paste(j, "||" , link@property@name ,"||", link@object@name)
          }else{
            index1 <- length(list_of_column_names[j]) + 1
            index2 <- length(list_of_column_names[j]) + 2
            list_of_column_names[[j]][index1] <- paste(j, "||" , link@property@name)
            list_of_column_names[[j]][index2] <- paste(j, "||" , link@property@name ,"||", link@object@name)
          }  
        }
      }  
    }
  }
}
#!contains(list_of_columns[[j]], link)

# <time>
time_E_obj_preparing <- Sys.time()



# ***************************************************************************************************************
# ***************************************************************************************************************
#
#
# MCA & HCPC
#
#
# ***************************************************************************************************************
# ***************************************************************************************************************





# <time>
time_B_matrix_creation <- Sys.time()





# ********************************************************
# CREATING THE EMPTY MATRIX
# ********************************************************
data <- matrix(NA, length(list_of_subjects), number_of_columns)
colnames(data) <- col_to_string
rownames(data) <-  row_to_string





# ********************************************************
# PREPARING MATRIX FOR MCA & HCPC
# ********************************************************
# Fillin the matrix : 

for (j in 1:length(list_of_subjects)){
  # selecting the current subject :
  subject <- list_of_subjects[[j]] 
  
  for (i in 1:length(col)){
    # select the current link :
    link <- col[[i]]
    
    col_num <- i*2-1
    if(containsPropertyAndObject(subject@links, link)){
      data[j,col_num] <- col_to_string[col_num]
      data[j,col_num+1] <- col_to_string[col_num+1]
    } else if(containsProperty(subject@links, link)){
      data[j,col_num] <- col_to_string[col_num]
    }
  }
}





# <time>
time_E_matrix_creation <- Sys.time()





# ********************************************************
# MCA & HCPC
# ********************************************************
# <time>
time_B_MCA <- Sys.time()

res.mca = MCA(data, quanti.sup = NULL, quali.sup = NULL, graph = FALSE)

# <time>
time_E_MCA <- Sys.time()

# <time>
time_B_HCPC <- Sys.time()

res.hcpc = HCPC(res.mca, graph = FALSE)
#res.hcpc$desc.ind

# <time>
time_E_HCPC <- Sys.time()




# ********************************************************
# DISPLAYING MCA & HCPC
# ********************************************************

# pour faire des affichages ACM nous pouvons utiliser la bibliothèque explor
# install.packages("explor")
#library(explor)
#explor(res.mca)
# Clear console cat("\014")




# ********************************************************
# CALCULATING TIME
# ********************************************************
time_obj_preparing <- time_length(interval(start = time_B_obj_preparing, end = time_E_obj_preparing), unit = "seconds") 

time_matrix_creation <- time_length(interval(start = time_B_matrix_creation, end = time_E_matrix_creation), unit = "seconds") 

time_MCA <- time_length(interval(start = time_B_MCA, end = time_E_MCA), unit = "seconds") 

time_HCPC <- time_length(interval(start = time_B_HCPC, end = time_E_HCPC), unit = "seconds") 

require(grDevices)

# Create Data
Prop=c(as.numeric(time_query_exec) , as.numeric(time_result_obj_creation) , as.numeric(time_link_obj_creation),
       as.numeric(time_obj_preparing),as.numeric(time_matrix_creation),
       as.numeric(time_MCA),as.numeric(time_HCPC))

# You can also custom the labels:
pie(Prop , labels = c("Query execution","Creation of results objects","Creation of link objects",
                      "Preparing time of objects" , "Creation of the matrix",
                      "MCA execution" , "HCPC execution"))
