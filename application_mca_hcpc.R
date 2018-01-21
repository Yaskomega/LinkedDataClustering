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

col <- list()
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
time_obj_preparing <- time_E_obj_preparing - time_B_obj_preparing
time_matrix_creation <- time_E_matrix_creation - time_B_matrix_creation
time_MCA <- time_E_MCA - time_B_MCA
time_HCPC <- time_E_HCPC - time_B_HCPC

require(grDevices)

# Create Data
Prop=c(as.numeric(time_query_exec) , as.numeric(time_result_obj_creation) , as.numeric(time_link_obj_creation),
       as.numeric(time_obj_preparing),as.numeric(time_matrix_creation),
       as.numeric(time_MCA),as.numeric(time_HCPC))

# You can also custom the labels:
pie(Prop , labels = c("Query execution","Creation of results objects","Creation of link objects",
                      "Preparing time of objects" , "Creation of the matrix",
                      "MCA execution" , "HCPC execution"))
