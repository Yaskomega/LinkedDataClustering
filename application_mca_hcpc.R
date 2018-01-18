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

T5<-Sys.time()
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

# ***************************************************************************************************************
# ***************************************************************************************************************
#
#
# MCA & HCPC
#
#
# ***************************************************************************************************************
# ***************************************************************************************************************

# ********************************************************
# CREATING THE EMPTY MATRIX
# ********************************************************
data <- matrix(NA, length(list_of_subjects), number_of_columns)
colnames(data) <- col_to_string
rownames(data) <-  row_to_string
T6<-Sys.time()




# ********************************************************
# PREPARING MATRIX FOR MCA & HCPC
# ********************************************************
# Fillin the matrix : 
T7<-Sys.time()
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
T8<-Sys.time()

TT <- (T2-T1) + (T4-T3) + (T6-T5) + (T8-T7)
print(TT)




# ********************************************************
# MCA & HCPC
# ********************************************************

res.mca = MCA(data, quanti.sup = NULL, quali.sup = NULL, graph = FALSE)

res.hcpc = HCPC(res.mca, graph = TRUE)




# ********************************************************
# DISPLAYING MCA & HCPC
# ********************************************************

# pour faire des affichages ACM nous pouvons utiliser la bibliothèque explor
# install.packages("explor")
#library(explor)
#explor(res.mca)
# Clear console cat("\014")



