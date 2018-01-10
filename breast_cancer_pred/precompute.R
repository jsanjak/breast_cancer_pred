#This script does some pre-computation of models
#and data sets for the breast cancer data shiny app
#Load libraries
library(tidyverse)
library(DBI)
library(ggplot2)
library(ggfortify)
library(Rtsne)
library(cowplot)
library(corrplot)
library(e1071)
library(plotly)
library(rpart)
library(nnet)
library(randomForest)
library(class)
library(rfUtilities)
set.seed(101)

#This is how the dataset comes off the Kaggle website
bc_data <- read_csv("~/projects/breast_cancer_pred/breast_cancer_data.csv")

#Grab the diagnosis data
diagnosis <- bc_data["diagnosis"]


#Principal component analysis
#Center and scale data
bc_prcomp <- prcomp(select(bc_data,3:32),
                    center=TRUE,
                    scale.=TRUE)

#t-Distributed Stochastic Neighbor Embedding
bc_Rtsne <- Rtsne(select(bc_data,3:32), 
                  dims = 2, 
                  perplexity=40, 
                  max_iter = 2000,
                  pca=T,
                  initial_dims=30)

bc_dim_reduce <- diagnosis %>% mutate(pc1=bc_prcomp$x[,1],
                                      pc2=bc_prcomp$x[,3],
                                      ts1=bc_Rtsne$Y[,1],
                                      ts2=bc_Rtsne$Y[,2])

#Open and sqlite database to store the data
bc_db <- dbConnect(RSQLite::SQLite(), "data/breast_cancer_data.sqlite")
dbWriteTable(bc_db, "breast_cancer_data", bc_data)
dbWriteTable(bc_db, "bc_reduced_data", bc_dim_reduce)
dbDisconnect(bc_db)


#SVM training
#Specificy the parameters over which to tune the model
gamma_range <- seq(0.01,1,0.01)
cost_range <- seq(0.01,10,0.1)

#Tune the SVM model, using 10-fold cross validation
rad.tsvm <- tune.svm(diagnosis ~., 
                     data=fullset,
                     kernel="radial",
                     gamma=gamma_range,
                     cost=cost_range,
                     tunecontrol=tune.control(cross = 10))

lin.tsvm <- tune.svm(diagnosis ~., 
                     data=fullset,
                     kernel="linear",
                     gamma=NULL,
                     cost=cost_range,
                     tunecontrol=tune.control(cross = 10))

#Random Forest training
#Specify parameters
node_range=1:5
ntree_range=c(1000,2000,3000,4000,5000)
mtry_range=4:10

bc_ranF <- tune.randomForest(y=as.factor(fullset$diagnosis),
                             x=fullset[,2:31],data=fullset,
                             nodesize=1:5,ntree=seq(1000,10000,1),mtry=4:10)

##TODO
#naiveBayes
#rpart
#nnet
#knn

#Save pre-trained models
save(list=c("gamma_range","cost_range",
            "node_range","ntree_range","mtry_range",
            "lin.tsvm","rad.tsvm","sig.tsvm",
            "bc_ranF"),
     file="data/models.rds")
