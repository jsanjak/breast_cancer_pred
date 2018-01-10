library(tidyverse)
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

bc_data <- read_csv("~/projects/breast_cancer_pred/breast_cancer_data.csv")

saveRDS(bc_data,file="data/breast_cancer_data.rds")

diagnosis <- bc_data["diagnosis"]


bc_prcomp <- prcomp(select(bc_data,3:32),
                    center=TRUE,
                    scale.=TRUE)

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

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p <- ggplot(bc_dim_reduce,aes(pc1,pc2,color=diagnosis))+geom_point()
p <- p + theme_bw() + xlab("Principal Component 1") + ylab("Principal Component 2")
p <- p + guides(color=guide_legend(title="Diagnosis"))
p <- p + scale_color_manual(values=cbbPalette,labels=c("Benign","Malignant"))
p <- p + theme(panel.grid.minor=element_blank())

q <- ggplot(bc_dim_reduce,aes(ts1,ts2,color=diagnosis))+geom_point()
q <- q + theme_bw() + xlab("tSNE Component 1") + ylab("tSNE Component 2")
q <- q + guides(color=guide_legend(title="Diagnosis"))
q <- q + scale_color_manual(values=cbbPalette,labels=c("Benign","Malignant"))
q <- q + theme(panel.grid.minor=element_blank())

legend <- get_legend(p)
pq <- plot_grid(p + theme(legend.position="none"),
                q + theme(legend.position="none"),
                legend,
                nrow=1, rel_widths = c(1,1,.3),
                labels=c("A","B"))
pq


corrplot(cor(as.matrix(select(bc_data,3:32))))


#SVM examples
index <- 1:nrow(bc_data)
testindex <- sample(index,trunc(length(index))/5)

testset <- bc_data[testindex,2:32] %>% mutate(diagnosis =ifelse(diagnosis=="B",0,1))
trainset <- bc_data[-testindex,2:32]%>% mutate(diagnosis =ifelse(diagnosis=="B",0,1))
fullset <- bc_data[2:32] %>% mutate(diagnosis =ifelse(diagnosis=="B",0,1))
svm.model <- svm(diagnosis ~ ., type="C-classification",
                 data = trainset, cost = .5,
                 cross=4, kernel="radial", gamma = .1)
svm.pred <- predict(svm.model, testset[,-1])
confMat <- table(pred = svm.pred, true = testset$diagnosis)
classAgreement(confMat)

plot(svm.model,data=trainset,formula=as.formula("radius_mean ~ texture_mean"))

gamma_range <- seq(0.01,1,0.01)
cost_range <- seq(0.01,10,0.1)


rad.tsvm <- tune.svm(diagnosis ~., 
                 data=fullset,
                 kernel="radial",
                 gamma=gamma_range,
                 cost=cost_range,
                 tunecontrol=tune.control(cross = 10))

sig.tsvm <- tune.svm(diagnosis ~., 
                     data=fullset,
                     kernel="sigmoid",
                     gamma=gamma_range,
                     cost=cost_range,
                     tunecontrol=tune.control(cross = 10))

lin.tsvm <- tune.svm(diagnosis ~., 
                     data=fullset,
                     kernel="linear",
                     gamma=NULL,
                     cost=cost_range,
                     tunecontrol=tune.control(cross = 10))





p <- plot_ly(x = gamma_range,
             y = cost_range,
             z = matrix(sig.tsvm$performances$error,nrow=length(gamma_range),ncol=length(cost_range),byrow=T),
             reversescale=T) %>% add_surface() %>% layout(
               title="Cross validation error",
               scene=list(
               xaxis = list(title = "Gamma",
                            range=c(0.01,0.5)),
               yaxis = list(title="Cost",
                            range=c(0.01,0.5)),
               zaxis = list(title="CV error"))
             ) 
p

p <- plot_ly(x = cost_range,
             y = lin.tsvm$performances$error) %>% add_lines() %>% layout(
               title="Cross validation error",
                 xaxis = list(title = "Cost",
                              range=c(0.01,0.5)),
                 yaxis = list(title="CV error")
             ) 
p


rad.model <- svm(diagnosis ~ ., type="C-classification",
                 data = fullset, cost = rad.tsvm$best.parameters$cost,
                 cross=10, kernel="radial", gamma = rad.tsvm$best.parameters$gamma)

sig.model <- svm(diagnosis ~ ., type="C-classification",
                 data = fullset, cost = sig.tsvm$best.parameters$cost,
                 cross=10, kernel="sigmoid", gamma = sig.tsvm$best.parameters$gamma)

sig.bad <- svm(diagnosis ~ ., type="C-classification",
                 data = fullset, cost = sig.tsvm$performances[9790,]$cost,
                 cross=10, kernel="sigmoid", gamma = sig.tsvm$performances[9790,]$gamma)


lin.model <- svm(diagnosis ~ ., type="C-classification",
                 data = fullset, cost = lin.tsvm$best.parameters$cost,
                 cross=10, kernel="linear")


svm.pred <- predict(svm.model, trainset[,-1])
confMat <- table(pred = svm.pred, true = trainset$diagnosis)
classAgreement(confMat)


predColor <- function(pred,truth){
  
  output <- rep(0,length(pred))
  for( i in 1:length(pred)){
    if(truth[i]==pred[i]){
      if (truth[i]==1){
        output[i] <- "Correctly benign"
      } else {
        output[i] <- "Correctly malignant"
      }
      
    } else {
      if (truth[i]==1){
        output[i] <- "Incorrectly benign"
      } else {
        output[i] <- "Incorrectly malignant"
      }
    }
  }
  return(output)
}
lin.pred <- predict(lin.model, trainset[,-1])

plot(as.formula("radius_mean ~ smoothness_mean"),data=trainset,col=predColor(lin.pred,trainset$diagnosis))

plot(lin.model,data=trainset,formula=as.formula("radius_mean ~ smoothness_mean"),fill=T)




#mtry, nodesize, ntree
node_range=1:5
ntree_range=c(1000,2000,3000,4000,5000)
mtry_range=4:10
bc_ranF <- tune.randomForest(y=as.factor(fullset$diagnosis),
                        x=fullset[,2:31],data=fullset,
                        nodesize=1:5,ntree=seq(1000,10000,1),mtry=4:10)

bc_ranF_best <- randomForest(y=as.factor(fullset$diagnosis),x=fullset[,2:31],
                             nodesize = bc_ranF$best.parameters$nodesize,
                             mtry = bc_ranF$best.parameters$mtry,
                             ntree = bc_ranF$best.parameters$ntree)
bc_ranF_CV
##TODO
#naiveBayes
#rpart
#nnet
#knn

save(list=c("gamma_range","cost_range",
            "node_range","ntree_range","mtry_range",
            "lin.tsvm","rad.tsvm","sig.tsvm",
            "bc_ranF"),
     file="data/models.rds")
