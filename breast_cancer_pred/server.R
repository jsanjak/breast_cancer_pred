# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#Load R libraries
library(shiny)
library(tidyverse)
library(DBI)
library(ggplot2)
library(ggfortify)
library(Rtsne)
library(cowplot)
library(corrplot)
library(plotly)
library(e1071)
library(randomForest)
library(class)
library(rfUtilities)
library(Cairo)

######################################
#Source user-defined helper functions#
source("helpers.R")

###################
#Load the datasets#
bc_data_db <- src_sqlite(path="data/breast_cancer_data.sqlite")
breast_cancer_data <- tbl(bc_data_db,"breast_cancer_data")
bc_reduced_data <- tbl(bc_data_db,"bc_reduced_data")
load("data/models.rds") #Pre-trained models

#############################################
# Define server logic required to run the app
shinyServer(function(input, output) {
  
  #This simply renders the raw data as a table
  output$table <- renderDataTable(
    select(breast_cancer_data,one_of(c("diagnosis",input$variable))))
  
  #Histogram plot of the distribution
  output$distPlot <- renderPlot({
    
    #Select the data based on the app input$variable
    bc_tbl <- select(breast_cancer_data,one_of(c("diagnosis",input$variable))) %>% 
      mutate(dg = ifelse(diagnosis=="B",0,1)) %>% collect()
    
    #Plot with ggplot
    #Color code by diagnosis
    #If diagnosis is the variable, then make a bar plot
    if(input$variable =="diagnosis"){
      p <- ggplot(bc_tbl,aes_string(x=input$variable,fill="diagnosis")) + 
        geom_bar(alpha=0.8) + theme_bw()
    } else {
      
      # generate bins based on input$bins from ui.R
      p <- ggplot(bc_tbl,aes_string(x=input$variable,fill="diagnosis")) + 
        geom_histogram(alpha=0.8,bins=input$bins) + theme_bw()
    }
    p <- p + scale_fill_manual(values=cbbPalette,labels=c("Benign","Malignant"))
    p <- p + guides(fill=guide_legend(title="Diagnosis"))
    p <- p + xlab(title_convert(input$variable))
    p <- p + theme(panel.grid.minor=element_blank(),
                   axis.text = element_text(size=14),
                   axis.title = element_text(size=16),
                   legend.position = "top")
    
    p
  })
  
  
  #Plot the single variable regression analysis
  output$regPlot <- renderPlot({  
    
    #Select the data based on the app input$variable
    bc_tbl <- select(breast_cancer_data,one_of(c("diagnosis",input$variable))) %>% 
      mutate(dg = ifelse(diagnosis=="B",0,1)) %>% collect()
    
    #Logistic regression or Probit regression
    #Often in bio we prefer logistic because we can interpret results as OR
    #sometimes probit is nicer because we can extend to unequal error variances
    p <- ggplot(bc_tbl,aes_string(y="dg",x=input$variable)) + geom_point(alpha=0.8) + 
      stat_smooth(method="glm",method.args = list(family = binomial(link = input$regType))) + 
      theme_bw()
    p <- p + guides(color=guide_legend(title="Diagnosis"))
    p <- p + xlab(title_convert(input$variable)) + 
      scale_y_continuous(name="Diagnosis", breaks=c(0,1),labels=break_label)
    p <- p + theme(panel.grid.minor=element_blank(),
                   axis.text = element_text(size=14),
                   axis.title = element_text(size=16))
    
    p
  })
  
  #Dimensionality reduction plots
  output$redPlot <- renderPlot({
    
    #Principal components analysis
    if (input$plotType=="pca"){
      p <- ggplot(collect(bc_reduced_data),aes(pc1,pc2,color=diagnosis))+geom_point()
      p <- p + theme_bw() + xlab("Principal Component 1") + ylab("Principal Component 2")
      p <- p + guides(color=guide_legend(title="Diagnosis"))
      p <- p + scale_color_manual(values=cbbPalette,labels=c("Benign","Malignant"))
      p <- p + theme(panel.grid.minor=element_blank())
      
    } else if (input$plotType=="tsne"){
      #t-Distributed Stochastic Neighbor Embedding
      p <- ggplot(collect(bc_reduced_data),aes(ts1,ts2,color=diagnosis))+geom_point()
      p <- p + theme_bw() + xlab("tSNE Component 1") + ylab("tSNE Component 2")
      p <- p + guides(color=guide_legend(title="Diagnosis"))
      p <- p + scale_color_manual(values=cbbPalette,labels=c("Benign","Malignant"))
      p <- p + theme(panel.grid.minor=element_blank())
      
    }
    p
  })
  
  #Correlation plots
  output$corPlot <- renderPlot({
    
    #Pass sort method and correlation style
    #We can hard code in the hclust method
    bc_data <- select( breast_cancer_data,one_of(input$checkGroup)) %>% collect()
    corrplot(cor(as.matrix(bc_data)),
             method=input$corType,
             type="lower",diag=T, tl.srt=60,
             order=input$order,
             hclust.method="ward.D2")
    
  })
  
  #Prediction plots
  output$predPlot <- renderPlot({
    bc_data <- breast_cancer_data %>% 
      mutate(dg = ifelse(diagnosis=="B",1,2)) %>% collect() 
    
    #Linear support vector machine
    if (input$predType=="linsvm"){
      lin.model <- svm(dg ~ ., type="C-classification",
                       data = bc_data[3:33], cost = lin.tsvm$best.parameters$cost,
                       cross=10, kernel="linear")
      lin.pred <- predict(lin.model, bc_data[3:33])
      bc_data <- bc_data %>% 
        mutate(Classification=predColor(lin.pred,dg)) 
      
      #Radial support vector machine
    } else if (input$predType=="radsvm"){
      rad.model <- svm(dg ~ ., type="C-classification",
                       data = bc_data[3:33], cost = rad.tsvm$best.parameters$cost,
                       cross=10, kernel="radial",gamma = rad.tsvm$best.parameters$gamma)
      rad.pred <- predict(rad.model, bc_data[3:33])
      bc_data <- bc_data %>% 
        mutate(Classification=predColor(rad.pred,dg)) %>% collect()
      
      #Random forest
    } else if (input$predType == "ranF"){
      
      ranF.model <- randomForest(y=as.factor(bc_data$dg),x=bc_data[,3:32],
                                 nodesize = bc_ranF$best.parameters$nodesize,
                                 mtry = bc_ranF$best.parameters$mtry,
                                 ntree = bc_ranF$best.parameters$ntree)
      ranF.pred <- round(ranF.model$votes[,2])+1
      bc_data <- bc_data %>% 
        mutate(Classification=predColor(ranF.pred,dg)) %>% collect()
      
      
    }
    p <- ggplot(bc_data,aes_string(x=input$variable1,y=input$variable2,
                                   color="Classification",shape="Classification")) + 
      geom_point(size=2,stroke=1) + theme_bw()
    p <- p + scale_color_manual(values=cbbPalette[1:4],
                                labels=c("Correctly benign",
                                         "Correctly malignant",
                                         "Incorrectly benign",
                                         "Incorrectly malignant"))
    p <- p + scale_shape_manual(values=c(16,17,3,4),labels=c("Correctly benign",
                                                             "Correctly malignant",
                                                             "Incorrectly benign",
                                                             "Incorrectly malignant"))
    p <- p + xlab(title_convert(input$variable1)) + ylab(title_convert(input$variable2))
    p <- p + theme(panel.grid.minor=element_blank(),
                   axis.text = element_text(size=14),
                   axis.title.y = element_text(size=16),
                   legend.title = element_blank())
    p
  })
  
  #Cross validation error plots
  output$cvPlot <- renderPlot({
    
    #Linear support vector machine
    if (input$predType=="linsvm"){
      
      p <- qplot(cost_range,lin.tsvm$performances$error,geom=c("point","line")) + geom_point(color="#009E73",size=2) + geom_line(color="#009E73") +theme_bw()
      p <- p + xlab("Cost") + ylab("Cross validation error")
      p <- p + theme(panel.grid.minor=element_blank(),
                     axis.text = element_text(size=14),
                     axis.title = element_text(size=16))
      
      #Radial support vector machine
    } else if (input$predType == "radsvm"){
      p <- ggplot(rad.tsvm$performances,aes(gamma,cost)) + 
        geom_raster(aes(fill=error),interpolate = T) + 
        scale_fill_gradientn(colours = rev(rainbow(100)))
      p <- p + geom_point(data=rad.tsvm$best.parameters,
                          aes(gamma,cost),size=2,shape=8,stroke=2,colour="green3")
      p <- p + xlab("Gamma") + ylab("Cost")+labs(fill="CV error") 
      p <- p + theme(panel.grid.minor=element_blank(),
                     axis.text = element_text(size=14),
                     axis.title = element_text(size=16))
      
      #Random forest
    } else if (input$predType == "ranF"){
      bestnode <- as.numeric(bc_ranF$best.parameters$nodesize)
      p <- ggplot(filter(bc_ranF$performances,
                         nodesize==bestnode),aes(mtry,ntree)) + 
        geom_raster(aes(fill=error),interpolate = F) + 
        scale_fill_gradientn(colours = rev(rainbow(100)))
      p <- p + geom_point(data=bc_ranF$best.parameters,
                          aes(mtry,ntree),size=2,shape=8,stroke=1.5,colour="green3")
      p <- p + xlab("Candidates per tree split (mtry)") + 
        ylab("Number of trees") + labs(fill="CV error") 
      p <- p + theme(panel.grid.minor=element_blank(),
                     axis.text = element_text(size=14),
                     axis.title = element_text(size=16))
      
    }
    p
  })
  
  #Statistics on prediction performance
  output$predStats <- renderTable({
    
    best <- c(lin.tsvm$best.performance,rad.tsvm$best.performance,bc_ranF$best.performance)
    xx <- data.frame(Model=c("Linear SVM","Radial SVM","Random Forest"),CV.Error=best)
  })
  
  #Mouse hover location
  output$hover_info <- renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      print(paste("x = ",signif(hover$x[1],3)," and y = ",signif(hover$y[1],3),sep=""),quote=F)
    }
  })
  
})
