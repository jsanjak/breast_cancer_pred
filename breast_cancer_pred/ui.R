#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#Libraries
library(shiny)
library(plotly)
library(Cairo)
library(markdown)
library(DBI)

#Read data for names
bc_data_db <- src_sqlite(path="data/breast_cancer_data.sqlite")
breast_cancer_data <- tbl(bc_data_db,"breast_cancer_data") %>% select(3:32)

shinyUI(
  navbarPage("Wisconsin breast cancer data",
             tabPanel("Welcome!",
                      column(10,
                             includeMarkdown("include.md")
                      )),
             
             #There will be a navigation bar on top 
             navbarMenu("Explore", # exploratory data analyses
                        tabPanel("Distribution", #Just look at the distrubutions with histograms
                                 sidebarLayout(
                                   sidebarPanel(
                                     sliderInput("bins",
                                                 "Number of bins:",
                                                 min = 1,
                                                 max = 50,
                                                 value = 30), 
                                     radioButtons("regType", "Regression type", #and regressions
                                                  c("Logistic"="logit","Probit"="probit"),
                                                  inline=TRUE),
                                     selectInput("variable","Variable:",
                                                 colnames(breast_cancer_data))
                                   ),
                                   mainPanel(
                                     plotOutput("distPlot",hover="plot_hover"),
                                     plotOutput("regPlot",hover="plot_hover")
                                   )
                                 )
                        ),
                        tabPanel("Correlations", #Display correlation strucutre of data
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("corType", "Correlation visual",
                                                  c("Circle"="cir", 
                                                    "Ellipse"="ellipse",
                                                    "Square"="square"),
                                                  inline=TRUE),
                                     radioButtons("order", "Order variables",
                                                  c("Original"="original", 
                                                    "Alphabetical"="alphabet",
                                                    "Cluster"="hclust",
                                                    "First PCA loading"="FPC", 
                                                    "Eigenvalues"="AOE"),
                                                  inline=TRUE),
                                     checkboxGroupInput("checkGroup","Variables", 
                                                        choices = colnames(breast_cancer_data),
                                                        selected = colnames(breast_cancer_data))
                                   ),
                                   mainPanel(
                                     plotOutput("corPlot",height="800px",hover="plot_hover")
                                   )
                                 )
                        ),
                        tabPanel("Reduce Dimenions", #Look at precomputed reduced data sets
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("plotType", "Reduction method",
                                                  c("PCA"="pca", "tSNE"="tsne")
                                     )
                                   ),
                                   mainPanel(
                                     plotOutput("redPlot",hover="plot_hover")
                                   )
                                 )
                        )
                        
             ),
         tabPanel("Prediction", #Take a look at some simple precomputed predictive models
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("predType","Prediction method:",
                                      c("Linear SVM" = "linsvm",
                                        "Radial SVM" = "radsvm",
                                        "Random Forest" = "ranF"
                                      )),
                          selectInput("variable1","x-axis:",colnames(breast_cancer_data)),
                          selectInput("variable2","y-axis:",colnames(breast_cancer_data)),
                          tableOutput("predStats"),
                          fluidRow(
                            column(width = 12,
                                   verbatimTextOutput("hover_info")
                            )
                          )
                        ),
                        mainPanel(
                          plotOutput("predPlot",hover="plot_hover"),
                          plotOutput("cvPlot",hover="plot_hover")
                        )
                      )
             )
             
  )
)

