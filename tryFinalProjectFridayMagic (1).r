#Load In Data
library(dplyr)
library(tidyverse)
library(ggplot2)
library(shiny)
library(caret)
options(java.parameters = "-Xmx64048m")

##Load data
setwd("/Users/dawsonmcmahon/Desktop/590RShiny")
untar("artifacts.tar.gz")
AutoModel <- readr::read_csv("artifacts/toy-model-testing-data.csv") %>% 
  sample_n(5000)
head(AutoModel,1)
summary(AutoModel$exposure)
set.seed(123)
inTrain <- createDataPartition(y = AutoModel$exposure,
                               p = 0.7,
                               list = F)
tr <- AutoModel[inTrain,]
te <- AutoModel[-inTrain,]
names(tr)
tr
trwOutliers <- tr #saving loaded tr and te with outliers
tewOutliers <- te
any(is.na(tr))
any(is.na(te))
getwd()
source("DataQualityReport.R")
DataQualityReport(tr)
DataQualityReport(te)

#Remove Outliers (EXPOSURE > 20)
summary(tr$data.exposure)
vehiageIQR <- (7-1)*1.5
vehiageIQR
#remove over 16 from te and tr
summary(te$exposure)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.500   0.500   0.500   4.209   2.500 683.500 
summary(tr$exposure)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.500   0.500   0.500   2.985   2.500 159.500
exposureIQR <- (2.5-.5)*1.5
exposureIQR
summary(te$loss_per_exposure)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0      0.0      0.0    802.2      0.0 201438.0 
summary(tr$loss_per_exposure)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0      0.0      0.0    721.5      0.0 112664.0 
tr <- tr[tr$vehicle_age<=16,]
te <- te[te$vehicle_age<=16,]
##Consider removing loss_per_exposure > 5000
##Consider Creating Dummy variables from all but 'make' here

#Zscore Standardization
preProcValues <- preProcess(tr[,2:ncol(tr)], method = c("center","scale"))
tr <- predict(preProcValues, tr)
tr #min-max normalized trv1 and tev1
# te set
preProcValues <- preProcess(te[,2:ncol(te)], method = c("center","scale"))
te <- predict(preProcValues, te)
te
rm(preProcValues) # clean up R environment
names(tr)
str(tr)
tr$sex <- as.factor(tr$sex)
tr$age_range <- as.factor(tr$age_range)
tr$make <- as.factor(tr$make)
tr$vehicle_category <- as.factor(tr$vehicle_category)
tr$region <- as.factor(tr$region)
DataQualityReport(tr)
te$sex <- as.factor(te$sex)
te$age_range <- as.factor(te$age_range)
te$make <- as.factor(te$make)
te$vehicle_category <- as.factor(te$vehicle_category)
te$region <- as.factor(te$region)
str(te)
DataQualityReport(te)
#########lets try Fiat
#lets try Fiat
trF <- subset(tr, tr$make=='FIAT') #subset tr to selected make
teF <- subset(te, te$make=="FIAT") #subset te to selected make
mExposureF <- train(exposure ~. , data=trF, method="svmLinear") #model exposure on selected make
trExpPredictF <- predict(mExposureF, newdata = trF)
teExpPredictF <- predict(mExposureF, newdata = teF)
trExpF <- data.frame(y=trF$exposure, yhat=trExpPredictF)
teExpF <- data.frame(y=teF$exposure, yhat=teExpPredictF)
trstatsExpF <- defaultSummary(data=data.frame(obs=trExpF$y, pred=trExpF$yhat))
testatsExpF <- defaultSummary(data=data.frame(obs=teExpF$y, pred=teExpF$yhat))
summary(mExposureF)
plot(x=trExpF$yhat, y=trExpF$y
     , main="Actual & Predicted"
     , xlab="Predicted Exposure"
     , ylab="Exposure"
     , pch=19)
abline(lm(y ~ yhat, data=trExpF))
print(trstatsExpF)
print(testatsExpF)

#UI
library(shiny)
ui <- fluidPage(
  titlePanel("Loss Per Exposure SLR"),
  sidebarPanel(
    fluidRow(
      selectInput(inputId = "make", 
                  label = "Choose a make",
                  choices = AutoModel$make,
                  selected = "FIAT"),
      
      uiOutput("makeSelection")),
    fluidRow(
      selectInput(inputId = "modelselector", 
                  label = "Choose a Problem Type",
                  choices = c("Linear","GBM","MASS None Robust","GLM Stats None","MASS None Recursive Partitioning","Random Forest","Partial Least Squares","Support Vector Machines","MASS None Robust Linear Regression"),
                  selected = "Linear"),
      
      uiOutput("modelSelection")),
    selectInput(inputId = "yselector", 
                label = "Choose a Y Variable",
                choices = c("Exposure","Loss Per Exposure"),
                selected = "Exposure"),
    
    uiOutput("ySelection")),
  mainPanel(
    h3("Regression Equation"), ##If Regression Start here
    verbatimTextOutput("ob1"),
    h3("Training Set"),
    verbatimTextOutput("ob2"),
    h3("Testing Set"),
    verbatimTextOutput("ob3"),
    h3("Sale Price"),
    plotOutput("plot1"),
  )
)

# SERVER
server <- function(input, output) {
  
  output$ob1 <- renderPrint({
    if(input$modelselector=="Linear" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="lm") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      summary(mExposure)}
      else{if(input$modelselector=="GBM" & input$yselector=="Exposure"){
        tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
        te1 <- subset(te, te$make==input$make) #subset te to selected make
        mExposure <- train(exposure ~. , data=tr1, method="gbm") #model exposure on selected make
        trExpPredict <- predict(mExposure, newdata = tr1)
        teExpPredict <- predict(mExposure, newdata = te1)
        trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
        teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
        trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
        testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
        summary(mExposure)}}
      else{if(input$modelselector=="GLM Stats None" & input$yselector=="Exposure"){
        tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
        te1 <- subset(te, te$make==input$make) #subset te to selected make
        mExposure <- train(exposure ~. , data=tr1, method="lmStepAIC") #model exposure on selected make
        trExpPredict <- predict(mExposure, newdata = tr1)
        teExpPredict <- predict(mExposure, newdata = te1)
        trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
        teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
        trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
        testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
               summary(mExposure)}}
       else{if(input$modelselector=="MASS None Recursive Partitioning" & input$yselector=="Exposure"){
        tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
        te1 <- subset(te, te$make==input$make) #subset te to selected make
        mExposure <- train(exposure ~. , data=tr1, method="rpart") #model exposure on selected make
       trExpPredict <- predict(mExposure, newdata = tr1)
       teExpPredict <- predict(mExposure, newdata = te1)
       trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
        teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
        trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
        testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
        summary(mExposure)}}
      else{if(input$modelselector=="Random Forest" & input$yselector=="Exposure"){
        tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
        te1 <- subset(te, te$make==input$make) #subset te to selected make
        mExposure <- train(exposure ~. , data=tr1, method="rf") #model exposure on selected make
        trExpPredict <- predict(mExposure, newdata = tr1)
        teExpPredict <- predict(mExposure, newdata = te1)
        trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
        teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
        trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
        testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
        summary(mExposure)}}
      else{if(input$modelselector=="Partial Least Squares" & input$yselector=="Exposure"){
        tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
        te1 <- subset(te, te$make==input$make) #subset te to selected make
        mExposure <- train(exposure ~. , data=tr1, method="pls") #model exposure on selected make
        trExpPredict <- predict(mExposure, newdata = tr1)
        teExpPredict <- predict(mExposure, newdata = te1)
        trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
        teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
        trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
        testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
        summary(mExposure)}}
      else{if(input$modelselector=="Support Vector Machines" & input$yselector=="Exposure"){
        tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
        te1 <- subset(te, te$make==input$make) #subset te to selected make
        mExposure <- train(exposure ~. , data=tr1, method="svmLinear") #model exposure on selected make
        trExpPredict <- predict(mExposure, newdata = tr1)
        teExpPredict <- predict(mExposure, newdata = te1)
       trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
        teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
       trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
       testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
       summary(mExposure)}}
     else{if(input$modelselector=="MASS None Robust Linear Regression" & input$yselector=="Exposure"){
        tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
       te1 <- subset(te, te$make==input$make) #subset te to selected make
       mExposure <- train(exposure ~. , data=tr1, method="rlm") #model exposure on selected make
        trExpPredict <- predict(mExposure, newdata = tr1)
        teExpPredict <- predict(mExposure, newdata = te1)
        trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
        teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
        trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
        testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
        summary(mExposure)}}
  })  

  output$plot1 <- renderPlot({
    if(input$modelselector=="Linear" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="lm") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      plot(x=trExp$yhat, y=trExp$y
           , main="Actual & Predicted"
           , xlab="Predicted Exposure"
           , ylab="Exposure"
           , pch=19)
      abline(lm(y ~ yhat, data=trExp))}
    else{if(input$modelselector=="GBM" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="gbm") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      plot(x=trExp$yhat, y=trExp$y
           , main="Actual & Predicted"
           , xlab="Predicted Exposure"
           , ylab="Exposure"
           , pch=19)
      abline(lm(y ~ yhat, data=trExp))}}
    else{if(input$modelselector=="GLM Stats None" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="lmStepAIC") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      plot(x=trExp$yhat, y=trExp$y
           , main="Actual & Predicted"
           , xlab="Predicted Exposure"
           , ylab="Exposure"
           , pch=19)
      abline(lm(y ~ yhat, data=trExp))}}
    else{if(input$modelselector=="MASS None Recursive Partitioning" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="rpart") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      plot(x=trExp$yhat, y=trExp$y
           , main="Actual & Predicted"
           , xlab="Predicted Exposure"
           , ylab="Exposure"
           , pch=19)
      abline(lm(y ~ yhat, data=trExp))}}
    else{if(input$modelselector=="Random Forest" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="rf") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      plot(x=trExp$yhat, y=trExp$y
           , main="Actual & Predicted"
           , xlab="Predicted Exposure"
           , ylab="Exposure"
           , pch=19)
      abline(lm(y ~ yhat, data=trExp))}}
    else{if(input$modelselector=="Partial Least Squares" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="pls") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      plot(x=trExp$yhat, y=trExp$y
           , main="Actual & Predicted"
           , xlab="Predicted Exposure"
           , ylab="Exposure"
           , pch=19)
      abline(lm(y ~ yhat, data=trExp))}}
    else{if(input$modelselector=="Support Vector Machines" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="svmLinear") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      plot(x=trExp$yhat, y=trExp$y
           , main="Actual & Predicted"
           , xlab="Predicted Exposure"
           , ylab="Exposure"
           , pch=19)
      abline(lm(y ~ yhat, data=trExp))}}
    else{if(input$modelselector=="MASS None Robust Linear Regression" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="rlm") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      plot(x=trExp$yhat, y=trExp$y
           , main="Actual & Predicted"
           , xlab="Predicted Exposure"
           , ylab="Exposure"
           , pch=19)
      abline(lm(y ~ yhat, data=trExp))}}
  })  

  
  output$ob2 <- renderPrint({
    if(input$modelselector=="Linear" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="lm") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(trstatsExp)}
    else{if(input$modelselector=="GBM" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="gbm") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(trstatsExp)}}
    else{if(input$modelselector=="GLM Stats None" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="lmStepAIC") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(trstatsExp)}}
    else{if(input$modelselector=="MASS None Recursive Partitioning" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="rpart") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(trstatsExp)}}
    else{if(input$modelselector=="Random Forest" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="rf") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(trstatsExp)}}
    else{if(input$modelselector=="Partial Least Squares" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="pls") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(trstatsExp)}}
    else{if(input$modelselector=="Support Vector Machines" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="svmLinear") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(trstatsExp)}}
    else{if(input$modelselector=="MASS None Robust Linear Regression" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="rlm") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(trstatsExp)}}
  })  

  output$ob3 <- renderPrint({
    if(input$modelselector=="Linear" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="lm") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(testatsExp)}
    else{if(input$modelselector=="GBM" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="gbm") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(testatsExp)}}
    else{if(input$modelselector=="GLM Stats None" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="lmStepAIC") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(testatsExp)}}
    else{if(input$modelselector=="MASS None Recursive Partitioning" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="rpart") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(testatsExp)}}
    else{if(input$modelselector=="Random Forest" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="rf") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(testatsExp)}}
    else{if(input$modelselector=="Partial Least Squares" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="pls") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(testatsExp)}}
    else{if(input$modelselector=="Support Vector Machines" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="svmLinear") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(testatsExp)}}
    else{if(input$modelselector=="MASS None Robust Linear Regression" & input$yselector=="Exposure"){
      tr1 <- subset(tr, tr$make==input$make) #subset tr to selected make
      te1 <- subset(te, te$make==input$make) #subset te to selected make
      mExposure <- train(exposure ~. , data=tr1, method="rlm") #model exposure on selected make
      trExpPredict <- predict(mExposure, newdata = tr1)
      teExpPredict <- predict(mExposure, newdata = te1)
      trExp <- data.frame(y=tr1$exposure, yhat=trExpPredict)
      teExp <- data.frame(y=te1$exposure, yhat=teExpPredict)
      trstatsExp <- defaultSummary(data=data.frame(obs=trExp$y, pred=trExp$yhat))
      testatsExp <- defaultSummary(data=data.frame(obs=teExp$y, pred=teExp$yhat))
      print(testatsExp)}}
  }) 

}

shinyApp(ui = ui, server = server)
