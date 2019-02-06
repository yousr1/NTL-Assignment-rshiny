## app.R ##
library(shinydashboard)
library(ggplot2)
options(shiny.maxRequestSize=30*1024^2) 

assign('y_sample',y_sample,envir=globalenv())
assign('mean_tc',mean_tc,envir=globalenv())
assign('std_tc',std_tc,envir=globalenv())

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Scoring", tabName = "upload", icon = icon("dashboard")),
      menuItem("Missing Data", icon = icon("th"), tabName = "missing"),
      menuItem("Encoding", tabName = "encode", icon = icon("dashboard")),
      menuItem("Scaling", icon = icon("th"), tabName = "scaling"),
      menuItem("choosing the model", icon = icon("th"), tabName = "choosingthemodel")
      
    )
  )    ,
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
          fluidRow(
            box(
              width = 12,
              title = "Upload Dataset",
              status = "success",
              olidHeader = TRUE,
              fileInput("dataset", "Choose CSV File",
                        multiple = TRUE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv"))
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "View Dataset",
              status = "success",
              solidHeader = TRUE,
              dataTableOutput("df")
            )
          )
          
      ),
      
      tabItem(tabName = "missing",
              tags$h2("Missing"),
              tableOutput("nan"),
              actionButton("fillna", "Fill NaN")
              
      ),
      tabItem(tabName = "encode",
              tags$h2("Encoding"),
              actionButton("encode", "Encode")
      ),
      
      tabItem(tabName = "scaling",
              tags$h2("Scaling"),
              actionButton("scaling", "Scale")
              
      ),
      
      
      tabItem( tabName = "choosingthemodel",
               
               fluidRow(
                 column(width = 6,
                        
                        tags$h2("choose model"),
                        selectInput("var", "Choose an Algorithm :",
                                    c("Logistic Regression" = "lr",
                                      "KNN " = "KNN",
                                      "Support vector machines" = "SVM",
                                      " naive bayes"='nb',
                                      "Descision trees"='dt',
                                      "random forest"="rf"))  ,
                        box( tags$h2("Results"),
                             tableOutput('table'),
                             tags$h4("Accuracy"),
                             textOutput('text1'),
                             tags$h4("precision"),
                             textOutput('text2'),
                             tags$h4("Recall"),
                             textOutput('text3'),
                             tags$h4("F-score"),
                             textOutput('text4') ,
                             tags$h5("Prediction whether the client will churn :"),
                             verbatimTextOutput("y_out") )
                        
                 ),
                 
                 column(width = 6, tags$h2("Predict"),selectInput("dep", "Dependents : ",
                                                        c("Yes" = 1,
                                                          "No" = 0)),
                         selectInput("pb", "PaperlessBilling : ",
                                     c("Yes" = 1,
                                       "No" = 0)),
                         selectInput("ob", "OnlineBackup : ",
                                     c("No" = 0,
                                       "No internet service" = 1,
                                       'Yes'=2)),
                         
                         textInput("tc", "totalcharges",placeholder='if not provided default is 3000 dollar'	),
                         selectInput("ts", "TechSupport : ",
                                     c("No" = 0,
                                       "No internet service" = 1,
                                       'Yes'=2)),
                         selectInput("os", "OnlineSecurity : ",
                                     c("No" = 0,
                                       "No internet service" = 1,
                                       'Yes'=2)),
                         selectInput("sc", "SeniorCitizen : ",
                                     c("Yes" = 1,
                                       "No" = 0)),
                         selectInput("c", "Contract : ",
                                     c("Month-to-month"=0 ,
                                       "One year"=1,
                                       "Two year" =2                  )) ,
                         actionButton("train", "Predict")
                        )
               )))))
                
                 
                 

backend <- function(input, output) {
  
 
  
  view_data(input,output)
  
  observeEvent(input$fillna, {
    fill_na(input,output)
  })
  observeEvent(input$encode, {
    encoding(input,output)
  })
  observeEvent(input$scaling, {
    scaling(input,output)
  })
  observeEvent(input$process, {
    processing(input,output)
  })
  #observeEvent(input$predict, {
  #  output$y_out<- renderText({y_out})
  #})
  renderTable({
    ddd = as.data.frame(colSums(is.na(df)))
    ddd$feat = colnames(df)
    return(ddd)
  })

  
  
  
  observeEvent(input$train, 
     { 
       input_tc=ifelse( input$tc=="",3000,input$tc)
       x<-(as.numeric(input_tc) - mean_tc)/std_tc
       y_sample<-data.frame('Dependents'=as.factor(input$dep),'PaperlessBilling'=as.factor(input$pb),'OnlineBackup'=as.factor(input$ob),'TotalCharges'=x,'TechSupport'=as.factor(input$ts),'OnlineSecurity'=as.factor(input$os),'SeniorCitizen'=as.integer(input$sc) , 'Contract'=as.factor(input$c))
       if(input$var == "lr"){
         
         cm<-log_reg(input,output)
         output$table <- renderTable({log_reg(input,output)})
         
         

       }
       else if(input$var == 'KNN'){
         cm<-knn(input,output)
         output$table <- renderTable({ knn(input,output)})
       }
       else if(input$var == "SVM"){
         cm<-svm(input,output)
         
         output$table <- renderTable({ svm(input,output)})
       }
       else if(input$var == 'nb'){
         cm<-nb(input,output)
         output$table <- renderTable({ nb(input,output)})
       }
       else if(input$var == 'dt'){
         cm<-dt(input,output)
         output$table <- renderTable({ dt(input,output)})
       }
       else if(input$var == 'rf'){
         cm<-rf(input,output)
         output$table <- renderTable({ rf(input,output)})
       }
       
       output$text1<-renderText({ paste(eval_metrics(cm)$acc) })
       output$text2<-renderText({ paste(eval_metrics(cm)$pre) } )
       output$text3<-renderText({ paste(eval_metrics(cm)$recall) } )
       output$text4<-renderText({ paste(eval_metrics(cm)$f1) } )
       
       output$y_out<- renderText({y_out})

    
      })
}


#############################################
load_data <- function(input,output)
{
  
  req(input$dataset)
  df=read.csv(input$dataset$datapath)
  df<-df[,c('Dependents', 'PaperlessBilling',  'OnlineBackup' , 'TotalCharges', 'TechSupport',   'OnlineSecurity','SeniorCitizen' , 'Contract','Churn')]
  return(df)
}

view_data <- function(input,output)
{
  output$df <- renderDataTable({
    df <- load_data(input,output)
    
    get_nan(input,output)
    return(df)
  },options = list(scrollX = TRUE))
}


get_nan <- function(input,output)
{
  df <- load_data(input,output)
  output$nan <- renderTable({
    ddd = as.data.frame(colSums(is.na(df)))
    ddd$feat = colnames(df)
    colnames(ddd)=c('NAN','feature')
    return(ddd)
  })
  
}



fill_na <- function(input,output)
{
  
  df <- load_data(input,output)
  df <-df[!(is.na(df$SeniorCitizen)),]
  #t_mode = mode(df$SeniorCitizen,na.rm = TRUE)
  #df[is.na(df[,"SeniorCitizen"]), "SeniorCitizen"] <- t_mode
  output$df <- renderDataTable({return(df)
  },options = list(scrollX = TRUE))
  
  output$nan <- renderTable({
    ddd = as.data.frame(colSums(is.na(df)))
    ddd$feat = colnames(df)
    colnames(ddd)=c('NAN','feature')
    return(ddd)
  })
 
  return(df)

}




encoding <- function(input,output)
{
  df = fill_na(input,output)
  
  df$PaperlessBilling = factor(df$PaperlessBilling,levels = c("No" ,"Yes"),labels = c(0,1))
  df$Contract = factor(df$Contract,levels = c("Month-to-month" , "One year","Two year"),labels = c(0,1,2))
  df$TechSupport = factor(df$TechSupport,levels = c("No","No internet service","Yes"),labels = c(0,1,2))
  df$OnlineBackup = factor(df$OnlineBackup,levels = c("No","No internet service","Yes"),labels = c(0,1,2))
  df$OnlineSecurity = factor(df$OnlineSecurity,levels = c("No","No internet service","Yes"),labels = c(0,1,2))
  df$Dependents  = factor(df$Dependents ,levels = c("No" ,"Yes"),labels = c(0,1))
  df$TotalCharges  = as.numeric(df$TotalCharges)
  mean_tc<-mean((df$TotalCharges))
  std_tc  = sd(df$TotalCharges)
  
  output$df <- renderDataTable({
    
    return(df)
  },options = list(scrollX = TRUE))
  return(df)
}
scaling <- function(input,output)
{
  df = encoding(input,output)
  df$TotalCharges=as.numeric(scale(df$TotalCharges))
  output$df <- renderDataTable({
    
    return(df)
  },options = list(scrollX = TRUE))
  return(df)
}





preprocess <-function(input,output)
{

  
  df<-scaling(input,output)
  
  
  library(caTools)
  set.seed(123)
  split = sample.split(df$Churn,SplitRatio = 0.2)
  train    = subset(df,split==FALSE)
  test     = subset(df,split==TRUE)
  x_train  = train[,1:length(colnames(train))-1] 
  x_test   = test[,1:length(colnames(test))-1] 
  y_train  = train[,"Churn"]
  y_test   = test[,"Churn"]
  
  return (list('x_train'=x_train,'x_test'=x_test ,'y_train'=y_train ,'y_test'=y_test ,'train'=train ))
}


log_reg <- function(input,output)
  
{ 
  x_y= preprocess(input,output)
  x_train<-x_y$x_train
  x_test<- x_y$x_test
  y_test<-x_y$y_test
  y_train<-x_y$y_train
  train<-x_y$train
  ###logistic regression 
  
  # Fitting Logistic Regression to the Training set
  lr = glm(formula = Churn ~ .,
           family = binomial,
           data = train)
  
  # Predicting the Test set results
  prob_pred = predict(lr, type = 'response', newdata = x_test)
  y_pred = ifelse(prob_pred > 0.5, 'Yes', 'No')
  
  prob_pred_y_sample = predict(lr, type = 'response', newdata = y_sample)
  y_out = ifelse(prob_pred_y_sample > 0.5, 'Yes', 'No')
  assign('y_out',y_out,envir=globalenv())
  
  cm_lr = as.matrix(table(Actual = y_test, Predicted = y_pred))
  return (cm_lr)
}

#assign('data',data,envir=globalenv())


knn<- function(input,output)
  
{ 
  x_y= preprocess(input,output)
  x_train<-x_y$x_train
  x_test<- x_y$x_test
  y_test<-x_y$y_test
  y_train<-x_y$y_train
  train<-x_y$train
  #using KNN 
  
  # Fitting K-NN to the Training set and Predicting the Test set results
  library(class)
  y_pred = class:::knn(train = x_train,
               test = x_test,
               cl = y_train,
               k = 5,
               prob = TRUE)
  
  # Making the Confusion Matrix

  cm_knn = as.matrix(table(Actual = y_test, Predicted = y_pred))
  
  y_out = class:::knn(train = x_train,
                      test = y_sample,
                      cl = y_train,
                      k = 5,
                      prob = TRUE)
  y_out=ifelse(y_out=='Yes','Yes','No')
  assign('y_out',y_out,envir=globalenv())
  return (cm_knn)
  
  
}

svm<- function(input,output)
  
{ 
  x_y= preprocess(input,output)
  x_train<-x_y$x_train
  x_test<- x_y$x_test
  y_test<-x_y$y_test
  y_train<-x_y$y_train
  train<-x_y$train
  ###using kernel SVM
  
  
  # Fitting Kernel SVM to the Training set
  
  # install.packages('e1071')
  library(e1071)
  svm = e1071::svm(formula = Churn ~ .,
            data = train,
            type = 'C-classification',
            kernel = 'radial')
  
  # Predicting the Test set results
  y_pred = predict(svm, newdata = x_test)
  levels(y_sample$Dependents)<-levels(x_train$Dependents)
  levels(y_sample$PaperlessBilling)<-levels(x_train$PaperlessBilling)
  levels(y_sample$OnlineBackup)<-levels(x_train$OnlineBackup)
  levels(y_sample$TechSupport)<-levels(x_train$TechSupport)
  levels(y_sample$OnlineSecurity)<-levels(x_train$OnlineSecurity)
  levels(y_sample$Contract)<-levels(x_train$Contract)
  
  y_out= predict(svm, newdata = y_sample)
  y_out=ifelse(y_out=='Yes','Yes','No')
  assign('y_out',y_out,envir=globalenv())
  # Making the Confusion Matrix
  cm_svm = as.matrix(table(Actual = y_test, Predicted = y_pred))
  return (cm_svm)
  
  
}

nb<- function(input,output)
  
{ 
  x_y= preprocess(input,output)
  x_train<-x_y$x_train
  x_test<- x_y$x_test
  y_test<-x_y$y_test
  y_train<-x_y$y_train
  train<-x_y$train
  ###using naive bayes
  
  
  # install.packages('e1071')
  library(e1071)
  nb = naiveBayes(x = x_train,
                  y = y_train)
  
  # Predicting the Test set results
  y_pred = predict(nb, newdata = x_test)
  y_out=predict(nb, newdata = y_sample)
  y_out=ifelse(y_out=='Yes','Yes','No')
  assign('y_out',y_out,envir=globalenv())
  # Making the Confusion Matrix
  cm_nb = as.matrix(table(Actual = y_test, Predicted = y_pred))
  return (cm_nb)
  
}

dt<- function(input,output)
  
{ 
  x_y= preprocess(input,output)
  x_train<-x_y$x_train
  x_test<- x_y$x_test
  y_test<-x_y$y_test
  y_train<-x_y$y_train
  train<-x_y$train
  ##using descision trees 

  dt = rpart::rpart(formula = Churn ~ .,
             data = train)
  
  # Predicting the Test set results
  #the output from the prediction here is 2 columns where beneath them each of the probabilites of the 2 classes
  #by adding type='class' , it outputs 1 and zeros again
  y_pred = predict(dt, newdata = x_test, type = 'class')
  y_out = predict(dt, newdata = x_test, type = 'class')
  # Making the Confusion Matrix
  cm_dt = as.matrix(table(Actual = y_test, Predicted = y_pred))
  return (cm_dt)
  
  
}

rf<- function(input,output)
  
{ 
  x_y= preprocess(input,output)
  x_train<-x_y$x_train
  x_test<- x_y$x_test
  y_test<-x_y$y_test
  y_train<-x_y$y_train
  train<-x_y$train
  
  library(randomForest)
  set.seed(123)
  rf= randomForest::randomForest(x = x_train,
                   y =y_train ,
                   ntree = 500)
  
  # Predicting the Test set results
  y_pred = predict(rf, newdata = x_test)
  levels(y_sample$Dependents)<-levels(x_train$Dependents)
  levels(y_sample$PaperlessBilling)<-levels(x_train$PaperlessBilling)
  levels(y_sample$OnlineBackup)<-levels(x_train$OnlineBackup)
  levels(y_sample$TechSupport)<-levels(x_train$TechSupport)
  levels(y_sample$OnlineSecurity)<-levels(x_train$OnlineSecurity)
  levels(y_sample$Contract)<-levels(x_train$Contract)
  y_out= predict(rf, newdata = y_sample)
  # Making the Confusion Matrix
  y_out=ifelse(y_out=='Yes','Yes','No')
  assign('y_out',y_out,envir=globalenv())
  cm_rf = as.matrix(table(Actual = y_test, Predicted = y_pred))
  return (cm_rf)
}

eval_metrics<-function(cm)
{ 
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  accuracy = (sum(diag) / n )*100
  precision = mean((diag / colsums ))*100
  recall = mean((diag / rowsums ))*100
  f1 = mean((2 * precision * recall / (precision + recall) ))
  return(list('acc'=accuracy,'pre'=precision,'recall'= recall,'f1'=f1))
  
 
}


shinyApp(ui, backend)