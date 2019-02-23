library('shiny')
library('caret')
library('caTools')
library('rminer')



shinyServer(function(input,output) ({

  data <- reactive({
    file1 <- input$dataset
    if(is.null(file1)){return()}
    read.csv(file= file1$datapath, header = TRUE, stringsAsFactors = FALSE)
  })  
  
  output$selattrib <- renderUI({
    att <- names(data())
    checkboxGroupInput("attrib","select the attributes",choices = att[1:length(att)-1],selected = att[1:length(att)-1])
    
    })
  
  
  myattri <- reactive({
    if(is.null(data())){return("please select the data set !!")}
    set.seed(130)
    diabetes <- data()
    diabetes[,"Outcome"] <- as.factor(diabetes[,"Outcome"])
    diabetes <- diabetes[,c(input$attrib,"Outcome")]
  
    s=0.8
    if(input$split==1){
      s=0.7
    }else if(input$split==2){
      s=0.75
    }else if(input$split==3){
      s=0.8
    }
    inter <- sample.split(Y=diabetes$Outcome, SplitRatio=s)
    trainData <- diabetes[inter, ] 
    testData <- diabetes[!inter, ]
    
    trctrl <- trainControl(method = "repeatedcv", number = 6, repeats = 3)
    
    grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
    
    #mtrygrid <- expand.grid(mtry=3)
    svm_Linear <- train(Outcome~., data = testData, method = "svmLinear",
                        trControl=trctrl,
                        preProcess = c("center", "scale"),
                        tuneGrid=grid,
                        tuneLength = 15
    )
    
    test_pred <- predict(svm_Linear, newdata = testData)
    
    t <- table(predictions=test_pred, actual=testData$Outcome)
    #output$details=renderTable(t)
    
    
    output$sensitivity = renderText(paste("Sensitivity(in %)",(t[2,2]/(t[2,2]+t[1,2])*100)))
    
    output$specificity = renderText(paste("Specificity(in %)",(t[2,1]/(t[1,1]+t[2,1])*100)))
    
    output$accuracy = renderText(paste("accuracy(in %)",sum(diag(t))/sum(t)*100))
    
    M <- fit(Outcome~., data=trainData, model="svm", kpar=list(sigma=0.10), C=0.25)
    
    svm.imp <- Importance(M, data=trainData)
    svm.imp$imp
    
    slices <- svm.imp$imp[0:8]
    lbls <- c("Pregnancies",	"Glucose",	"BloodPressure",	"SkinThickness"	,"Insulin",	"BMI",	"DiabetesPedigreeFunction","Age")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels
    lbls <- paste(lbls,"%",sep="") # ad % to labels
    output$impGraph = renderPlot(pie(slices,labels = lbls, col=rainbow(length(lbls)),
        main="Pie Chart of Importance"))
    
    
    t
    
    
   
    
  })
  
  
  output$test <- renderTable({
    myattri()
  })
  
  output$dat <- renderDataTable({
    data()
  })
  output$struct <- renderPrint({
    str(data())
  })
  
  
  output$summ<- renderPrint({
    summary(data())
  })
  

  
  
  output$compGraph <- renderPlot({
    barplot(height = c(74.76,74.70,75.67,73.65,76.83,81.81,74.48), 
            names.arg = c("Random forest","Naive Baysian","lda","CART","glm","SVM","KNN"), 
            col="yellow", 
            border=TRUE,
            xlab="MODEL NAMES",
            ylab="ACCURACY IN PERCENTAGE",
            ylim = c(0,100),
            main="ACCURACY COMPARISION OF MODELS",
            beside = TRUE)
  })
  

  output$splGraph <- renderPlot({
    plot(x=c(60,65,70,75,80), y=c(79.8,79.1,79.5,80.7,83.1),
         type="o",
         xlab="SPLIT RATIO IN PERCENTAGE",
         ylab="ACCURACY IN PERCENTAGE",
         main="SPLIT RATIO V/S ACCURACY",
         col="brown",
         lwd=3)
  })
  
  
  
  
  
})
            
)