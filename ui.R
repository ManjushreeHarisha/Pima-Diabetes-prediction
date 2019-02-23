library(shiny)
library(shinyjs)
shinyUI(fluidPage(
  headerPanel("Diabetes prediction"),
  
  sidebarPanel(
    conditionalPanel(condition="input.tabselected==1",h4("diabetes prediction"), tags$img(src="diabetes.jpg",width=380, height=400)),
    conditionalPanel(condition="input.tabselected==2", 
                     fileInput("dataset", "browse the dataset"),
    radioButtons("choice","choose an option",choices=c("Dataset"=1, "Structure"=2, "Summary"=3)),
    tags$img(src="search-man1.jpg",width=300)
    ),
    
    conditionalPanel(condition="input.tabselected==3", h4("comparision of models"),
                     helpText("This bar graph shows the accuracy of different models in determing the onset of diabetes mellitus")),
    
    
    conditionalPanel(condition="input.tabselected==4", 
                     h4("prediction"),
                     radioButtons("split","choose the split ratio",choices=c("70%"=1, "75%"=2, "80%"=3)),
                     uiOutput("selattrib"),
                     helpText("ACCURACY = (TP+TN)/(TP+TN+FP+FN)"), 
                     helpText("SENSITIVITY(TP RATE) = TP/(TP+FN)"), 
                     helpText("SPECIFICITY(FP Rate) = FP/(FP+TN)"),
                     helpText("Sensitivity and Specificity are statistical measures that describe how well the classifier discriminates between a case with positive and with negative class. Sensitivity is the detection of disease rate that needs to be maximized and Specificity is the false alarm rate that is to be minimized for accurate diagnosis.")),
    
    
    conditionalPanel(condition="input.tabselected==5",h4("IMPORTANCE OF ATTRIBUTES"), helpText("This pie chart shows the importance of each attribute in determining the onset of diabetes mellitus")),
    conditionalPanel(condition="input.tabselected==6",h4("SPLIT RATIO V/S ACCURACY"), helpText("This plot shows the variation of accuracy with respect to split raio"))

                     
    
    
  ),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("ABOUT", value=1, tags$div(HTML(paste(tags$span(style="font-size:150%;", "This application has different sections to browse the dataset and to compare
                                                               the accuracy of different algorithms. The pictorial representation which gives information about
                                                               accuracy helps the user to understand and to use specific algorithm that suits for the given
                                                               dataset. It also gives information about the importance of each attribute in determining the
                                                               accurate output."),sep="")))),
      
      tabPanel("DATA", value=2, conditionalPanel(condition = "input.choice==1", dataTableOutput("dat")),
               conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
               conditionalPanel(condition="input.choice==3", verbatimTextOutput("summ"))
               ),
      tabPanel("COMPARISION", value=3 ,plotOutput("compGraph")),
      tabPanel("PERFORMANCE", value=4 , tableOutput("test"),verbatimTextOutput("accuracy"), verbatimTextOutput("sensitivity"), verbatimTextOutput("specificity")),
      tabPanel("IMPORTANT ATTRIBUTES", value=5 , plotOutput("impGraph")),
      tabPanel("SPLIT RATIO V/S ACCURACY", value=6, plotOutput("splGraph")),
      id= "tabselected"
      
      
    )
    
  )
  
))