library(shiny)
library(knitr)


shinyUI(fluidPage(
  sidebarLayout(
      sidebarPanel(h1("Hi !"),
                   p("Before we get started Follow the steps on the navbar to the left, and you should be fine. "),
                   
                                    
                   br(),
                   h4(strong("Step 1:"),"File parameters"),
                   p("To ensure optimal parsing, please write the column names that you have used to identify values in your file"),
                   textInput("cumuStrain", label = ("Animal strain and treatment group"),value = "Strain"),  
                   textInput("animalID", label = ("ID of individual animal"),value = "MID"), 
                   
                   
                   br(),
                   h4(strong("Step 2:"),"File upload"),
                   p("Please ensure that all your files are csv formatted with a comma delimit"),
                   fileInput("fileInput", label = "Choose a csv formatted file to upload into the program"),
                   
                                      
                   h2("About..."),
                   p(" This was created by Pradeep Harish. He can be reached at pradeep@harish.rr.nu")),
  mainPanel( 
    textOutput("Log"),
    h4("Descriptive Statistics:"),
    textOutput("desStatsTable"),
    
    
    downloadButton("downloadPDF", "Download Analysis report as PDF"),
    downloadButton("downloadHTML", "Download Analysis report as HTML")
   
    
  )
  
)
)
)