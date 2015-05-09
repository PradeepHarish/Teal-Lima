library(shiny)

library(dataview) #For Tree View



##################################################################################################################################
#                                                         SHINY CODE
##################################################################################################################################

shinyServer(function(input, output) {
  
  
  
  ####Pre Load####
  print("[LOG] Preload Init")
  #we have to wrap this in a reactive expression to make this work, instead of recalling and processing the text file each and every time.
  
  #This gives us the string for the file location. You ideally need not use this.
  fileLocation <- reactive({FileLoadFunction(input)})
  
  #Getting and converting CSV from list to dataframe
  listCSV <- reactive({FileProcFunction(fileLocation(),input$animalID,input$cumuStrain)})
  
  #Use dataSorted() to point to your CSV file, processed by the FileProcFunction. The output is a superlist, containing child lists of 
  #the input dataset, split by strain
  dataSorted <- reactive({SortDataIntoStrainFunction(listCSV())})
  
  ####--------####
  print("[LOG] Preload complete: Check for errors")
  
  
  ####Data Outputs####
  print("[LOG] Data Output Init")
  
  
  output$desStatsTable <- renderText({ 
    
    print(tree.view(dataSorted()))
    
  })
  
  print("[LOG] Data Output Complete: Check for errors")
})



##################################################################################################################################
#                                                         BACKEND FUNCTIONS
##################################################################################################################################



FileLoadFunction <- function(inputArg){
  
  #HTML can't access local file system due to security reasons. Since we are running a local server, we
  #have to modify the string path to reflect that, as the input string contains initial garbage
  #apparently in ubuntu, the 4th argument is the tmp filepath... will this change in windows?
  
  print("[LOG] File Load Init")
  
  #Uncomment for Deployment
  #v=strsplit(toString(inputArg$fileInput),split=",",fixed=TRUE)
  #we have to perform a substr to trim initial whitespace  
  #fileInput <-  substr(v[[1]][4],start=2,stop=nchar(v[[1]][4]))
  
  #Uncomment for Test
  fileInput="/home/theblackalchemist/Documents/GCMassConsolidated.csv"
  
  
  return(fileInput)
  print("[LOG] File Load Complete")
  
}



FileProcFunction <- function(csvImport,animalID_i,cumuStrain_i){
  #CSV Parser 
  print("[LOG] File Proc Init")
  Data <- read.csv(csvImport)
  Data <- within(Data, {
    FinalID <-  factor(as.character(Data[[cumuStrain_i]])) 
    AnimalID <- factor(as.character(Data[[animalID_i]]))    
  })
  
  #The below is not needed, as a reactive function in the main shiny function takes care of this.
  #dataOutput <- SortDataIntoStrainFunction(Data)  
  dataOutput <- Data
  return(dataOutput)
  print("[LOG] File Proc Complete")
}



SortDataIntoStrainFunction <- function(y){
  
  #init datainput as a list for loop. 
  dataInput <- list()
  
  for(i in levels(y$Strain))
  {
    #subset function to extract relevant values
    x <- subset(y, y$Strain == i)  
    #renaming for loop and assigning
    name <- paste("sub", i, sep = "")
    assign(name, x)
    #attaching child to prent
    dataInput[[name]]<-list(x)
  }  
  return(dataInput)
}






