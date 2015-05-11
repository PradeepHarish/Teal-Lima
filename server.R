library(shiny)

library(dataview)      #For Tree View

library(data.table)    #For Data Table Computations..duh
library(plotrix)       #For std.error Fx
library(psych)         #For Summary Stats
library(plyr)          #for cleaning up dataframe vector inequalities



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
  dataSorted <- reactive({SortDataIntoStrainFunction(listCSV(),input$cumuStrain)})
  
  
  print("[LOG] Preload complete: Check for errors")
  
  
  #------------------------------------------------------------------------------------------------------------------------------
  #                                                   DATA OUTPUTS  
  #------------------------------------------------------------------------------------------------------------------------------
  
  print("[LOG] Data Output Init")  
  
  output$desStatsRawMassGraphTable <- renderTable({   
    
    #Des Stats for Graph: WORKS!    
    #renaming to something manageable
    t <- dataSorted()     
    #so that nothing funny happens
    cumuStrain_i <-as.character(input$cumuStrain)
    #init for the loop
    dataProc <- list()
    
    for(i in 1:length(t))      
    {  
      #setting the name to the strain
      name <- as.character(t[[i]][[1]][cumuStrain_i][[1]][[1]]) #By jove, this is huge and needlessly complicated
      #calling Fx to display mean and SE weekly on a per strain basis
      y <- DesStatsForGraphFunction(t,i)
      #Appending answers to master list
      dataProc[[name]]<-list(y)      
    }  
    #converting to dataframe so shiny can read it and returning this to render table
    dataProc <- as.data.frame(dataProc)
    return(dataProc)
    
    
    
    
  })
  output$desStatsCumuMassGraphTable <- renderTable({   
    #Des Stats Cumu Mass for Graph: WORKS!    
    #renaming to something manageable
    x <- dataSorted()
    #converting to cumumass table instead of raw mass
    t <- ConvertRawToSelfNormFunction(x,input$normColID,input$initColID,input$finalColID,input$cumuStrain)    
    # so that nothing funny happens
    cumuStrain_i <-as.character(input$cumuStrain)
    #init for the loop
    dataProc <- list()
    
    for(i in 1:length(t))      
    { 
      #setting the name to the strain
      name <- as.character(t[[i]][[1]])
      #calling Fx to display mean and SE weekly on a per strain basis
      y<-DesStatsForGraphFunction(t,i,2)            
      #Appending answers to master list
      dataProc[[name]]<-list(y)      
    }  
    #converting to dataframe so shiny can read it and returning this to render table
    dataProc <- as.data.frame(dataProc)
    return(dataProc)  
  })
  
  print("[LOG] Data Output Complete: Check for errors")
})



##################################################################################################################################
#                                                         BACKEND FUNCTIONS
##################################################################################################################################

#------------------------------------------------------------------------------------------------------------------------------
#                                                   STATISTICS 
#------------------------------------------------------------------------------------------------------------------------------

DesStatsForGraphFunction <- function(dataInput,i,dataStructure = 1){
  
  if(dataStructure ==1){
    #Structure 1 for raw muscle mass  
    #slightly less ugly solution.            
    x <- (dataInput[[i]][[1]])
    Mean <- rapply(x,mean,classes="numeric")
    StdError <- rapply(x,std.error,classes="numeric")
    tabularOutput <- cbind(Mean,StdError)
    
    return(tabularOutput)
  }
  
  else if(dataStructure ==2){
    #Structure 2 for self normalised muscle mass 
    x <- (dataInput[[i]][[2]])    
    Mean <- lapply(x,mean)
    StdError <- lapply(x,std.error)
    tabularOutput <- cbind(Mean,StdError)
    
    return(tabularOutput)
  }
  
}

ConvertRawToSelfNormFunction <- function(t,normColID,initColID,finalColID,cumuStrain){
  
  #need these for the nested loops
  dataTempTable <- list()  
  dataTempTable2 <- list()
  
  #so that we are all on the same page
  normColID_i <- as.numeric(normColID)
  initColID_i <- as.numeric(initColID)
  finalColID_i <- as.numeric(finalColID)
  cumuStrain_i <-as.character(cumuStrain)
  
  #nested loops to reiterate over table columns 
  for(i in 1:length(t))      
  { 
    #Loop 1 goes accross each strain
    dataInput <- t[[i]][[1]]    
    #[cumuStrain_i][[1]] gives the strain column, the extra [[1]] is for just printing one element
    name <- as.character(dataInput[cumuStrain_i][[1]][[1]])     
    
    for(j in initColID_i:finalColID_i)
    {
      #Loop 2 goes into each tcolumn, from the strain table      
      #adjusting for index of dataframe to prevent nulls 
      k <- j-initColID_i+1      
      #Self normalisation to the value indicated by user and stich as child list to strain parent
      dataTempTable[[k]] <- (dataInput[j][[1]] - dataInput[normColID_i][[1]])    
    }    
    #final stiching of strain child lists to master parent list
    #Double instance of name to smooth data handling downstream
    dataTempTable2[[name]] <- list(name,dataTempTable)   
  }    
  return(as.list(dataTempTable2))
}
















#------------------------------------------------------------------------------------------------------------------------------
#                                                   DATA I/O  
#------------------------------------------------------------------------------------------------------------------------------



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
  Data <- within(Data, 
{  
  FinalID <-  factor(as.character(Data[[cumuStrain_i]]))
  AnimalID <- factor(as.character(Data[[animalID_i]]))    
})

#The below is not needed, as a reactive function in the main shiny function takes care of this.
#dataOutput <- SortDataIntoStrainFunction(Data) return(dataOutput) 
return(Data)
print("[LOG] File Proc Complete")
}



SortDataIntoStrainFunction <- function(y,cumuStrain){
  
  #init datainput as a list for loop. 
  dataInput <- list()
  #ensure nothing funny is going on here
  cumuStrain_i <- as.character(cumuStrain)
  
  for(i in levels(y[cumuStrain_i][[1]]))
  {
    #subset function to extract relevant values
    x <- subset(y, y[cumuStrain_i][[1]] == i)  
    #renaming for loop and assigning
    name <- paste("sub", i, sep = "")
    assign(name, x)
    #attaching child to prent
    dataInput[[name]]<-list(x)
  }  
  
  #return master list containing all other lists
  return(dataInput)
  
  #For Testing: Input of Strain not user driven
  #   for(i in levels(y$Strain))
  #   {
  #     #subset function to extract relevant values
  #     x <- subset(y, y$Strain == i)  
  #     #renaming for loop and assigning
  #     name <- paste("sub", i, sep = "")
  #     assign(name, x)
  #     #attaching child to prent
  #     dataInput[[name]]<-list(x)
  #   }  
  
}






