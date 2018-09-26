
#' This app reads multiple excel documents, labeled in YYYY.MM structure 
#' orders them, reads them all, and creates an index called Time. it then imputes
#' any NA values to the previous value in the PFE columns specifically. The combined, sorted, imputed spreadsheet is then downloadable. 
library(shiny)
library(readxl)

# Define server logic required to 
shinyServer(function(input, output, session) {
   
  # This only prints running inside the R console, effectively does nothing in a standalone app, but here for testing. 
  observeEvent(
  eventExpr = input[["submit"]],
  handlerExpr = {print("Running")}
    )
  
  # the use of event reactive is to prevent anything running upon opening. 
  df <- eventReactive(input$submit, {
    
    #only run the program if they confirm they've checked the YYYY.MM structure 
    
    if(input$confirm == TRUE){
    
    # get all of the data into a dataframe of:   name | size | type | datapath 
    inFile <- input$the.files
    inFile <- inFile[order(inFile$name),]  # sorts it by the name, i.e.  2018.04, 2018.11, 2019.03  
    
    spreadsheet <- NULL  # initialize empty holder of the data
    j <- 1               # this is the counter for our index, i.e. Time 
    print("working")
    for(i in inFile$datapath){  # go to each datapath and add an index column before combining 
    temp.xl <- read_xlsx(i, 
                         sheet = 1,
                         col_types = "text") # all the columns will be text for now. 
    temp.xl$Time <- as.integer(j) # add the index 
    spreadsheet <- rbind(spreadsheet, temp.xl) # combine the data into a single object. 
      j <- j+1        # Time increases by 1 for each file. 
                             }  
    
    print("data loaded")
   # future version of app may allow selection of these in the UI
    columns.to.impute <- c("pfe1","pfe2","pfe3","pfe4","pfe5","total_pfe") # note it will also impute IN THIS ORDER
    the.idcols <- c("hospital_name","ahaid")
    
    
      suppressWarnings(  # supressing the tibble row names warning 
    # impute for each column to impute
    for(i in columns.to.impute){ 
      spreadsheet <- imputer(thedata = spreadsheet,  # may have warning about rownames being deprecated in tibbles. 
                            idcols = the.idcols,    # set above
                            indexer = "Time",       # set at the temp.xl[[indexer]] level.  Currently, temp.xl$Time
                            imputecol = i)          # looping here 
                      })
    print("data imputed")
          } #this is the wrap for if TRUE 
    
    # Else need to re-do. 
    else print("you didn't verify the files have YYYY.MM structure. Please checkmark that box.")
    
    print("done")
    spreadsheet
    })   # this is the wrap for eventReactive
  
  output$the.spreadsheet <- renderTable({
   head(df())
  })
  
  output$get.data <- downloadHandler(
    filename = function() { 
      paste("FinalData",".csv",sep="")
              },
    content = function(file) { 
      write.csv(df(),file,row.names = FALSE)
              }
      )
      
    
  })





######################### Here is the key function: The imputer function 

imputer <- function(thedata, idcols, indexer, imputecol){ 
  # this function takes data  (thedata)
  # creates a new column as a concatenated name of the idcols [for sorting]
  # identifies NAs in the impute column  (imputecol) 
  # and then replaces the NA with the value 
  # found previously (by climbing up the indexer column - which should be numeric) 
  
  index.error.message <- "Please convert your index to integer, for example,
  time as months since a start date (e.g 1,2,3...)"
  if(class(thedata[[indexer]]) != "integer") return(index.error.message)
  # I wrote this in case I needed the original, I didn't, but leaving anyway. I realize this forces a full copying which is slow. 
  .data <- thedata
  
  # get the column numbers of the idcols for concatenation with paste    
  ## // actually this step is not needed, idcols can be passed through apply, but leaving for future reference.
  .thecols <- which(             #converts the TRUE / FALSE vector into positions of TRUE in that vector 
    names(.data) %in% idcols    # column names that are in the idcols (in form: FALSE TRUE FALSE TRUE TRUE FALSE...)
  )
  
  
  # using do.call allows for excel-style concatenation of columns 
  .data$FULLID <- do.call(paste,
                          c(.data[.thecols], sep = "-")   # do.call requires all arguments of the called function to be in a list 
  ) # creates a unique ID from the idcols separated by - between each column
  # equivalent long form would be of the form paste(col1,col2,col3..., sep = "-") 
  
  
  ## alternative equivalent method 
  #' .data$FULLID <- apply(.data[idcols], 1, paste, collapse = "-")
  #' 
  
  # order the data by this ID column   -- Note: position and row # are made different here. I want them to be the SAME 
  .data <- .data[order(.data$FULLID),]
  
  
  #' this may seem strange, but when we search for Time using the position of the NAs, it's going to search by rowname**
  #' which is ORDERED by something DIFFERENT than row number. We want to completely throw away the original row ordering 
  #' because it's not relevant. 
  #' 
  #' ** Technically, it will do a position search when searching by a class numeric, but 
  #' that is the kind of thing that causes a big error later. 
  
  ### this is deprecated for tibbles, which is fine, since there is already an integer check on the indexer.  
  rownames(.data) <- 1:nrow(.data)
  
  # creates a matrix  of row & cols location NAs in the data 
  .nalocals <- which(is.na(.data), 
                     arr.ind = TRUE)
  
  # convert .nalocals to a data frame so that it has named columns. 
  .nalocals <- as.data.frame(.nalocals)
  
  
  #' the structure of this dataframe is as follows:    columns:     row  col 
  #'                                             the values:         a    b               
  #'                                             where a  = row number of an NA in the full dataset 
  #'                                                   b  = column number of that NA  
  
  #' now: we'll append the indexer of the NA to this table 
  #' and we will go up the rows until we find a non-NA (max movement up would be = indexer - 1) or
  #' we run out of values of the relevant id and just leave it and move on. This will likely only happen if data
  #' starts with an NA. Which was unlikely in the dataset I am in.   
  #' this movement up works because the data table is ordered by the fullid column. 
  
  
  .nalocals$index <- .data[[indexer]][.nalocals$row]  # cannot use $ operator to get the indexer column due to environments.  
  
  #' for each na row go up a until !na then assign. 
  #' because we are accessing a column based on an argument, for data based on an argument. we use [[]] 
  for(i in .nalocals$row){ 
    temp.index <- .data[[indexer]][i]
    if(temp.index > 1){
      .data[[imputecol]][i] <- .data[[imputecol]][i-1]
    }
    else next 
  }
  return(.data)
}

