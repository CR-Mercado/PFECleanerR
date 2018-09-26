

library(shiny)

# 
shinyUI(fluidPage(
  
  # 
  titlePanel("PFE Excel Cleaner"),
  
  # 
  sidebarLayout(
    sidebarPanel(
       h5("Insert a set of Excel documents (1 sheet per document - other sheets will not be read) with names in the form:
YYYY.MM.DD Month PFE DATA, the YYYY.MM convention is critical a that is how the data will be combined and ordered for imputation."),
      h4("Please be patient. It is a lot of data. Should be under 2 minutes total. You will get a data preview when done."),
      h5("You can verify the process was done correctly by examing the data for NAs. All NAs left over are due to: case_when(Time==1) == NA,
         that is, the first index value (i.e. Time 1) is NA, so future NAs don't have anything to impute."),
      h5("If you download as csv, the data will be sorted by a new column: FULLID"),
    # input the files    
    fileInput("the.files","Choose Excel docs", 
               multiple   = TRUE,
               accept     = c(".xls",".xlsx"),
               buttonLabel = "Browse"),

    # I'm adding this annoying checkbox to make sure users actually check their data makes sense. 
    checkboxInput("confirm", 
                  "Please confirm Excel docs are named beginning with YYYY.MM structure and that the first sheet is correct.",
                  value = FALSE),
    checkboxInput("removefinalNAs", 
                  "Remove hospitals that have never reported (i.e. started as NAs and never stopped being NA), recommended.",
                  value = TRUE),
    # the submit button 
    actionButton(inputId = "submit", label = "Run Program"),
    
    # download the cleaned spreadsheet. 
    downloadButton("get.data", label = "Download to csv")
           ),
    
    # 
    mainPanel(
      tableOutput("the.spreadsheet")
    )
  )
))
