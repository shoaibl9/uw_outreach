#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(dplyr)
options(shiny.maxRequestSize = 30*1024^2)

# Define UI for application
ui <- fluidPage(
    titlePanel("Tool for Outreach"),
    
    bsCollapse(
        bsCollapsePanel("How to use",
                        HTML("Step 1) Log into BI Portal and go to Search_By_Curriculum <br />
                        Step 2) Download relevant student data
                        <ul>
                        <li>Filter courses by year (Su/Au/Wi/Sp of a given academic year) and only search for non-graduated students (exclude C/o 2021 if course taken in 2020-2021 or prior, etc.)</li>
                        <li>Filter courses by department to avoid overlapping course numbers</li>
                        </ul>
                        Step 2) Upload all csv files simultaneously to input below <br />
                        Step 3) Download output if desired"
                        )),
        bsCollapsePanel("Data prerequisites",
                        HTML("-Each file must be in CSV format <br />
                        -Each file must include at least Email column <br />
                        -All files must have identical formatting (same columns listed)")),
        bsCollapsePanel("Output",
                        HTML("-The first table shows you the number of students (n) that have taken (counts$n) courses in the data provided <br />
                        -The second table shows the list of emails to reach out to along with the number of courses taken by that student")),
        bsCollapsePanel("Please note",
                        HTML("-Larger data will take longer to display (30 MB max) <br />
                        -The arrows to change the minimum number of courses are a bit wonky, just use up/down keys or type in the number <br />
                        -Problems? Email me (Shoaib Laghari) at msl09@uw.edu"))
    ),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file1",
                      "Choose CSV files from directory",
                      multiple = TRUE,
                      accept=c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv')),
            numericInput("minCount",
                         "Minimum number of courses taken?",
                         0, # default
                         0, # min
                         100, # max
                         1), # step
            downloadButton('downloadData', 'Download')
        ),
        mainPanel(
            tableOutput('insights'),
            tableOutput('contents')
        )
    )
)

# Define server logic
server <-  function(input, output) {
    
    getContents <- reactive({
        inFile <- input$file1
        if (is.null(inFile)){
            return(NULL)
        } else {
            # browser()
            numfiles = nrow(inFile) 
            master = list()
            
            for (i in 1:numfiles)
            {
                
                JSON_csv = read.csv(input$file1[[i, 'datapath']], header = TRUE)
                lastrow = nrow(JSON_csv)
                #shift = function(x, n){
                #    c(x[-(seq(n))], rep("Yes", n))
                #}
                #JSON_csv$Processed = shift(JSON_csv$Processed, 1)
                master[[i]] = JSON_csv[-(lastrow+1), ]
                
            }
            # combining
            combined <- do.call(rbind, master)
            
            # filtering
            filtered.df <- select(combined, Email)
            filtered.df <- filter(filtered.df, !(Email == ""))
            
            # display
            counts <- count(filtered.df, filtered.df$Email, sort = TRUE)
            counts = counts[-1,] # excludes empty email count
            
            # prioritize
            priority <- filter(counts, n >= input$minCount)
        }
    })
    
    getInsights <- reactive({
        inFile <- input$file1
        if (is.null(inFile)){
            return(NULL)
        } else {
            # browser()
            numfiles = nrow(inFile) 
            master = list()
            
            for (i in 1:numfiles)
            {
                
                JSON_csv = read.csv(input$file1[[i, 'datapath']], header = TRUE)
                lastrow = nrow(JSON_csv)
                #shift = function(x, n){
                #    c(x[-(seq(n))], rep("Yes", n))
                #}
                #JSON_csv$Processed = shift(JSON_csv$Processed, 1)
                master[[i]] = JSON_csv[-(lastrow+1), ]
                
            }
            # combining
            combined <- do.call(rbind, master)
            
            # filtering
            filtered.df <- select(combined, Email)
            filtered.df <- filter(filtered.df, !(Email == ""))
            
            # insights
            counts <- count(filtered.df, filtered.df$Email, sort = TRUE)
            counts = counts[-1,] # excludes empty email count
            
            countsOfCounts <- count(counts, counts$n)
        }
    })
    
    output$contents <- renderTable( 
        getContents() 
    )
    
    output$insights <- renderTable( 
        getInsights() 
    )
    
    output$downloadData <- downloadHandler(
        filename = function() { 
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) { 
            write.csv(getContents(), file, row.names=FALSE)   
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
