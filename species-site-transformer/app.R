#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("util.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Transforming species x site matrices"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Upload CSV:
            fileInput("file", "Upload CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv",
                                 ".CSV")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            # Horizontal line ----
            tags$hr(),
            
            selectInput("speciesSelector",
                        h3("Specify which column corresponds to species"),
                        choices = c("Species"),
                        selected = 1),
            
            numericInput("speciesColumn", label = NULL, value = 100),
            
            fluidRow(column(12, h3("Specify which column corresponds to species"))),
            
            fluidRow(
                    splitLayout(
                        textInput("speciesColumn", "Column"),
                        textInput("speciesColName", "Name")
                    )
            ),
            
            fluidRow(column(12, h3("Specify which column corresponds to sites"))),
            
            fluidRow(
                splitLayout(
                    textInput("siteColumn", "Column"),
                    textInput("siteColName", "Name")
                )
            ),
            
            
            
            # Horizontal line ----
            tags$hr(),
            
            # Button
            downloadButton("downloadData", "Download")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("tableContents")
        )
    )
)

# Define server logic required to transform the input file.
server <- function(input, output) {
    speciesSiteInput <- reactive({
        req(input$file)
        
        species_site_matrix <- read.csv(input$file$datapath, 
                                        header = input$header,
                                        sep = input$sep,
                                        quote = input$quote)
                                 
        if(input$disp == "head") {
            return(head(species_site_matrix))
        } else {
            return(species_site_matrix)
        }})
    
    
    output$tableContents <- renderTable({
        speciesSiteInput()
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            input$file$name
        },
        content = function(file) {
            write.csv(speciesSiteInput(), file, row.names = FALSE)
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
