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
            
            fluidRow(column(12, h3("Specify which column corresponds to species"))),
            
            fluidRow(
                    splitLayout(cellWidths = c("25%", "75%"),
                        numericInput("speciesColumn", "Column", value = 0, min = 0, max = 100),
                        h2(textOutput("speciesColName"))
                    )
            ),
            
            actionButton("identifySpeciesColumn", label = "Identify column"),
            
            # Horizontal line ----
            tags$hr(),
            
            fluidRow(column(12, h3("Specify which column corresponds to sites"))),
            
            fluidRow(
                splitLayout(cellWidths = c("25%", "75%"),
                    numericInput("sitesColumn", "Column", value = 0, min = 0, max = 100),
                    h2(textOutput(outputId = "sitesColName"))
                )
            ),
            
            actionButton(inputId = "identifySitesColumn", label = "Identify column"),
            
            
            
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
    values <- reactiveValues()
    values$previousClickSpecies <- 0
    values$previousClickSites <- 0
    
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
    
    speciesColumn <- eventReactive(input$speciesColumn | input$identifySpeciesColumn, {
        # Check if this item has been clicked
        if (values$previousClickSpecies + 1 == input$identifySpeciesColumn[1]) {
            # Bookkeeping in this reactive value of how many clicks has been carried out.
            values$previousClickSpecies <- values$previousClickSpecies + 1
            computedColumn <- identifySpeciesColumn(speciesSiteInput())
            if (length(computedColumn)) {
                # Return the column name if it can be computed.
                return(computedColumn$columnName)
            }
        }
        # Check if the column selector has been used.
        if (input$speciesColumn) {
            columns = names(speciesSiteInput())
            column_name <- columns[input$speciesColumn]
            return(column_name)
        }
    })
    
    sitesColumn <- eventReactive(input$sitesColumn | input$identifySitesColumn, {
        # Check if this item has been clicked
        if (values$previousClickSites + 1 == input$identifySitesColumn[1]) {
            # Bookkeeping in this reactive value of how many clicks has been carried out.
            values$previousClickSites <- values$previousClickSites + 1
            computedColumn <- identifySiteColumn(speciesSiteInput())
            if (length(computedColumn)) {
                # Return the column name if it can be computed.
                return(computedColumn$columnName)
            }
        }
        # Check if the column selector has been used.
        if (input$sitesColumn) {
            columns = names(speciesSiteInput())
            column_name <- columns[input$sitesColumn]
            return(column_name)
        }
    })
    
    
    output$tableContents <- renderTable({
        speciesSiteInput()
    })
    
    output$speciesColName <- renderText({
        result <- speciesColumn()
        # Sanity check
        if (length(result) == 0 || is.na(result)) {
            return("")
        } else { 
            return(result)
        }
    })
    
    output$sitesColName <- renderText({
        result <- sitesColumn()
        print(result)
        # Sanity check
        if (length(result) == 0 || is.na(result)) {
            return("")
        } else { 
            return(result)
        }
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
