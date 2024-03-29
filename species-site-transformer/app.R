#
# This is a Shiny web application to transform species x site matrices. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("util.R")

# Define UI for application that transforms a species-site matrix stored as a CSV.
ui <- fluidPage(

    # Application title
    titlePanel("Transforming species x site matrices"),

    # Sidebar with control widgets.
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

            h4("Specify which column corresponds to species"),
            splitLayout(cellWidths = c("25%", "75%"),
                        numericInput("speciesColumn", "Column", value = 0, min = 0, max = 100),
                        h3(textOutput("speciesColName"))
            ),
            actionButton("identifySpeciesColumn", label = "Detect column", icon = icon("search")),

            # Horizontal line ----
            tags$hr(),
            h4("Specify which column corresponds to sites"),

            splitLayout(cellWidths = c("25%", "75%"),
                        numericInput("sitesColumn", "Column", value = 0, min = 0, max = 100),
                        h3(textOutput(outputId = "sitesColName"))
            ),
            actionButton(inputId = "identifySitesColumn", label = "Detect column", icon = icon("search")),

            # Horizontal line ----
            tags$hr(),

            h4("Specify which column corresponds to abundances"),
            splitLayout(cellWidths = c("25%", "75%"),
                        numericInput("abundancesColumn", "Column", value = 0, min = 0, max = 100),
                        h3(textOutput(outputId = "abundancesColName"))
            ),
            actionButton(inputId = "identifyAbundancesColumn", label = "Detect column", icon = icon("search")),


            # Horizontal line ----
            tags$hr(),
            # Input: Checkbox if  has header ----
            checkboxInput("filterIndividuals", "Filter individuals", TRUE),
            actionButton(inputId = "showOutput", label = "Generate species-site matrix", icon = icon("table")),
            
            # Horizontal line ----
            tags$hr(),

            # Download Button
            downloadButton("downloadData", "Download")
        ),

        # Show the input table as well as the computed table.
        mainPanel(
            h3(textOutput(outputId = "headingInputCSV")),
            tableOutput("tableContents"),
            h3(textOutput(outputId = "headingTableShortFormat")),
            tableOutput("tableShortFormat")
        )
    )
)

# Define server logic required to transform the input file.
server <- function(input, output) {
    values <- reactiveValues()
    values$previousClickSpecies <- 0
    values$previousClickSites <- 0
    values$previousClickAbundances <- 0

    speciesSiteInput <- reactive({
        req(input$file)
        
        csvFilename <- input$file$datapath

        species.site.matrix <- read.csv(csvFilename,
                                        header = input$header,
                                        sep = input$sep,
                                        quote = input$quote)
        

        if(input$disp == "head") {
            numRows <- 12
            return(head(species.site.matrix, n = numRows))
        } else {
            return(species.site.matrix)
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
            } else {
                return("")
            }
        }
        # Check if the column selector has been used.
        if (input$speciesColumn) {
            return(columnName(speciesSiteInput(), input$speciesColumn))
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
            } else {
                return("")
            }
        }
        
        # Check if the column selector has been used.
        if (input$sitesColumn) {
            return(columnName(speciesSiteInput(), input$sitesColumn))
        }
    })
    
    abundancesColumn <- eventReactive(input$abundancesColumn | input$identifyAbundancesColumn, {
        # Check if this item has been clicked
        if (values$previousClickAbundances + 1 == input$identifyAbundancesColumn[1]) {
            # Bookkeeping in this reactive value of how many clicks has been carried out.
            values$previousClickAbundances <- values$previousClickAbundances + 1
            computedColumn <- identifyAbundancesColumn(speciesSiteInput())
            if (length(computedColumn)) {
                # Return the column name if it can be computed.
                return(computedColumn$columnName)
            } else {
                return("")
            }
        }
        
        # Check if the column selector has been used.
        if (input$abundancesColumn) {
            return(columnName(speciesSiteInput(), input$abundancesColumn))
        }
    })
    
    output$headingInputCSV <- renderText({
        req(input$file)
        paste("Input:", input$file$name)
    })

    
    output$tableContents <- renderTable({
        speciesSiteInput()
    })
    
    output$headingTableShortFormat <- renderText({
        # If we have generated an output
        if (is.data.frame(speciesSiteOutput())) {
            # then prepend a heading.
            return("Output as species x site matrix:")
        } else {
            return("No output possible")
        }
    })
    
    output$tableShortFormat <- renderTable({
        speciesSiteOutput()
    })
    
    output$speciesColName <- renderText({
        speciesColumn()
    })

    output$sitesColName <- renderText({
        sitesColumn()
    })
    
    output$abundancesColName <- renderText({
        abundancesColumn()
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            input$file$name
        }, 
        content = function(file) {
            outputData <- speciesSiteOutput()
            # FIXME Validate whether we obtained a matrix or not.
            write.csv(outputData, file, row.names = FALSE)
        }
    )
    
    speciesSiteOutput <- eventReactive(input$showOutput, {
        # Request values for these columns in the UI.
        sp.col <- speciesColumn()
        sites.col <- sitesColumn()
        abund.col <- abundancesColumn()
        
        # Validate the the column names.
        # If they don't provide correct values, output error message in the main panel.
        validate(
            need(!is.null(sp.col) && nchar(sp.col) > 0, "A column needs to be specified for species"),
            need(!is.null(sites.col) && nchar(sites.col) > 0, "A column needs to be specified for sites"),
            need(!is.null(abund.col) && nchar(abund.col) > 0, "A column needs to be specified for abundances")
        )
        
        
        # Request the input.
        input.data <- speciesSiteInput()
        # cat(speciesColumn(), sitesColumn(), abundancesColumn())
        
        toSpeciesSiteMatrix(input.data, speciesCol = sp.col,
                            sitesCol = sites.col, abundancesCol = abund.col, 
                            filterNonAbundantSpecies = input$filterIndividuals)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
