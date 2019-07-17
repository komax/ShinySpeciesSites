library(tidyverse)

columnName <- function(df, column.index) {
    columns = names(df)
    column.name = columns[column.index]
    # Sanity check and return empty string when not found.
    if (length(column.name) == 0 || is.na(column.name)) {
        return("")
    } else {
        return(column.name)
    }
}

filterCounts <- function(data, column_name) {
    data %>%
        filter(.data[[column_name]] > 0)
}

toSpeciesSiteMatrix <- function(longFormatData, speciesCol, sitesCol, abundancesCol, filterNonAbundantSpecies = FALSE, colNameAbundancePerSpecies = "Count") {
    # Ensure that the columns are characters.
    stopifnot(is.character(speciesCol),
              is.character(sitesCol),
              is.character(abundancesCol))
    
    
    # Filter non abundant species.
    inputData <- longFormatData
    if (filterNonAbundantSpecies) {
        inputData <- longFormatData %>%
            filterCounts(abundancesCol)
    }
    # Transform long format into species x site matrix
    outputData <-
        inputData %>%
        group_by(.data[[sitesCol]], .data[[speciesCol]]) %>%
        # Introduce a new column with the abundance per species.
        summarise(colNameAbundancePerSpecies = sum(.data[[abundancesCol]])) %>%
        spread(key = speciesCol, value = colNameAbundancePerSpecies, fill = 0)
    return(outputData)
}

identifyColumn <- function(data, names) {
    column_names <- names(data)
    for (i in seq_along(data)) {
        column_name <- column_names[i]
        if (tolower(column_name) %in% names) {
            return(list(
                index = i,
                columnName = column_name
            ))
        }
    }
    # Otherwise return an empty list.
    list()
}

identifySpeciesColumn <- function(data, names = c("species")) {
    return(identifyColumn(data, names))
}

identifySiteColumn <- function(data, names = c("site", "sites", "sampling site")) {
    identifyColumn(data, names)
}

identifyAbundancesColumn <- function(data, names = c("abundance", "abundances", "individuals", "individual", "count", "frequency")) {
    identifyColumn(data, names)
}


if(FALSE) {
    data <- read.csv("data/Choi_2018_long_format.csv")
    filtered_data <- filterCounts(data, column_name = "Individuals")
    res <- identifySpeciesColumn(data)
    print(res[1])
    length(res)
    print(identifySiteColumn(data))
    print(identifyAbundancesColumn(data))
    print(columnName(data, 0))
    print(columnName(data, 2))
    print(columnName(data, 39750))
    speciesSiteMatrix <- toSpeciesSiteMatrix(data, speciesCol = "Species", sitesCol = "Site", abundancesCol = "Individuals", filterNonAbundantSpecies = FALSE)
}