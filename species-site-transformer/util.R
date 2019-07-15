library(tidyverse)


filterCounts <- function(data, column_name) {
    data %>%
        filter(.data[[column_name]] > 0)
}

identifyColumn <- function(data, names) {
    column_names <- names(data)
    for (i in seq_along(data)) {
        column_name <- column_names[i]
        if (tolower(column_name) %in% names) {
            print(tolower(column_name))
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


if(FALSE) {
    data <- read.csv("data/Choi_2018_long_format.csv")
    filtered_data <- filterCounts(data, column_name = "Individuals")
    res <- identifySpeciesColumn(data)
    print(res[1])
    length(res)
    print(identifySiteColumn(data))
}