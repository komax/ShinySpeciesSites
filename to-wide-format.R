library(tidyverse)
Choi_2018 <- read.csv("data/Choi_2018_long_format.csv")

Choi_2018_matrix <-
    Choi_2018 %>%
        filter(Individuals > 0) %>%
        group_by(Site, Species) %>%
        summarise(Count = sum(Individuals)) %>%
        spread(key = "Species", value = "Count", fill = 0)

write.csv(Choi_2018_matrix, "data/Choi_2018_matrix.csv",row.names = F)
