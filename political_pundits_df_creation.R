library(skimr)
library(tidyverse)

# I am using data from the twitterverse paper and extracting the most followed
# political pundits from their analysis and their corresponding ideal points and
# other metrics. 

# reading in the classifications

elite_class <- read_csv("elite_classification.csv")

elite_ideologies <- read_csv("pundits_ideologies.csv")

elite_joined <- elite_class %>%
  left_join(elite_ideologies, by = c("handle" = "userhandle"))


# filtering to political pundits

pundits <- elite_joined %>%
  filter((sector1 == "political pundit") | (sector2 == "political pundit")
         | (sector3 == "political pundit"))
