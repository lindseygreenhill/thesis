library(rvest)
library(readxl)
library(janitor)
library(skimr)
library(tidyverse)

# 117TH CONGRESS TWITTER HANDLES (2021-2022)

handles_117_senate <- read_excel("congress_twitter_handles_117.xlsx",
                                 sheet = 1,
                                 skip = 1) %>%
  clean_names() %>%
  mutate(handle = substring(link, 21)) %>%
  select(-link)

handles_117_house <- read_excel("congress_twitter_handles_117.xlsx",
                                 sheet = 2,
                                 skip = 1) %>%
  clean_names() %>%
  mutate(handle = substring(link, 21)) %>%
  select(-link)

handles_117_congress <- handles_117_senate %>%
  rbind(handles_117_house)

save(handles_117_congress, file = "handles_117_congress.RData")

# 116TH CONGRESS TWITTER HANDLES (2021-2022)

# this data comes from github data set I found online

handles_116 <- read_csv("116Congress.csv") %>%
  clean_names() %>%
  select(name = wikipedia_names, handle = odu_wsdl)

save(handles_116, file = "handles_116_congress.RData")


## 115th CONGRESS TWITTER HANDLES

# getting data from the retweets study on the umich data base and then using the
# unique values of the names and user ids

handles_115 <- read_csv("retweets_congress_115th.csv")

handles_115_a <- handles_115 %>%
  select(user_screen_name, name, chamber, party) %>%
  unique()

save(handles_115_a, file = "handles_115_congress.RData")
