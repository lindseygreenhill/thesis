library(rvest)
library(readxl)
library(janitor)
library(tidyverse)

# 117TH CONGRESS TWITTER HANDLES (2021-2022)

handles_117_senate <- read_excel("congress_twitter_handles_117.xlsx",
                                 sheet = 1,
                                 skip = 1) %>%
  clean_names() %>%
  mutate(handle = substring(link, 21)) %>%
  select(-link)



