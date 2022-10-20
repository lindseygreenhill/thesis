library(tidyverse)

# reading in data sets

house_116 <- read_csv("house_116_tweets.csv")
load("handles_DWNOM_joined_116.RData")



# selecting rhe rows I want and filtering out retweets

house_116 <- house_116 %>%
  select(user_screen_name, user_name, text,
         retweet_id) %>%
  filter(is.na(retweet_id))

# making handle names lowercase

house_116_2 <- house_116 %>%
  mutate(handle = tolower(user_screen_name))

congress_116_joined_full_3 <- congress_116_joined_full_3 %>%
  mutate(handle = tolower(handle))

# joining the data sets using left join. I'm filtering to only include the
# official twitter accounts from my twitter accounts list

house_116_DWNOM_joined <- house_116_2 %>%
  left_join(congress_116_joined_full_3, by = "handle") %>%
  filter(handle %in% congress_116_joined_full_3$handle)


