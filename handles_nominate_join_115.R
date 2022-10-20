library(stringr)
library(tidyverse)

## partial match algorithm

signature=function(x){
  sig=paste(sort(unlist(strsplit(tolower(x)," "))),collapse='')
  return(sig)
}
partialMatch=function(x,y,levDist=0.1){
  xx=data.frame(sig=sapply(x, signature),row.names=NULL)
  yy=data.frame(sig=sapply(y, signature),row.names=NULL)
  xx$raw=x
  yy$raw=y
  xx=subset(xx,subset=(sig!=''))
  xy=merge(xx,yy,by='sig',all=T)
  matched=subset(xy,subset=(!(is.na(raw.x)) & !(is.na(raw.y))))
  matched$pass="Duplicate"
  todo=subset(xy,subset=(is.na(raw.y)),select=c(sig,raw.x))
  colnames(todo)=c('sig','raw')
  todo$partials= as.character(sapply(todo$sig, agrep, yy$sig,max.distance = levDist,value=T))
  todo=merge(todo,yy,by.x='partials',by.y='sig')
  partial.matched=subset(todo,subset=(!(is.na(raw.x)) & !(is.na(raw.y))),select=c("sig","raw.x","raw.y"))
  partial.matched$pass="Partial"
  matched=rbind(matched,partial.matched)
  un.matched=subset(todo,subset=(is.na(raw.x)),select=c("sig","raw.x","raw.y"))
  if (nrow(un.matched)>0){
    un.matched$pass="Unmatched"
    matched=rbind(matched,un.matched)
  }
  matched=subset(matched,select=c("raw.x","raw.y","pass"))
  return(matched)
}

# loading twitter handles

load("handles_115_congress.RData")

# making names lower case and adding a name

handles_115_a <- handles_115_a %>%
  mutate(name = tolower(name),
         handle = user_screen_name,
         name = case_when(handle == "lindseygrahamsc" ~ "lindsey graham",
                          handle == "jasoninthehouse" ~ "jason chaffetz",
                          handle == "louiseslaughter" ~ "louise slaughter",
                          handle == "pattiberi" ~ "pat tiberi",
                          handle == "repblumenauer" ~ "earl blumenauer",
                          handle == "repcharliedent" ~ "charlie dent",
                          handle == "repjohnconyers" ~ "john conyers",
                          handle == "senjohnmccain" ~ "john mccain",
                          handle == "senfranken" ~ "al franken",
                          handle == "senthadcochran" ~ "thad cochran",
                          handle == "amashoffice" ~ "justin amash",
                          handle == "senatorstrange" ~ "luther strange",
                          handle == "senbillcassidy" ~ "william morgan cassidy",
                          TRUE ~ name)) %>%
  filter(!(is.na(name)))

# loading DW nominate data

DWNOM_115 <- read_csv("HS115_members.csv") %>%
  select(bioname, party_code, nominate_dim1) %>%
  mutate(bioname = tolower(bioname))

# putting into first last format

DWNOM_115[c("last_name", "first_name")] <- str_split_fixed(DWNOM_115$bioname,",",2)

# concatenating to get full name 

DWNOM_115 <- DWNOM_115 %>%
  mutate(name = paste(first_name, last_name))

# trying to do left join. did not work. 

DW_handle_joined_115 <- DWNOM_115 %>%
  left_join(handles_115_a, by = "name")

matches_115 <- partialMatch(DWNOM_115$name, handles_115_a$name)

congress_115_joined <- merge(DWNOM_115,matches_115, by.x="name",by.y="raw.x",all.x=T)
congress_115_joined <- merge(congress_115_joined,handles_115_a, by.x="raw.y",by.y="name",all.x=T)

# failed to join data

congress_115_missing <- congress_115_joined %>%
  filter(is.na(handle))

# data set of handles to join with the missing

handles_115_missing <- handles_115_a %>%
  filter(!(handle %in% unique(congress_115_joined$handle)))

