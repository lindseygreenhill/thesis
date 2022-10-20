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
## 117TH CONGRESS

# loading previous data and changing so I have just the last name

load("handles_117_congress.RData")

handles_117_congress <- handles_117_congress %>%
  mutate(last_name = tolower(sub(",.*", "", name))) %>%
  mutate(name = tolower(name))


DW_nom_117 <- read_csv("HS117_members.csv")

DW_nom_117 <- DW_nom_117 %>%
  mutate(last_name = tolower(sub(",.*", "", bioname))) %>%
  mutate(name = tolower(bioname)) %>%
  select(name, last_name, congress, party_code, nominate_dim1)




matches_117 <- partialMatch(DW_nom_117$name, handles_117_congress$name)

congress_117_joined <- merge(DW_nom_117,matches_117, by.x="name",by.y="raw.x",all.x=T)
congress_117_joined <- merge(congress_117_joined,handles_117_congress, by.x="raw.y",by.y="name",all.x=T)

# checking the missing data

congress_117_missing <- congress_117_joined %>%
  filter(is.na(handle)) %>%
  mutate(last_name = last_name.x)

# filtered data

congress_117_filtered <- congress_117_joined %>%
  filter(!(is.na(handle))) %>%
  select(name, nominate_dim1, handle)

# weird data

htd_117 <- c("SenSherrodBrown", "RepAnthonyBrown", "RepShontelBrown", "RepAGonzalez", 
             "RepGonzalez", "RepSamGraves", "RepRaulGraves", "SenRonJohnson", "SenBillJohnson", 
             "RepEBJ", "RepHankJohnson", "RepMikeJohnson", "SenMarkKelly", "MikeKellyPA", "RepRobinKelly",
             "RepTrentKelly", "RepMaloney", "RepSeanMaloney", "RepBarryMoore", "RepBlakeMoore",
             "RepGwenMoore", "RepKathleenRice", "RepTomRice", "RepHapRogers", "RepMikeRogersAL",
             "PatRyanUC", "RepTimRyan", "SenRickScott", "SenatorTimScott", "AustinScottGAO8",
             "repdavidscott", "BobbyScott", "SenTinaSmith", "RepAdamSmith", "RepAdrianSmith", 
             "RepJasonSmith", "BennieGThompson", "CongressmanGT", "RepThompson", "NormajTorres", 
             "RepRitchie", "RepWilson", "RepJoeWilson")


# trying to join with DW nom

congress_117_missing_2 <- congress_117_missing %>%
  left_join(handles_117_congress, by = ("last_name")) %>%
  mutate(handle = case_when(last_name == "san nicolas" ~ "guamCongressman",
                            last_name == "jacobs" ~ "RepJacobs",
                            last_name == "keller" ~ "RepFredKeller",
                            last_name == "vela" ~ "RepFilemonVela",
                            last_name == "radewagen" ~ "RepAmata",
                            last_name == "butterfield" ~ "GKButterfield",
                            last_name == "casey" ~ "SenBobCasey",
                            last_name == "hall" ~ "kwanzahall",
                            last_name == "payne" ~ "RepDonaldPayne",
                            last_name == "tiffany" ~ "TomTiffanyWI",
                            last_name == "mfume" ~ "Mfume4Congress",
                            last_name == "loeffler" ~ "KLoeffler",
                            TRUE ~ handle.y)) %>%
  select(name.x, nominate_dim1, name.y, last_name.x, handle)



# filtering out missing data

congress_117_missing_3 <- congress_117_missing_2 %>%
  filter(!(handle %in% htd_117)) %>%
  select(name = name.x, handle, nominate_dim1)

# adding back in data

handles_to_add_117 <- handles_117_congress %>%
  filter(handle %in% htd_117) %>%
  left_join(DW_nom_117, by = "name") %>%
  mutate(nominate_dim1 = case_when(handle == "SenMarkKelly" ~ -.178,
                                   handle == "SenRickScott" ~ .650,
                                   handle == "RepAnthonyBrown" ~ -.344,
                                   handle == "RepShontelBrown" ~ -.468,
                                   handle == "RepGonzalez" ~ -.379,
                                   handle == "RepSamGraves" ~ .443,
                                   handle == "RepHankJohnson" ~ -.468,
                                   handle == "RepRobinKelly" ~ -.462,
                                   handle == "RepMaloney" ~ -.387,
                                   handle == "RepGwenMoore" ~ -.525,
                                   handle == "RepKathleenRice" ~ -.280,
                                   handle == "RepMikeRogersAL" ~ .360,
                                   handle == "RepTimRyan" ~ -.402,
                                   handle == "BennieGThompson" ~ -.516,
                                   handle == "RepThompson" ~ -.395,
                                   handle == "RepJoeWilson" ~ .531,
                                   TRUE ~ nominate_dim1)) %>%
select(name, handle, nominate_dim1)

congress_117_missing_4 <- congress_117_missing_3 %>%
  rbind(handles_to_add_117)

# binding the missing rows

congress_117_full_joined <- congress_117_filtered %>%
  rbind(congress_117_missing_4) %>%
  unique()

save(congress_117_joined, file = "handles_DWNOM_joined_117.RData")

## 116TH CONGRESS ######################

# loading twitter handles

load("handles_116_congress.RData")

# fixing Andre Carson (actually juste ended up make_clean_names to do this for
# all special characters in names.)


handles_116[c("first_name", "last_name")] <- str_split_fixed(handles_116$name," ",2)

handles_116_clean <- handles_116 %>%
  mutate(bioname = paste(last_name, ", ",first_name,
                         sep="")) %>%
  select(bioname, handle, last_name) %>%
  mutate(name = bioname) %>%
  select(name, handle) %>%
  mutate(name = case_when(handle == "RepAndreCarson" ~ "Carson, Andre",
                        handle == "RepJenniffer" ~ "Gonzalez-colon, Jenniffer",
                        handle == "RepChuyGarcia" ~ "Garcia, Jesus",
                        handle == "RepBenRayLujan" ~ "Lujan, Ben Ray",
                        handle == "JoaquinCastrotx" ~ "Castro, Joaquin",
                        handle == "RepJoseSerrano" ~ "Serrano, Jose E",
                        handle == "RepLindaSanchez" ~ "Sanchez, Linda T",
                        handle == "RepBarragan" ~ "Barragan, Nanette Diaz",
                        handle == "NydiaVelazquez" ~ "Velazquez, Nydia M",
                        handle == "RepraulGrijalva" ~ "Grijalva, Raul",
                        handle == "RepCardenas" ~ "Cardenas, Tony",
                        handle == "RepGonzalez" ~ "Gonzalez, Vicente",
                        TRUE ~ name)) %>%
  mutate(name = tolower(name))
# loading DW nominate data

DW_nom_116 <- read_csv("HS116_members.csv") %>%
  mutate(last_name = tolower(sub(",.*", "", bioname))) %>%
  mutate(name = tolower(bioname)) %>%
  select(name, congress, party_code, nominate_dim1, last_name)

# creating partial matches

matches_116 <- partialMatch(DW_nom_116$name, handles_116_clean$name)

congress_116_joined <- merge(DW_nom_116,matches_116, by.x="name",by.y="raw.x",all.x=T)
congress_116_joined <- merge(congress_116_joined,handles_116_clean, by.x="raw.y",by.y="name",all.x=T)

congress_116_joined_filtered <- congress_116_joined %>%
  filter(!is.na(handle)) %>%
  select(name, nominate_dim1,handle)


# now solving for the matches that didn't work

congress_116_missing <- congress_116_joined %>%
  filter(is.na(handle)) %>%
  select(name, nominate_dim1) %>%
  mutate(last_name = substr(name,1,regexpr(",",name)-1))

handles_116_clean <- handles_116_clean %>%
  mutate(last_name = substr(name,1,regexpr(",",name)-1))

congress_116_missing_j <- congress_116_missing %>%
  left_join(handles_116_clean, by = "last_name") %>%
  mutate(handle = case_when(last_name == "san nicolas" ~ "guamCongressman",
                            last_name == "jacobs" ~ "RepJacobs",
                            last_name == "keller" ~ "RepFredKeller",
                            last_name == "vela" ~ "RepFilemonVela",
                            last_name == "radewagen" ~ "RepAmata",
                            last_name == "butterfield" ~ "GKButterfield",
                            last_name == "casey" ~ "SenBobCasey",
                            last_name == "hall" ~ "kwanzahall",
                            last_name == "payne" ~ "RepDonaldPayne",
                            last_name == "tiffany" ~ "TomTiffanyWI",
                            last_name == "mfume" ~ "Mfume4Congress",
                            last_name == "loeffler" ~ "KLoeffler",
                            TRUE ~ handle)) %>%
  select(name = name.x, nominate_dim1, handle)


congress_116_joined_full <- congress_116_joined_filtered %>%
  rbind(congress_116_missing_j) %>%
  unique()

htd <- c("justinamash", "RepPaulMitchell","BennieGThompson", "CongressmanGT", 
         "RepThompson","RepRobBishop", "SanfordBishop", "SenJackReed", "SenTomReed", 
         "RepChuyGarcia", "RepSylviaGarcia", "RepAGonzalez", "RepGonzalez", "RepGarretGraves", 
         "RepTomGraves", "RepTomRice", "RepChrisCollins", "RepDougCollins", "MikeKellyPA", "RepRobinKelly",
         "reptrentkelly", "RepWilson","SenTinaSmith","RepAdamSmith","RepJasonSmith", "RepMikeRogersAL", "ChrisMurphyCT",
         "RepStephMurphy","SteveKingIA", "SenJackReed", "RepStephMurphy", "SenRickScott", "SenatorTimScott",
         "AustinScottGA08","BobbyScott", "RepDavidScott", "RepHalRogers", "SenToddYoung", "RepAndyHarrisMD",
         "SenRickScott", "SenatorTimScott", "AustinScottGAO8", "BobbyScott", "RepDavidScott")

congress_116_joined_full_2 <- congress_116_joined_full %>%
  filter(!(handle %in% htd))

handles_to_add <- handles_116_clean %>%
  filter(handle %in% htd) %>%
  left_join(DW_nom_116, by = "name") %>%
  mutate(nominate_dim1 = case_when(handle =="RepRobBishop" ~ .537,
                                   handle =="SanfordBishop" ~ -.283, 
                                   handle =="SenJackReed" ~ -.367,
                                   handle =="RepGonzalez" ~ -.379, 
                                   handle =="RepRobinKelly" ~ -.462,
                                   handle =="RepMikeRogersAL" ~ .360, 
                                   handle =="ChrisMurphyCT" ~ -.271,
                                   handle =="BobbyScott" ~ -.450,
                                   handle =="RepHalRogers" ~ .338, 
                                   handle =="SenRickScott" ~ .650,
                                   TRUE ~ nominate_dim1)) %>%
  filter(nominate_dim1 != .654,
         !(party_code == 328 & last_name.y == "mitchell")) %>%
  select(name, handle, nominate_dim1)

congress_116_joined_full_3 <- congress_116_joined_full_2 %>%
  rbind(handles_to_add)

save(congress_116_joined_full_3, file = "handles_DWNOM_joined_116.RData")  
