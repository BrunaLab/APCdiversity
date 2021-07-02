#=============================================================================================================#
# Script created by Emilio M. Bruna (embruna@ufl.edu) to prep and clean
# data for the analyses in : PAPER CITATION TO BE ADDED 
# Script created in  R version 3.6.3 (2020-02-29)
# Uses packages bibliometrix_3.0.0 , tidyverse_1.3.0 , countrycode_1.1.3
#=============================================================================================================#

# the libraries
library(tidyverse)
library(countrycode) 
library(bibliometrix)
library(stringr)
################################################################
# load and add World Bank data on national income categories
CountryData <- read.csv("data_raw/CLASS.csv", header = TRUE)
CountryData <- CountryData[-1,]
CountryData <- CountryData %>%
  select(Code,Region, IncomeGroup = Income.group)

################################################################
# Load the list of Pawall Journals and their OA mirrors 
################################################################
MirrorPairs<-read_csv(file="./data_raw/MirrorPairs.csv")

################################################################
# Load the list of Waiver Countries
################################################################
# WaiverCountries<-read_csv(file="./data_raw/ElsevierWaivers.csv")
# # add to the waiver
# WaiverCountries$Code<-countrycode(WaiverCountries$Country,
#                                   "country.name",
#                                   "iso3c", warn = TRUE)
# 
# left_join(WaiverCountries, CountryData,by="Code")
# 
# # Note: Kosovo is not listed as an ISO standard country. 'XKX' is the unofficial code 
# # used by the European Commission and others until Kosovo is assigned an ISO code.
# WaiverCountries$Code[WaiverCountries$Country=="Kosovo"]<-"XKX"
# WaiverCountries<- WaiverCountries %>% select(-notes)
# # save the df as .csv file in "data_clean" folder
# write.csv(WaiverCountries,"./data_clean/WaiverCountries.csv", row.names = FALSE)
# # remove from the environment

################################################################
# Load the data on stipends
################################################################
stipends<-read_csv(file="./data_clean/stipends.csv")
# stipends$stipend_national<-round(stipends$stipend_national,0)
# stipends$stipend_USD<-round(stipends$stipend_USD,0)
# write.csv(stipends,"./data_clean/stipends.csv", row.names = FALSE)

# 12420
################################################################
# LOAD WOS & SCOPUS DATA
################################################################
WOS<-read_csv("./data_clean/WOS_july2020.csv")
WOS<-select(WOS,-X1)
WOS$db<-"wos" # add column to indicate what db these are from

# may_2020 last date, 
excludeWOS<-WOS %>% filter(PY==2020) %>% 
  select(PD) %>% 
  group_by(PD) %>% 
  summarize(n_distinct(PD)) 

months<-c("AUG","JUL","JUN","SEP","OCT","NOV")
excludeWOS$exclude<-sapply(excludeWOS$PD, function(x) any(sapply(months, str_detect, string = x)))
excludeWOS$PY<-2020
excludeWOS<-excludeWOS %>% filter(exclude=="TRUE")
WOS <- anti_join(WOS,excludeWOS,by=c("PD","PY"))
WOSrefID<-as_tibble(WOS$refID) %>% rename("refID"="value")

             
WOS_refined<-read_csv("./data_clean/WOS_refined.csv")
# 
# WOS_refined2<-WOS_refined %>% 
#   group_by(refID) %>% 
#   filter(refID %in% WOS) %>% 
#   arrange(refID)

WOS_refined <- semi_join(WOS_refined,WOSrefID)

scopus<-read_csv("./data_clean/scopus.csv")
scopus$db<-"scopus" # add column to indicate what db these are from
# NOTE THAT THERE are several rows in scopus csv file that are offset so not read in propoerly
# I checked all of these manually and they are all in the WOS dataset 
# so no data are lost


# Add a refID number to SCOPUS that starts 1 after the last WOS
scopus$refID<-seq(max(nrow(WOS)+1), max(nrow(WOS))+nrow(scopus),length.out = nrow(scopus))


################################################################
# PREP THE DATASETS FOR JOINING
################################################################

# some records are in both, but we want to keep the ones in WOS 
# because refsplitr is better at processing. 
# keep in `scopus` only the papers that are unique to scopus
unique_to_scopus<-setdiff(scopus$DI,WOS$DI)
# This will keep only thouse unique to scopus
scopus_data <- scopus %>% 
  filter(DI %in% unique_to_scopus) %>% 
  arrange(DI)

# to check use this: if zero, then there are 
# none in common that need to be deleted
in_common<-intersect(WOS$DI,scopus_data$DI)

rm(in_common,unique_to_scopus)
# scopus_data<-read_csv("./output/scopus_data.csv")

################################################################
# this will help inspect the resulting datasets to see how 
# many papers there are from each journal
################################################################

WOS_count_df<-WOS %>% select(pair_key,SO,PY,journal_cat,db)
scopus_count_df<-scopus_data %>% select(pair_key,SO,PY,journal_cat,db)
WOS_count_df$PY<-as.numeric(WOS_count_df$PY)

str(scopus_count_df)
str(WOS_count_df)
all<-bind_rows(scopus_count_df,WOS_count_df)

all_summary<-all %>% 
  group_by(pair_key,SO,journal_cat,db) %>% 
  tally() %>% 
  arrange(pair_key)
# write_csv(all_summary,"./output/all_summary.csv")

all_summary %>% filter(is.na(journal_cat)) %>% select(SO,db)

all_summary2<-all %>% 
  group_by(pair_key,SO,db) %>% 
  tally() %>% 
  arrange(pair_key)
# write_csv(all_summary2,"./output/all_summary2.csv")

all_summary3<-all %>% 
  group_by(journal_cat) %>% 
  tally()  
# write_csv(all_summary3,"./output/all_summary3.csv")
rm(all,all_summary)
################################################################
# REMOVE JOURNAL PAIRS IF NO DATA FOR ONE OR BOTH
################################################################

# 9: "Clinics and Research in Hepatology and Gastroenterology: X" 
# 13: Diabetes and Metabolism: X was discontinued
# 10: none for Graphics and Visual computing 


WOS$pair_key[WOS$SO == "DIABETES & METABOLISM"] <- 13
missing_jrnls<-c(9,10,13)
wos_data<-WOS %>% filter(!pair_key%in% missing_jrnls)
scopus_data<-scopus_data %>% filter(!pair_key%in% missing_jrnls)


################################################################
# Extract the 1st Author's Country from SCOPUS records with bibliometrix 
################################################################
AuthorFirst_scopus <- metaTagExtraction(scopus_data, Field = "AU1_CO", sep = ";")
AuthorFirst_scopus<-as_tibble(AuthorFirst_scopus)
AuthorFirst_scopus$AU1_CO
summary(as.factor(AuthorFirst_scopus$AU1_CO))
colnames(AuthorFirst_scopus)
AuthorFirst_scopus<-AuthorFirst_scopus %>% select(db,package,DI,refID,TI,
                                                  PY,pair_key,SO,journal_cat,
                                                  AU,AU_UN,AU1_CO)

# add a column with the author number for the AU1_CO
# extracted with refsplitr (it's one, obviously, as 
# it is first author)
AuthorFirst_scopus$author_order<-1
AuthorFirst_scopus<-as_tibble(AuthorFirst_scopus)
# nt sure why this isn't working...rlang issue? https://github.com/tidyverse/dplyr/issues/3252
# AuthorFirst_scopus<-AuthorFirst_scopus %>% rename(author_country=AU1_CO),university=AU_UN)
names(AuthorFirst_scopus)<- c("db","package","DI","refID","TI","PY","pair_key","SO",
                              "journal_cat","AU","university", "author_country","author_order")
# save the scopus 1st authors
write_csv(AuthorFirst_scopus,"./output/AuthorFirst_scopus.csv")

# AuthorFirst_scopus<-read_csv("./output/AuthorFirst_scopus.csv")
################################################################
# First Extract the counrtries in which the authors of a paper are based
# (the last column of the processed df) 
# https://cran.r-project.org/web/packages/rscopus/vignettes/multi_author.html
AuGeo_scopus <- metaTagExtraction(scopus_data, Field = "AU_CO", sep = ";")
AuGeo_scopus$AU_CO
AuGeo_scopus<-as_tibble(AuGeo_scopus)
colnames(AuGeo_scopus)

all_data_AuGeo<-AuGeo_scopus %>% select(db,package,DI,refID,TI,
                                       PY,pair_key,SO,journal_cat,
                                       AU,AU_UN,AU1_UN,AU_CO)


# The countries of all authors of an article are in one cell.
# this section splits them into multiple columns, then converts
# the df from wide to long form.
tempDF<- as.data.frame(str_split(all_data_AuGeo$AU_CO, ";", simplify = TRUE))
tempDF <- tempDF %>% mutate_all(na_if,"")  #replace the blanks with NA
# Need to do this next step or 'gather' won't work properly
tempDF <- data.frame(lapply(tempDF, as.character), stringsAsFactors=FALSE)
#bind the new dataframe of countries in wide form to original
all_data_AuGeo<-cbind(all_data_AuGeo,tempDF)
rm(tempDF) #remove the tempdf from environment
# gather into long form
colnames(all_data_AuGeo)
all_data_AuGeo<-all_data_AuGeo %>%
  gather(author_order,author_country,V1:ncol(all_data_AuGeo))
# remove the 'V' from cells in author column
# This also adds the order of authors for each paper
all_data_AuGeo$author_order<-gsub("V","",all_data_AuGeo$author_order)
all_data_AuGeo$author_order<-as.numeric(all_data_AuGeo$author_order)
# head(AllData,10)
# remove the NA rows of country column

all_data_AuGeo$author_country <- 
  ifelse((all_data_AuGeo$author_order==1 & is.na(all_data_AuGeo$author_country)),
         "unable_to_extract", all_data_AuGeo$author_country)

all_data_AuGeo<-all_data_AuGeo %>%  drop_na("author_country")
# organize the df by article, with authors in order from 1...N
all_data_AuGeo<-all_data_AuGeo %>% arrange(DI,TI,author_order)
all_data_AuGeo$DI<-as.factor(all_data_AuGeo$DI)
# delete the column with all countries in a single cell
all_data_AuGeo$AU_CO<-NULL

all_data_AuGeo<-as_tibble(all_data_AuGeo)
all_data_AuGeo<-all_data_AuGeo %>% 
  select(refID,author_order_AU_CO=author_order,author_country_AU_CO=author_country)
# 
# all_data_AuGeo<-all_data_AuGeo %>% 
#   select(refID,author_order,author_country)


# setdiff(all_data_AuGeo$refID,AuthorFirst_scopus$refID)

write.csv(all_data_AuGeo,"./data_clean/all_data_AuGeo.csv", row.names = FALSE)
# all_data_AuGeo<-read_csv("./data_clean/all_data_AuGeo.csv")
colnames(AuthorFirst_scopus)
colnames(all_data_AuGeo)
scopus_all_authors<-inner_join(AuthorFirst_scopus,all_data_AuGeo,by="refID")
colnames(scopus_all_authors)
scopus_all_authors$author_order<-
  ifelse((scopus_all_authors$author_order==scopus_all_authors$author_order_AU_CO),
                                        scopus_all_authors$author_order, NA)

scopus_all_authors$author_country<-
  ifelse(is.na(scopus_all_authors$author_order),NA,
         scopus_all_authors$author_country)


scopus_all_authors$first_au_comparison<-scopus_all_authors$author_country==scopus_all_authors$author_country_AU_CO

################################################################
# this will take the info on individual authors from WOS-refined
# and add the information on the journals, etc. from wos_data
# this is the last step needed to combine with scopus results

# first do some organizing of WOS_refined
WOS_refined<-as_tibble(WOS_refined)
WOS_refined$db<-"wos"
WOS_refined$package<-"refsplitr"
WOS_refined<-WOS_refined %>% 
  select(db,package,groupID,author_name,
         author_order,university,
         author_country=country,refID,PY)

wos_data<-wos_data %>% 
  select(db,package,refID,AU,pair_key,SO,TI,DI,journal_cat)
wos_data<-left_join(wos_data,WOS_refined) %>% 
  arrange(pair_key,SO,PY,author_order)
# save 
write_csv(wos_data,"./output/wos_data.csv")

# wos_data<-read_csv("./output/wos_data.csv")
# scopus_all_authors<-read_csv("./output/scopus_all_authors")
# BIND THEM UP
all_data<-bind_rows(wos_data,scopus_all_authors) %>% arrange(pair_key,SO,PY,author_order)

# do some cleanup
all_data$author_country<-tolower(all_data$author_country)
all_data$author_country_AU_CO<-tolower(all_data$author_country_AU_CO)
all_data$SO<-str_to_title(all_data$SO,locale="en")

# add country code


all_data$author_country[all_data$author_country == 'england'] <- "UK"
all_data$author_country[all_data$author_country == 'north ireland'] <- "UK"
all_data$author_country[all_data$author_country == 'scotland'] <- "UK"
all_data$author_country[all_data$author_country == 'united kingdom'] <- "UK"
all_data$author_country[all_data$author_country == 'wales'] <- "UK"
all_data$author_country[all_data$author_country == 'peoples r china'] <- "china"
all_data$author_country[all_data$author_country == 'korea'] <- "south korea"


all_data$author_country_AU_CO[all_data$author_country_AU_CO == 'england'] <- "UK"
all_data$author_country_AU_CO[all_data$author_country_AU_CO == 'north ireland'] <- "UK"
all_data$author_country_AU_CO[all_data$author_country_AU_CO == 'scotland'] <- "UK"
all_data$author_country_AU_CO[all_data$author_country_AU_CO == 'united kingdom'] <- "UK"
all_data$author_country_AU_CO[all_data$author_country_AU_CO == 'wales'] <- "UK"
all_data$author_country_AU_CO[all_data$author_country_AU_CO == 'peoples r china'] <- "china"
all_data$author_country_AU_CO[all_data$author_country_AU_CO == 'korea'] <- "south korea"


all_data$country_code<-
  countrycode(all_data$author_country,"country.name", "iso3c", warn = TRUE)

all_data$author_country[all_data$author_country=="png"]<-"papua n guinea"
all_data$country_code[all_data$author_country=="papua n guinea"]<-"PNG"
all_data$country_code[all_data$author_country=="guinea"]<-"GIN"
all_data$country_code[all_data$author_country=="cent afr republ"]<-"CAF"
all_data$country_code[all_data$author_country=="kosovo"]<-"XKX"

all_data$country_code_AU_CO<-
  countrycode(all_data$author_country_AU_CO,"country.name", "iso3c", warn = TRUE)

# 2x the journals

levels(as.factor(all_data$SO))

all_data$SO<-gsub("Water Research X","Water Research: X",all_data$SO)
all_data$SO<-gsub("Resources Conservation And Recycling","Resources, Conservation And Recycling",all_data$SO)
all_data$SO<-gsub("European Journal Of Obstetrics And Gynecology And Reproductive Biology: X",
                  "European Journal Of Obstetrics & Gynecology And Reproductive Biology: X",all_data$SO)
all_data$journal_cat[all_data$SO == 'European Journal Of Obstetrics & Gynecology And Reproductive Biology: X'] <- 'OA'
all_data$journal_cat[all_data$SO == 'Water Research: X'] <- 'OA'
all_data$journal_cat[all_data$SO == 'Resources, Conservation And Recycling: X'] <- 'OA'

 

# levels(as.factor(all_data$SO))
# 
# journals_list <- all_data %>% 
#   group_by(pair_key,SO, journal_cat) %>%
#   summarize(n())
# journals_list

# Correct the data types
all_data$author_country_AU_CO<-as.factor(all_data$author_country_AU_CO)
all_data$country_code_AU_CO<-as.factor(all_data$country_code_AU_CO)

all_data$author_country<-as.factor(all_data$author_country)
all_data$country_code<-as.factor(all_data$country_code)
all_data$journal_cat<-as.factor(all_data$journal_cat)
all_data$pair_key<-as.factor(all_data$pair_key)
all_data$SO<-as.factor(all_data$SO)
all_data$DI<-as.factor(all_data$DI)
all_data$db<-as.factor(all_data$db)
all_data$package<-as.factor(all_data$package)
all_data$groupID<-as.factor(all_data$groupID)
summary(all_data)
# THIS IS JUST THE FIRST AUTHORS
all_first<-all_data %>% filter(author_order==1)
all_first_summary<-all_first %>% group_by(country_code,journal_cat) %>% 
  tally() %>% arrange(journal_cat,desc(n),country_code)

all_first_summary %>% filter(is.na(country_code))
# 1.37% missing country

write.csv(all_data,"./output/all_data.csv", row.names = FALSE)
# write.csv(all_first_summary,"./output/all_first_summary.csv", row.names = FALSE)



all_first_missing_code<-all_data %>% 
  filter(author_order==1) %>% 
  filter(is.na(country_code)) %>% 
  select(db,package,DI) %>% 
  arrange(db,package)


first_missing_scopus<-all_first_missing_code %>%
  filter(db=="scopus") %>% 
  select(DI)
first_missing_scopus<-all_data %>% filter(DI %in% first_missing_scopus$DI) %>% filter(author_order==1)

first_missing_wos<-all_first_missing_code %>%
  filter(db=="wos") %>% 
  select(DI)
first_missing_wos<-all_data %>% filter(DI %in% first_missing_wos$DI) %>% filter(author_order==1)

summary(all_first_missing_code$first_au_comparison)

# # colnames(AllData_AuGeo)
# # first_missing_scopus<-all_data_AuGeo %>% filter(DI %in% all_first_missing_code$DI) %>% arrange(DI)
# # first_missing_wos<-wos_data %>% filter(DI %in% all_first_missing_code$DI) %>% arrange(DI,author_order)
# 
# # The countries of all authors of an article are in one cell.
# # this section splits them into multiple columns, then converts
# # the df from wide to long form.
# tempDF<- as.data.frame(str_split(first_missing_scopus$AU_CO, ";", simplify = TRUE))
# tempDF <- tempDF %>% mutate_all(na_if,"")  #replace the blanks with NA
# # Need to do this next step or 'gather' won't work properly
# tempDF <- data.frame(lapply(tempDF, as.character), stringsAsFactors=FALSE)
# #bind the new dataframe of countries in wide form to original
# first_missing_scopus<-cbind(first_missing_scopus,tempDF)
# rm(tempDF) #remove the tempdf from environment
# # gather into long form
# colnames(first_missing_scopus)
# first_missing_scopus<-first_missing_scopus %>%
#   gather(author_order,author_country,V1:ncol(first_missing_scopus))
# # remove the 'V' from cells in author column
# # This also adds the order of authors for each paper
# first_missing_scopus$author_order<-gsub("V","",first_missing_scopus$author_order)
# first_missing_scopus$author_order<-as.numeric(first_missing_scopus$author_order)
# # head(AllData,10)
# # remove the NA rows of country column
# first_missing_scopus<-first_missing_scopus %>%  drop_na("author_country")
# # organize the df by article, with authors in order from 1...N
# first_missing_scopus<-first_missing_scopus %>% arrange(DI,TI,author_order)
# first_missing_scopus$DI<-as.factor(first_missing_scopus$DI)
# # delete the column with all countries in a single cell
# first_missing_scopus$AU_CO<-NULL
write.csv(first_missing_scopus,"./data_clean/first_missing_scopus.csv", row.names = FALSE)
write.csv(first_missing_wos,"./data_clean/first_missing_wos.csv", row.names = FALSE)

# are all the countries for an article the same? 
authors_from_same<-all_data %>% 
  filter(DI %in% first_missing_scopus$DI) %>% 
  group_by(refID) %>% 
  summarize(n_countries=n_distinct(author_country_AU_CO)) %>% 
  arrange(desc(n_countries))
nrow(authors_from_same)
# n_countries=1, that means that country is the lead author
one_country_scopus<-authors_from_same %>% 
  filter(n_countries==1) %>% 
  select(-n_countries)

multi_country_scopus<-authors_from_same %>% 
  filter(n_countries>1) %>% 
  select(-n_countries)

nrow(multi_country_scopus)+nrow(one_country_scopus)
nrow(authors_from_same)

Scopus_1st<-semi_join(first_missing_scopus,one_country_scopus) 
Scopus_1st<-as_tibble(Scopus_1st)
Scopus_1st<-Scopus_1st %>% 
  group_by(refID) %>% 
  slice(1) %>% 
  select(package,DI,refID,author_order_AU_CO,
         inferred_author_country=author_country_AU_CO,
         country_code_AU_CO)

Scopus_1st$inferred_author_country<-as.character(Scopus_1st$inferred_author_country)
# Scopus_1st$author_country<-tolower(Scopus_1st$author_country)
# Scopus_1st$author_country<-as.factor(Scopus_1st$author_country)
# levels(all_data$country_code)
# levels(all_data$author_country)
# levels(Scopus_1st$author_country)

# TODO: can fold this BACK INTO all_data
# Scopus_1st<-Scopus_1st %>% select(package,DI,refID,author_order,inferred_author_country=author_country)


all_first$author_country<-as.character(all_first$author_country)
# foo<-all_first
all_first<-left_join(all_first,Scopus_1st)
# all_first<-ungroup(all_first)
# all_first$author_country<-as.factor(all_first$author_country)
# all_first$inferred_author_country<-as.factor(all_first$inferred_author_country)
all_first$author_country2 <- ifelse(is.na(all_first$author_country), all_first$inferred_author_country, as.character(all_first$author_country))



# TODO: country still NA, both bibliometrix and refsplitr
ManualCheck_first<-all_first %>% 
  filter(is.na(author_country2)) %>% 
  arrange(university)

# Take the institutional affiliations column, split them up, and keep 1st 
# The inst of all authors of an article are in one cell.
# this section splits them into multiple columns, then converts
# the df from wide to long form.
tempDF<- as.data.frame(str_split(ManualCheck_first$university, ";", simplify = TRUE))
tempDF <- tempDF %>% mutate_all(na_if,"")  #replace the blanks with NA
# Need to do this next step or 'gather' won't work properly
tempDF <- data.frame(lapply(tempDF, as.character), stringsAsFactors=FALSE)
#bind the new dataframe of countries in wide form to original
ManualCheck_first<-cbind(ManualCheck_first,tempDF)
rm(tempDF) #remove the tempdf from environment
# gather into long form
colnames(ManualCheck_first)
ManualCheck_first<-ManualCheck_first %>%
  gather(author_order,institution,V1:ncol(ManualCheck_first))
# remove the 'V' from cells in author column
# This also adds the order of authors for each paper
ManualCheck_first$author_order<-gsub("V","",ManualCheck_first$author_order)
ManualCheck_first$author_order<-as.numeric(ManualCheck_first$author_order)
# head(AllData,10)
# remove the NA rows of country column
ManualCheck_first<-ManualCheck_first %>%  drop_na("institution")
# organize the df by article, with authors in order from 1...N
ManualCheck_first<-ManualCheck_first %>% arrange(DI,TI,author_order)
ManualCheck_first$DI<-as.factor(ManualCheck_first$DI)
ManualCheck_first <- ManualCheck_first %>%
  arrange(author_order) %>% 
  filter(author_order==1) %>% 
  select(author_name,university,refID,PY,
         AU,pair_key,SO,TI,DI,author_order,institution)
# delete the column with all countries in a single cell
write.csv(ManualCheck_first,"./output/missing_1stauthor_institutions_2.csv", row.names = FALSE)

#### WE CHECKED EACH INSTITUION 1st AUTHOR INST USING THE MAP AFFIL TOOL
# http://abel.ischool.illinois.edu/cgi-bin/mapaffil/search.pl
# correections merged back in
ManualCheck_first_corrected<-read_csv("./output/missing_1stauthor_institutions_corrected.csv")

ManualCheck_first_corrected<-ManualCheck_first_corrected %>% 
  select(DI,author_order,inferred_country_manual)
# ManualCheck_first_corrected$inferred_country_manual<-as.factor(ManualCheck_first_corrected$inferred_country_manual)

all_first<-left_join(all_first,ManualCheck_first_corrected)

all_first$author_country3 <- ifelse(is.na(all_first$author_country2), all_first$inferred_country_manual, all_first$author_country2)

#add a country code

all_first$country_code3<-countrycode(all_first$author_country3,
                                  "country.name",
                                  "iso3c", warn = TRUE)




all_first$country_code3[all_first$author_country3=="PNG"]<-"PNG"
all_first$country_code3[all_first$author_country3=="papua n guinea"]<-"PNG"
all_first$country_code3[all_first$author_country3=="PNG"]<-"PNG"
all_first$country_code3[all_first$author_country3=="GIN"]<-"GIN"
all_first$country_code3[all_first$author_country3=="england"]<-"GBR"
all_first$country_code3[all_first$author_country3=="north ireland"]<-"GBR"
all_first$country_code3[all_first$author_country3=="scotland"]<-"GBR"
all_first$country_code3[all_first$author_country3=="wales"]<-"GBR"
all_first$country_code3[all_first$author_country3=="cent afr republ"]<-"CAF"
all_first$country_code3[all_first$author_country3=="kosovo"]<-"XKX"
all_first$country_code3[all_first$author_country3=="dubai"]<-"UAE"

all_first<-all_first %>% dplyr::rename(Code=country_code3)



################################################################
# ADD DATA ON COUNTRIES REGION AND INCOME TO AllData
head(CountryData)
all_first <- left_join(all_first, CountryData, by="Code")
all_data <-as_tibble(all_data)
all_first<-all_first %>% arrange(pair_key,SO,PY)


# save the csv
write.csv(all_first,"./data_clean/first_author_data.csv", row.names = FALSE)


###########################################
###########################################
# Merge first authors back into All_data
###########################################
###########################################
all_data_analysis<-left_join(all_data,all_first)

# rename columns

colnames(all_data_analysis)
all_data_analysis<-all_data_analysis %>%
  select(refID,db,package, 
         DOI = DI, pair_key,
         Journal = SO, 
         JrnlType = journal_cat, 
         Year = PY, Authors=AU,
         Title=TI, AuthorNum = author_order, 
         First_Author_Inst=university,
         First_Author=author_name,
         author_country_biblio1=author_country,
         author_country_biblio2=author_country_AU_CO,
         author_country_manual=inferred_country_manual,
         biblio1v2=first_au_comparison,
         First_Author_Country=author_country3,
         Code,Region,IncomeGroup)

              

# make IncomeGroup an ordered factor (Low to High)
all_data_analysis$IncomeGroup<- ordered(all_data_analysis$IncomeGroup, levels = c("Low income", "Lower middle income", "Upper middle income","High income"))
levels(all_data_analysis$IncomeGroup)[levels(all_data_analysis$IncomeGroup)=="High income"] <- "High"
levels(all_data_analysis$IncomeGroup)[levels(all_data_analysis$IncomeGroup)=="Low income"] <- "Low"
levels(all_data_analysis$IncomeGroup)[levels(all_data_analysis$IncomeGroup)=="Lower middle income"] <- "Lower middle"
levels(all_data_analysis$IncomeGroup)[levels(all_data_analysis$IncomeGroup)=="Upper middle income"] <- "Upper middle"
str(all_data_analysis)

# convert to factor
all_data_analysis$pair_key<-as.factor(all_data_analysis$pair_key)
all_data_analysis$pair_key<-droplevels(all_data_analysis$pair_key)
# levels(all_data_analysis$pair_key)
# There might be some with missing DOI values, if you don't replace missing DOI
# they will be excluded when grouping
all_data_analysis$DOI<- as.character(all_data_analysis$DOI)
all_data_analysis$DOI<- all_data_analysis$DOI %>% replace_na("missing_DOI")
all_data_analysis$DOI<- as.factor(all_data_analysis$DOI)
all_data_analysis<-all_data_analysis %>% arrange(Title,AuthorNum)
all_data_analysis$IncomeGroup <- as.factor(all_data_analysis$IncomeGroup)
all_data_analysis$IncomeGroup <- ordered(all_data_analysis$IncomeGroup, levels = c("High", "Upper middle","Lower middle","Low"))
# head(all_data_analysis)




############
# Need to remove all the OA articles in PW journals 
# (added after review, 28 nov 2020)

# all_data_analysis<-read_csv("./data_clean/all_data_analysis.csv")
OA_in_PW<-read_csv("./data_raw/OA_in_PW/OA_in_PW.csv") 
OA_in_PW<-OA_in_PW %>% select(DOI)

# the full record of all OA in PW
OA_in_PW<-semi_join(all_data_analysis,OA_in_PW)
n_distinct(OA_in_PW$DOI)
OA_in_PW$Journal<-droplevels(OA_in_PW$Journal)
OA_in_PW$JrnlType<-droplevels(OA_in_PW$JrnlType)
OA_in_PW$JrnlType
OA_in_PW<-filter(OA_in_PW,JrnlType=="PW")
OA_in_PW$ArticleType<-"OAinPW"
# to remove all the OA in the PW
all_data_analysis<-anti_join(all_data_analysis,OA_in_PW)
n_distinct(all_data_analysis$DOI)
n_distinct(all_data_analysis$DOI)
all_data_analysis$Journal<-droplevels(all_data_analysis$Journal)
all_data_analysis$JrnlType<-droplevels(all_data_analysis$JrnlType)
all_data_analysis$ArticleType<-all_data_analysis$JrnlType




# save the csv
write.csv(all_data_analysis,"./data_clean/all_data_analysis_noOAinPW.csv", row.names = FALSE)
write.csv(OA_in_PW,"./data_clean/OA_in_PW.csv", row.names = FALSE)

# all_data_analysis<-read_csv("./data_clean/all_data_analysis.csv")
# OA_in_PW<-read_csv("./data_clean/OA_in_PW.csv")


################################################################
# Change column names of Mirror Pairs and save csv to 'data_clean'
################################################################
MirrorPairs <- MirrorPairs%>%
  select(pair_key,Journal = SO, JrnlType = journal_cat, APC=apc,waiver) %>% 
  filter(pair_key>0) %>% 
  arrange(pair_key,JrnlType)

# save the df as .csv file in "data_clean" folder
write.csv(MirrorPairs,"./data_clean/MirrorPairs.csv", row.names = FALSE)
# remove from the environment
rm(MirrorPairs)

##############################################################
# END 30 NOV
##############################################################















































#########################
# THIS IS HELD OVER< HABVEN"T DONE ANYTHING WITH THIS YET"

##############################################
# AUTHOR AFFILIATIONS - CORRESPONDING AUTHORS
##############################################

Affiliations_1st_missing_scopus <- metaTagExtraction(articles_scopus_df, Field = "AU_UN", sep = ";")

Affiliations_scopus <-as_tibble(Affiliations_scopus)

AllData_CorrAffil<-bind_rows(Affiliations_scopus,Affiliations_wos)
AllData_CorrAffil$AU1_UN

AllData_CorrAffil<-AllData_CorrAffil %>% select(database,DI,TI,
                                                PY,SO,AU,AU1_UN)
write.csv(AllData_CorrAffil,"./data_clean/AllData_CorrAffil.csv", row.names = FALSE)



# The affiliations of all authors of an article are in one cell. 
# this section splits them into multiple columns, then converts
# the df from wide to long form.
tempDF<- as.data.frame(str_split(AllData_CorrAffil$AU1_UN, ";", simplify = TRUE))
tempDF <- tempDF %>% mutate_all(na_if,"")  #replace the blanks with NA
# Need to do this next step or 'gather' won't work properly
tempDF <- data.frame(lapply(tempDF, as.character), stringsAsFactors=FALSE) 
#bind the new dataframe of countries in wide form to original
AllData_CorrAffil<-cbind(AllData_CorrAffil,tempDF)
rm(tempDF) #remove the tempdf from environment
# gather into long form 
colnames(AllData_CorrAffil)
AllData_CorrAffil<-AllData_CorrAffil %>%
  gather(author,corr_affil,V1:ncol(AllData_CorrAffil))
# remove the 'V' from cells in author column
# This also adds the order of authors for each paper 
AllData_CorrAffil$author<-gsub("V","",AllData_CorrAffil$author) 
AllData_CorrAffil$author<-as.numeric(AllData_CorrAffil$author)
# head(AllData,10)

# AUTHOR IS MEANINGLESS, the order in the bibliometrix dataframe is NOT
# NECESSARILY THE ORDER OF AUTHORSHIP IN THE PAPER
AllData_CorrAffil<-AllData_CorrAffil %>% 
  select(-author) %>%
  filter(corr_affil!="NOTREPORTED")

# organize the df by article, with authors in order from 1...N
AllData_CorrAffil<-AllData_CorrAffil %>% arrange(TI,SO,corr_affil)

# remove the NA rows of country column
AllData_CorrAffil<-AllData_CorrAffil %>%  drop_na("corr_affil")
AllData_CorrAffil$DI<-as.factor(AllData_CorrAffil$DI)
# delete the column with all countries in a single cell
AllData_CorrAffil$AU1_UN<-NULL 
write.csv(AllData_CorrAffil,"./data_clean/AllData_CorrAffil.csv", row.names = FALSE)


















################################################################
# ADD DATA ON COUNTRIES REGION AND INCOME TO AllData
CountryData <-CountryData %>% rename("country_code"="Code")
all_data <- left_join(all_data, CountryData, by="country_code")
all_data <-as_tibble(all_data)
# remove CountryData from the environment
rm(CountryData)








all_data$country_check<-(all_data$AU1_CO==all_data$country)
country_check_summary<-all_data %>% filter(CountryNum==1) %>% 
  group_by(database,country_check) %>% 
  tally()



###############
###############



# remove from the environment
rm(all_articles_df,
   articles_wos,
   articles_scopus,
   articles_scopus_df,
   articles_wos_df,
   AuCountry_scopus,
   AuCountry_wos,
   AuthorFirst_wos,
   AuthorFirst_scopus,
   Affiliations_wos,
   Affiliations_scopus,
   AllData_AuthorFirst)











################################################################
# ALL DATA without papers that have a USA or CHN first or last author

# ID papers with a first author in USA or CHN
first_author_no_USA_CHN<-all_data %>%
  group_by(TI) %>% 
  filter(AuthorNum==1 & Country!="CHINA") %>% 
  filter (AuthorNum==1 & Country!="USA") %>% 
  select(TI)

# REMOVE THESE FROM THE all_data df
NO_first_author_USA_CHN<-semi_join(all_data,first_author_no_USA_CHN) %>% 
  arrange(Journal,TI,AuthorNum)

# Now find the ones in this new reduced df that have 
# china or usa as last author
last_author_no_USA_CHN<-NO_first_author_USA_CHN %>%
  group_by(TI) %>% 
  filter(AuthorNum == max(AuthorNum)) %>%
  filter(Country!="CHINA") %>% 
  filter(Country!="USA") %>% 
  select(TI)

# Remove them from the reduced df
NO_USA_CHN_FL<-semi_join(NO_first_author_USA_CHN,last_author_no_USA_CHN) %>% 
  arrange(Journal,TI,AuthorNum)

write.csv(NO_USA_CHN_FL,"./data_clean/NO_USA_CHN_FL.csv", row.names = FALSE)



one_author_pubs <- all_data %>%
  group_by(TI) %>% 
  summarize(n=n_distinct(AuthorNum)) %>% 
  filter(n==1) %>% 
  select(-n) %>% 
  left_join(all_data,by="TI")
one_author_pubs$Dataset<-"All Countries"
one_author_pubs$author<-"solo"

write.csv(one_author_pubs,"./data_clean/one_author_pubs_ALL.csv", row.names = FALSE)

coauthor_pubs<- all_data %>%
  group_by(TI) %>% 
  summarize(n=n_distinct(AuthorNum)) %>% 
  filter(n>=2) %>% 
  left_join(all_data,by="TI")
coauthor_pubs$Dataset<-"All Countries"
coauthor_pubs$author<-"CoAuthored"

write.csv(coauthor_pubs,"./data_clean/coauthor_pubs_ALL.csv", row.names = FALSE)

one_author_pubsNOCHNUSA <- NO_USA_CHN_FL %>%
  group_by(TI) %>% 
  summarize(n=n_distinct(AuthorNum)) %>% 
  filter(n==1) %>% 
  select(-n) %>% 
  left_join(NO_USA_CHN_FL,by="TI")
one_author_pubsNOCHNUSA$Dataset<-"CHN & USA excluded"
one_author_pubsNOCHNUSA$author<-"solo"

write.csv(one_author_pubsNOCHNUSA,"./data_clean/one_author_pubsNOCHNUSA.csv", row.names = FALSE)

coauthor_pubsNOCHNUSA<- NO_USA_CHN_FL %>%
  group_by(TI) %>% 
  summarize(n=n_distinct(AuthorNum)) %>% 
  filter(n>=2) %>% 
  left_join(NO_USA_CHN_FL,by="TI") 
coauthor_pubsNOCHNUSA$Dataset<-"CHN & USA excluded"
coauthor_pubsNOCHNUSA$author<-"CoAuthored"

write.csv(coauthor_pubsNOCHNUSA,"./data_clean/coauthor_pubsNOCHNUSA.csv", row.names = FALSE)

