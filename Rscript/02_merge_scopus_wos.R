
# Code overview -----------------------------------------------------------
# Script created by Emilio M. Bruna (embruna@ufl.edu) to prep and clean
# data for the analyses in : PAPER CITATION TO BE ADDED
# Script created in  R version 3.6.3 (2020-02-29)
# Uses packages bibliometrix_3.0.0 , tidyverse_1.3.0 , countrycode_1.1.3



# load the libraries ------------------------------------------------------
library(tidyverse)
library(countrycode)
library(bibliometrix)
library(stringr)


# load SCOPUS and WOS data ------------------------------------------------
WOS <- read_csv("./output/WOS_july2020.csv")
WOS <- select(WOS, -X1)
WOS$db <- "wos" # add column to indicate what db these are from

# may_2020 last date,
excludeWOS <- WOS %>%
  filter(PY == 2020) %>%
  select(PD) %>%
  group_by(PD) %>%
  summarize(n_distinct(PD))

months <- c("AUG", "JUL", "JUN", "SEP", "OCT", "NOV")
excludeWOS$exclude <- sapply(
  excludeWOS$PD,
  function(x) any(sapply(months, str_detect, string = x))
)
excludeWOS$PY <- 2020
excludeWOS <- excludeWOS %>% filter(exclude == "TRUE")
WOS <- anti_join(WOS, excludeWOS, by = c("PD", "PY"))
WOSrefID <- as_tibble(WOS$refID) %>% rename("refID" = "value")


WOS_refined <- read_csv("./output/WOS_refined.csv")
#
# WOS_refined2<-WOS_refined %>%
#   group_by(refID) %>%
#   filter(refID %in% WOS) %>%
#   arrange(refID)

WOS_refined <- semi_join(WOS_refined, WOSrefID)

scopus <- read_csv("./output/scopus.csv")
scopus$db <- "scopus" # add column to indicate what db these are from
# NOTE THAT THERE are several rows in scopus csv file that are offset so not read in propoerly
# I checked all of these manually and they are all in the WOS dataset
# so no data are lost


# Add a refID number to SCOPUS that starts 1 after the last WOS
scopus$refID <- seq(max(nrow(WOS) + 1),
  max(nrow(WOS)) + nrow(scopus),
  length.out = nrow(scopus)
)



# Prep datasets for joining -----------------------------------------------

# some records are in both, but we want to keep the ones in WOS
# because refsplitr is better at processing.
# keep in `scopus` only the papers that are unique to scopus
unique_to_scopus <- setdiff(scopus$DI, WOS$DI)
# This will keep only thouse unique to scopus
scopus_data <- scopus %>%
  filter(DI %in% unique_to_scopus) %>%
  arrange(DI)

# to check use this: if zero, then there are
# none in common that need to be deleted
in_common <- intersect(WOS$DI, scopus_data$DI)

rm(in_common, unique_to_scopus)
# scopus_data<-read_csv("./output/scopus_data.csv")

################################################################
# this will help inspect the resulting datasets to see how
# many papers there are from each journal
################################################################

WOS_count_df <- WOS %>% select(pair_key, SO, PY, journal_cat, db)
scopus_count_df <- scopus_data %>% select(pair_key, SO, PY, journal_cat, db)
WOS_count_df$PY <- as.numeric(WOS_count_df$PY)

# str(scopus_count_df)
# str(WOS_count_df)
all <- bind_rows(scopus_count_df, WOS_count_df)

all_summary <- all %>%
  group_by(pair_key, SO, journal_cat, db) %>%
  tally() %>%
  arrange(pair_key)
# write_csv(all_summary,"./output/all_summary.csv")

all_summary %>%
  filter(is.na(journal_cat)) %>%
  select(SO, db)

all_summary2 <- all %>%
  group_by(pair_key, SO, db) %>%
  tally() %>%
  arrange(pair_key)
all_summary2
# write_csv(all_summary2,"./output/all_summary2.csv")

all_summary3 <- all %>%
  group_by(journal_cat) %>%
  tally()
all_summary3
# write_csv(all_summary3,"./output/all_summary3.csv")
rm(all, all_summary,all_summary2,all_summary3)


# REMOVE JOURNAL PAIRS IF NO DATA FOR ONE OR BOTH -------------------------
# 9: "Clinics and Research in Hepatology and Gastroenterology: X"
# 13: Diabetes and Metabolism: X was discontinued
# 10: none for Graphics and Visual computing


WOS$pair_key[WOS$SO == "DIABETES & METABOLISM"] <- 13
missing_jrnls <- c(9, 10, 13)
wos_data <- WOS %>% filter(!pair_key %in% missing_jrnls)
scopus_data <- scopus_data %>% filter(!pair_key %in% missing_jrnls)



# Extract 1st Author Country from SCOPUS records with bibliometrix --------
AuthorFirst_scopus <- metaTagExtraction(scopus_data, Field = "AU1_CO", sep = ";")
AuthorFirst_scopus <- as_tibble(AuthorFirst_scopus)
AuthorFirst_scopus$AU1_CO
summary(as.factor(AuthorFirst_scopus$AU1_CO))
colnames(AuthorFirst_scopus)
AuthorFirst_scopus <- AuthorFirst_scopus %>% select(
  db, package, DI, refID, TI,
  PY, pair_key, SO, journal_cat,
  AU, AU_UN, AU1_CO
)

# add a column with the author number for the AU1_CO
# extracted with refsplitr (it's one, obviously, as
# it is first author)
AuthorFirst_scopus$author_order <- 1
AuthorFirst_scopus <- as_tibble(AuthorFirst_scopus)
# nt sure why this isn't working...rlang issue? https://github.com/tidyverse/dplyr/issues/3252
# AuthorFirst_scopus<-AuthorFirst_scopus %>% rename(author_country=AU1_CO),university=AU_UN)
names(AuthorFirst_scopus) <- c(
  "db", "package", "DI", "refID", "TI", "PY", "pair_key", "SO",
  "journal_cat", "AU", "university", "author_country", "author_order"
)
# save the scopus 1st authors
write_csv(AuthorFirst_scopus, "./output/AuthorFirst_scopus.csv")

# AuthorFirst_scopus<-read_csv("./output/AuthorFirst_scopus.csv")

################################################################
# First Extract the counrtries in which the authors of a paper are based
# (the last column of the processed df)
# https://cran.r-project.org/web/packages/rscopus/vignettes/multi_author.html
AuGeo_scopus <- metaTagExtraction(scopus_data, Field = "AU_CO", sep = ";")
AuGeo_scopus$AU_CO
AuGeo_scopus <- as_tibble(AuGeo_scopus)
colnames(AuGeo_scopus)

all_data_AuGeo <- AuGeo_scopus %>% select(
  db, package, DI, refID, TI,
  PY, pair_key, SO, journal_cat,
  AU, AU_UN, AU1_UN, AU_CO
)


# The countries of all authors of an article are in one cell.
# this section splits them into multiple columns, then converts
# the df from wide to long form.
tempDF <- as.data.frame(str_split(all_data_AuGeo$AU_CO, ";", simplify = TRUE))
tempDF <- tempDF %>% mutate_all(na_if, "") # replace the blanks with NA
# Need to do this next step or 'gather' won't work properly
tempDF <- data.frame(lapply(tempDF, as.character), stringsAsFactors = FALSE)
# bind the new dataframe of countries in wide form to original
all_data_AuGeo <- cbind(all_data_AuGeo, tempDF)
rm(tempDF) # remove the tempdf from environment
# gather into long form
colnames(all_data_AuGeo)
all_data_AuGeo <- all_data_AuGeo %>%
  gather(author_order, author_country, V1:ncol(all_data_AuGeo))
# remove the 'V' from cells in author column
# This also adds the order of authors for each paper
all_data_AuGeo$author_order <- gsub("V", "", all_data_AuGeo$author_order)
all_data_AuGeo$author_order <- as.numeric(all_data_AuGeo$author_order)
# head(AllData,10)
# remove the NA rows of country column

all_data_AuGeo$author_country <-
  ifelse((all_data_AuGeo$author_order == 1 & is.na(all_data_AuGeo$author_country)),
    "unable_to_extract", all_data_AuGeo$author_country
  )

all_data_AuGeo <- all_data_AuGeo %>% drop_na("author_country")
# organize the df by article, with authors in order from 1...N
all_data_AuGeo <- all_data_AuGeo %>% arrange(DI, TI, author_order)
all_data_AuGeo$DI <- as.factor(all_data_AuGeo$DI)
# delete the column with all countries in a single cell
all_data_AuGeo$AU_CO <- NULL

all_data_AuGeo <- as_tibble(all_data_AuGeo)
all_data_AuGeo <- all_data_AuGeo %>%
  select(refID, author_order_AU_CO = author_order, author_country_AU_CO = author_country)

colnames(AuthorFirst_scopus)
colnames(all_data_AuGeo)
scopus_all_authors <- inner_join(AuthorFirst_scopus, all_data_AuGeo, by = "refID")
colnames(scopus_all_authors)
scopus_all_authors$author_order <-
  ifelse((scopus_all_authors$author_order == scopus_all_authors$author_order_AU_CO),
    scopus_all_authors$author_order, NA
  )

scopus_all_authors$author_country <-
  ifelse(is.na(scopus_all_authors$author_order), NA,
    scopus_all_authors$author_country
  )


scopus_all_authors$first_au_comparison <-
  scopus_all_authors$author_country == scopus_all_authors$author_country_AU_CO

################################################################
# this will take the info on individual authors from WOS-refined
# and add the information on the journals, etc. from wos_data
# this is the last step needed to combine with scopus results

# first do some organizing of WOS_refined
WOS_refined <- as_tibble(WOS_refined)
WOS_refined$db <- "wos"
WOS_refined$package <- "refsplitr"
WOS_refined <- WOS_refined %>%
  select(db, package, groupID, author_name,
    author_order, university,
    author_country = country, refID, PY
  )

wos_data <- wos_data %>%
  select(db, package, refID, AU, pair_key, SO, TI, DI, journal_cat)
wos_data <- left_join(wos_data, WOS_refined) %>%
  arrange(pair_key, SO, PY, author_order)
# save
write_csv(wos_data, "./output/wos_data.csv")

# wos_data<-read_csv("./output/wos_data.csv")
# scopus_all_authors<-read_csv("./output/scopus_all_authors")
# BIND THEM UP
all_data <- bind_rows(wos_data, scopus_all_authors) %>%
  arrange(pair_key, SO, PY, author_order)

# Remove from memory all that we no longer need
rm(all_data_AuGeo, 
   AuGeo_scopus, 
   AuthorFirst_scopus,
   excludeWOS,scopus,
   scopus_all_authors,
   scopus_count_df,
   scopus_data,
   WOS,
   WOS_count_df,
   wos_data,
   WOS_refined, 
   WOSrefID,
   missing_jrnls,
   months)
# names(all_data)
# summary(as.factor(all_data$author_country_AU_CO))
# do some cleanup

all_data$author_country <- tolower(all_data$author_country)
all_data$author_country_AU_CO <- tolower(all_data$author_country_AU_CO)
all_data$SO <- str_to_title(all_data$SO, locale = "en")

# add country code


all_data$author_country[all_data$author_country == "england"] <- "UK"
all_data$author_country[all_data$author_country == "north ireland"] <- "UK"
all_data$author_country[all_data$author_country == "scotland"] <- "UK"
all_data$author_country[all_data$author_country == "united kingdom"] <- "UK"
all_data$author_country[all_data$author_country == "wales"] <- "UK"
all_data$author_country[all_data$author_country == "peoples r china"] <- "china"
all_data$author_country[all_data$author_country == "korea"] <- "south korea"


# foo<-as.data.frame(summary(as.factor(all_data$author_country)))

all_data$author_country_AU_CO[all_data$author_country_AU_CO == "england"] <- "UK"
all_data$author_country_AU_CO[all_data$author_country_AU_CO == "north ireland"] <- "UK"
all_data$author_country_AU_CO[all_data$author_country_AU_CO == "scotland"] <- "UK"
all_data$author_country_AU_CO[all_data$author_country_AU_CO == "united kingdom"] <- "UK"
all_data$author_country_AU_CO[all_data$author_country_AU_CO == "wales"] <- "UK"
all_data$author_country_AU_CO[all_data$author_country_AU_CO == "peoples r china"] <- "china"
all_data$author_country_AU_CO[all_data$author_country_AU_CO == "korea"] <- "south korea"


all_data$country_code <-
  countrycode(all_data$author_country, "country.name", "iso3c", warn = TRUE)

all_data$author_country[all_data$author_country == "png"] <- "papua n guinea"
all_data$country_code[all_data$author_country == "papua n guinea"] <- "PNG"
all_data$country_code[all_data$author_country == "guinea"] <- "GIN"
all_data$country_code[all_data$author_country == "cent afr republ"] <- "CAF"
all_data$country_code[all_data$author_country == "kosovo"] <- "XKX"

# foo<-as.data.frame(summary(as.factor(all_data$country_code)))


all_data$country_code_AU_CO <-
  countrycode(all_data$author_country_AU_CO, "country.name", "iso3c", warn = TRUE)

# 2x the journals

levels(as.factor(all_data$SO))

all_data$SO <- gsub("Water Research X", "Water Research: X", all_data$SO)
all_data$SO <- gsub("Resources Conservation And Recycling", "Resources, Conservation And Recycling", all_data$SO)
all_data$SO <- gsub(
  "European Journal Of Obstetrics And Gynecology And Reproductive Biology: X",
  "European Journal Of Obstetrics & Gynecology And Reproductive Biology: X", all_data$SO
)
all_data$journal_cat[all_data$SO == "European Journal Of Obstetrics & Gynecology And Reproductive Biology: X"] <- "OA"
all_data$journal_cat[all_data$SO == "Water Research: X"] <- "OA"
all_data$journal_cat[all_data$SO == "Resources, Conservation And Recycling: X"] <- "OA"



# levels(as.factor(all_data$SO))
#
# journals_list <- all_data %>%
#   group_by(pair_key,SO, journal_cat) %>%
#   summarize(n())
# journals_list

# Correct the data types
all_data$author_country_AU_CO <- as.factor(all_data$author_country_AU_CO)
all_data$country_code_AU_CO <- as.factor(all_data$country_code_AU_CO)

all_data$author_country <- as.factor(all_data$author_country)
all_data$country_code <- as.factor(all_data$country_code)
all_data$journal_cat <- as.factor(all_data$journal_cat)
all_data$pair_key <- as.factor(all_data$pair_key)
all_data$SO <- as.factor(all_data$SO)
all_data$DI <- as.factor(all_data$DI)
all_data$db <- as.factor(all_data$db)
all_data$package <- as.factor(all_data$package)
all_data$groupID <- as.factor(all_data$groupID)
summary(all_data)
# THIS IS JUST THE FIRST AUTHORS

all_first <- all_data %>% filter(author_order == 1)
all_first_summary <- all_first %>%
  group_by(country_code, journal_cat) %>%
  tally() %>%
  arrange(journal_cat, desc(n), country_code)

all_first_summary %>% filter(is.na(country_code))

# percentage that is missing country 
  
no_country<-(all_first_summary %>% 
  filter(is.na(country_code)) %>% 
  summarize(sum(n)))

with_country<-all_first_summary %>% 
  filter(is.na(country_code)==FALSE) %>% 
  ungroup() %>% 
  summarize(sum(n))
no_country[1,2]/with_country[1,1]*100



all_first_missing_code <- all_data %>%
  filter(author_order == 1) %>%
  filter(is.na(country_code)) %>%
  select(db, package, DI) %>%
  arrange(db, package)


first_missing_scopus <- all_first_missing_code %>%
  filter(db == "scopus") %>%
  select(DI)
first_missing_scopus <- all_data %>%
  filter(DI %in% first_missing_scopus$DI) %>%
  filter(author_order == 1)

first_missing_wos <- all_first_missing_code %>%
  filter(db == "wos") %>%
  select(DI)
first_missing_wos <- all_data %>%
  filter(DI %in% first_missing_wos$DI) %>%
  filter(author_order == 1)

# are all the countries for an article the same?
authors_from_same <- all_data %>%
  filter(DI %in% first_missing_scopus$DI) %>%
  group_by(refID) %>%
  summarize(n_countries = n_distinct(author_country_AU_CO)) %>%
  arrange(desc(n_countries))
nrow(authors_from_same)
# n_countries=1, that means that country is the lead author
one_country_scopus <- authors_from_same %>%
  filter(n_countries == 1) %>%
  select(-n_countries)

multi_country_scopus <- authors_from_same %>%
  filter(n_countries > 1) %>%
  select(-n_countries)

nrow(multi_country_scopus) + nrow(one_country_scopus)
nrow(authors_from_same)

Scopus_1st <- semi_join(first_missing_scopus, one_country_scopus)
Scopus_1st <- as_tibble(Scopus_1st)
Scopus_1st <- Scopus_1st %>%
  group_by(refID) %>%
  slice(1) %>%
  select(package, DI, refID, author_order_AU_CO,
    inferred_author_country = author_country_AU_CO,
    country_code_AU_CO
  )

Scopus_1st$inferred_author_country <- as.character(Scopus_1st$inferred_author_country)
# Scopus_1st$author_country<-tolower(Scopus_1st$author_country)
# Scopus_1st$author_country<-as.factor(Scopus_1st$author_country)
# levels(all_data$country_code)
# levels(all_data$author_country)
# levels(Scopus_1st$author_country)

# TODO: can fold this BACK INTO all_data
# Scopus_1st<-Scopus_1st %>% select(package,DI,refID,author_order,inferred_author_country=author_country)


all_first$author_country <- as.character(all_first$author_country)
# foo<-all_first
all_first <- left_join(all_first, Scopus_1st)
# all_first<-ungroup(all_first)
# all_first$author_country<-as.factor(all_first$author_country)
# all_first$inferred_author_country<-as.factor(all_first$inferred_author_country)
all_first$author_country2 <- ifelse(is.na(all_first$author_country),
  all_first$inferred_author_country,
  as.character(all_first$author_country)
)


# This will check all author_country to to see if you have a country in 
# author_country_AU_CO and if you do, drop it in there.

# how many are there with all_first$author_country == NA?

missing_author_country2 <- all_first %>% select(author_country2,
                     author_country_AU_CO,
                     inferred_author_country) %>% 
  filter(is.na(author_country2))

summary(missing_author_country2$author_country_AU_CO==
          missing_author_country2$inferred_author_country)

summary(is.na(missing_author_country2$author_country2)) # count is the ones == TRUE
summary(is.na(missing_author_country2$author_country_AU_CO)) # count is the ones == TRUE
summary(is.na(missing_author_country2$inferred_author_country)) # count is the ones == TRUE
all_first$author_country2 <- ifelse(is.na(all_first$author_country2),
                                    all_first$author_country_AU_CO,
                                    as.character(all_first$author_country2)
                                    )

summary(is.na(all_first$author_country2)) # count is the ones == TRUE

rm(all_first_missing_code,
   all_first_summary,
   authors_from_same,
   first_missing_scopus,
   first_missing_wos,
   missing_author_country2,
   multi_country_scopus,
   no_country,
   one_country_scopus,
   Scopus_1st,
   with_country)

# TODO: country still NA, both bibliometrix and refsplitr
ManualCheck_first <- all_first %>%
  filter(is.na(author_country2)) %>%
  arrange(university)

# Take the institutional affiliations column, split them up, and keep 1st
# The inst of all authors of an article are in one cell.
# this section splits them into multiple columns, then converts
# the df from wide to long form.
tempDF <- as.data.frame(str_split(ManualCheck_first$university, ";", simplify = TRUE))
tempDF <- tempDF %>% mutate_all(na_if, "") # replace the blanks with NA
# Need to do this next step or 'gather' won't work properly
tempDF <- data.frame(lapply(tempDF, as.character), stringsAsFactors = FALSE)
# bind the new dataframe of countries in wide form to original
ManualCheck_first <- cbind(ManualCheck_first, tempDF)
rm(tempDF) # remove the tempdf from environment
# gather into long form
colnames(ManualCheck_first)
ManualCheck_first <- ManualCheck_first %>%
  gather(author_order, institution, V1:ncol(ManualCheck_first))
# remove the 'V' from cells in author column
# This also adds the order of authors for each paper
ManualCheck_first$author_order <- gsub("V", "", ManualCheck_first$author_order)
ManualCheck_first$author_order <- as.numeric(ManualCheck_first$author_order)
# head(AllData,10)
# remove the NA rows of country column

# this removes any columns where all values of column are NA
ManualCheck_first <- ManualCheck_first %>% select(where(~!all(is.na(.x))))
# organize the df by article, with authors in order from 1...N
ManualCheck_first <- ManualCheck_first %>% arrange(DI, TI, author_order)
ManualCheck_first$DI <- as.factor(ManualCheck_first$DI)
ManualCheck_first <- ManualCheck_first %>%
  arrange(author_order) %>%
  filter(author_order == 1) %>%
  select(
    author_name, refID, PY,
    AU, pair_key, SO, TI, DI, author_order
  )
# delete the column with all countries in a single cell
write_csv(ManualCheck_first, "./output/missing_1stauthor_institutions.csv")

#### WE CHECKED EACH INSTITUION 1st AUTHOR INST USING THE MAP AFFIL TOOL
# http://abel.ischool.illinois.edu/cgi-bin/mapaffil/search.pl
# correections merged back in
ManualCheck_first_corrected <- read_csv("./data_raw/missing_1stauthor_institutions_corrected.csv")

ManualCheck_first_corrected <- ManualCheck_first_corrected %>%
  select(DI, author_order, inferred_country_manual)
# ManualCheck_first_corrected$inferred_country_manual<-as.factor(ManualCheck_first_corrected$inferred_country_manual)

all_first <- left_join(all_first, ManualCheck_first_corrected)

rm(ManualCheck_first,
   ManualCheck_first_corrected)

all_first$author_country3 <- ifelse(is.na(all_first$author_country2),
  all_first$inferred_country_manual,
  all_first$author_country2
)

# add a country code

all_first$country_code3 <- countrycode(all_first$author_country3,
  "country.name",
  "iso3c",
  warn = TRUE
)



all_first$country_code3[all_first$author_country3 == "PNG"] <- "PNG"
all_first$country_code3[all_first$author_country3 == "papua n guinea"] <- "PNG"
all_first$country_code3[all_first$author_country3 == "PNG"] <- "PNG"
all_first$country_code3[all_first$author_country3 == "GIN"] <- "GIN"
all_first$country_code3[all_first$author_country3 == "england"] <- "GBR"
all_first$country_code3[all_first$author_country3 == "north ireland"] <- "GBR"
all_first$country_code3[all_first$author_country3 == "scotland"] <- "GBR"
all_first$country_code3[all_first$author_country3 == "wales"] <- "GBR"
all_first$country_code3[all_first$author_country3 == "cent afr republ"] <- "CAF"
all_first$country_code3[all_first$author_country3 == "kosovo"] <- "XKX"
all_first$country_code3[all_first$author_country3 == "dubai"] <- "UAE"

all_first <- all_first %>% dplyr::rename(Code = country_code3)



no_country<-all_first %>%
  select(DI,Code) %>% 
  filter(is.na(Code)) %>% 
  summarize(n_distinct(DI))
with_country<-all_first %>% 
  filter(is.na(Code)==FALSE) %>% 
  summarize(n_distinct(DI))
no_country[1,1]/with_country[1,1]*100 
rm(no_country,with_country)
################################################################
# ADD DATA ON COUNTRIES REGION AND INCOME TO AllData
# load. add World Bank data (nat income cats) -----------------------------

CountryData <- read_csv("data_clean/CountryData.csv")

head(CountryData)
all_first <- left_join(all_first, CountryData, by = "Code")
all_data <- as_tibble(all_data)
all_first <- all_first %>% arrange(pair_key, SO, PY)


# save the csv
# write_csv(all_first, "./data_clean/first_author_data.csv")



###########################################
###########################################
# Merge first authors back into All_data
###########################################
###########################################

all_data_analysis <- left_join(all_data, all_first)

# rename columns

colnames(all_data_analysis)
all_data_analysis <- all_data_analysis %>%
  select(refID, db, package,
    DOI = DI, pair_key,
    Journal = SO,
    JrnlType = journal_cat,
    Year = PY, Authors = AU,
    Title = TI, AuthorNum = author_order,
    First_Author_Inst = university,
    First_Author = author_name,
    author_country_biblio1 = author_country,
    author_country_biblio2 = author_country_AU_CO,
    author_country_manual = inferred_country_manual,
    biblio1v2 = first_au_comparison,
    First_Author_Country = author_country3,
    Code, Region, IncomeGroup
  )



# make IncomeGroup an ordered factor (Low to High)
all_data_analysis$IncomeGroup <- ordered(all_data_analysis$IncomeGroup, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))
levels(all_data_analysis$IncomeGroup)[levels(all_data_analysis$IncomeGroup) == "High income"] <- "High"
levels(all_data_analysis$IncomeGroup)[levels(all_data_analysis$IncomeGroup) == "Low income"] <- "Low"
levels(all_data_analysis$IncomeGroup)[levels(all_data_analysis$IncomeGroup) == "Lower middle income"] <- "Lower middle"
levels(all_data_analysis$IncomeGroup)[levels(all_data_analysis$IncomeGroup) == "Upper middle income"] <- "Upper middle"
str(all_data_analysis)

# convert to factor
all_data_analysis$pair_key <- as.factor(all_data_analysis$pair_key)
all_data_analysis$pair_key <- droplevels(all_data_analysis$pair_key)
# levels(all_data_analysis$pair_key)
# There might be some with missing DOI values, if you don't replace missing DOI
# they will be excluded when grouping
all_data_analysis$DOI <- as.character(all_data_analysis$DOI)
all_data_analysis$DOI <- all_data_analysis$DOI %>% replace_na("missing_DOI")
all_data_analysis$DOI <- as.factor(all_data_analysis$DOI)
all_data_analysis <- all_data_analysis %>% arrange(Title, AuthorNum)
all_data_analysis$IncomeGroup <- as.factor(all_data_analysis$IncomeGroup)
all_data_analysis$IncomeGroup <- ordered(all_data_analysis$IncomeGroup,
  levels = c("High", "Upper middle", "Lower middle", "Low")
)
# head(all_data_analysis)




############
# Need to remove all the OA articles in PW journals
# (added after review, 28 nov 2020)

# all_data_analysis<-read_csv("./data_clean/all_data_analysis.csv")
OA_in_PW <- read_csv("./data_raw/OA_in_PW/OA_in_PW.csv")
OA_in_PW <- OA_in_PW %>% select(DOI)

# the full record of all OA in PW
OA_in_PW <- semi_join(all_data_analysis, OA_in_PW)
n_distinct(OA_in_PW$DOI)
OA_in_PW$Journal <- droplevels(OA_in_PW$Journal)
OA_in_PW$JrnlType <- droplevels(OA_in_PW$JrnlType)
OA_in_PW$JrnlType
OA_in_PW <- filter(OA_in_PW, JrnlType == "PW")
OA_in_PW$ArticleType <- "OAinPW"
# to remove all the OA in the PW
all_data_analysis <- anti_join(all_data_analysis, OA_in_PW)
n_distinct(all_data_analysis$DOI)
n_distinct(all_data_analysis$DOI)
all_data_analysis$Journal <- droplevels(all_data_analysis$Journal)
all_data_analysis$JrnlType <- droplevels(all_data_analysis$JrnlType)
all_data_analysis$ArticleType <- all_data_analysis$JrnlType




# save the csv
write_csv(all_data_analysis, "./output/data_noOAinPW.csv")
write_csv(OA_in_PW, "./output/data_OAinPW.csv")


# load the rest of the data, bind to the OA in PW
AllData<-all_data_analysis
# AllData<-read_csv(file="./output/data_noOAinPW.csv")
# AllData<-ungroup(AllData)
summary(as.factor(AllData$ArticleType))


AllData<-bind_rows(AllData,OA_in_PW)
summary(as.factor(AllData$ArticleType))

AllData$pair_key<-as.factor(AllData$pair_key)
AllData$pair_key<-droplevels(AllData$pair_key)

AllData$First_Author_Country[AllData$First_Author_Country=="uk"|
                               AllData$First_Author_Country=="UK"|
                               AllData$First_Author_Country=="GBR"]<-"gbr"

summary((as.factor(AllData$First_Author_Country)))
# 56 NA articles


AllData_missing<-AllData %>% 
  filter(AuthorNum==1) %>% 
  filter(is.na(First_Author_Country)) %>% 
  select(refID) 
AllData<-AllData %>% filter(!refID %in% AllData_missing$refID) 

rm(AllData_missing, OA_in_PW)

AllData<-AllData %>%
  group_by(refID) %>%
  mutate(n=n_distinct(AuthorNum)) %>%
  mutate(author = ifelse(n == 1, "solo", "coauthored")) %>% 
  select(-n)

AllData<-AllData %>%
  group_by(refID) %>%
  mutate(n=n_distinct(AuthorNum)) %>%
  mutate(author = ifelse(n == 1, "solo", "coauthored")) %>% 
  select(-n)



AllData<-AllData %>%
  group_by(pair_key) %>%
  mutate(JrnlType = ifelse(JrnlType=="OAinPW", "PW", as.character(JrnlType)))
AllData$JrnlType<-as.factor(AllData$JrnlType)

AllData<-AllData %>%
  group_by(pair_key) %>%
  mutate(ArticleType = ifelse(ArticleType=="OAinPW", "OA", ArticleType))
AllData$ArticleType<-as.factor(AllData$ArticleType)




############ read in rsrch_area ############
RsrchAreas<-read_csv("./data_raw/journal_areas.csv") %>% 
  select(pair_key,Journal,wos_cat1,wos_cat2,subcat_1)
RsrchAreas$wos_cat1 <- as.factor(RsrchAreas$wos_cat1)
RsrchAreas$subcat_1 <- as.factor(RsrchAreas$subcat_1)
RsrchAreas$pair_key <- as.factor(RsrchAreas$pair_key)
levels(as.factor(AllData$Journal))==levels(as.factor(RsrchAreas$Journal))
AllData<-left_join(AllData,RsrchAreas)
AllData$pair_key<-as.factor(AllData$pair_key)
AllData$Journal<-as.factor(AllData$Journal)
AllData$ArticleType<-as.factor(AllData$ArticleType)

levels(AllData$Journal)
levels(as.factor(AllData$Region))




AllData$Code[AllData$author_country_biblio2 == "algeria"] <- "DZA"
AllData$Code[AllData$author_country_biblio2 == "argentina"] <- "ARG"
AllData$Code[AllData$author_country_biblio2 == "australia"] <- "AUS"
AllData$Code[AllData$author_country_biblio2 == "austria"] <- "AUT"
AllData$Code[AllData$author_country_biblio2 == "bahrain"] <- "BHR"
AllData$Code[AllData$author_country_biblio2 == "bangladesh"] <- "BGD"
AllData$Code[AllData$author_country_biblio2 == "belgium"] <- "BEL"
AllData$Code[AllData$author_country_biblio2 == "bolivia"] <- "BOL"
AllData$Code[AllData$author_country_biblio2 == "brazil"] <- "BRA"
AllData$Code[AllData$author_country_biblio2 == "canada"] <- "CAN"
AllData$Code[AllData$author_country_biblio2 == "chile"] <- "CHL"
AllData$Code[AllData$author_country_biblio2 == "china"] <- "CHN"
AllData$Code[AllData$author_country_biblio2 == "costa rica"] <- "CRI"
AllData$Code[AllData$author_country_biblio2 == "czech republic"] <- "CZE"
AllData$Code[AllData$author_country_biblio2 == "denmark"] <- "DNK"
AllData$Code[AllData$author_country_biblio2 == "ecuador"] <- "ECU"
AllData$Code[AllData$author_country_biblio2 == "egypt"] <- "EGY"
AllData$Code[AllData$author_country_biblio2 == "ethiopia"] <- "ETH"
AllData$Code[AllData$author_country_biblio2 == "france"] <- "FRA"
AllData$Code[AllData$author_country_biblio2 == "germany"] <- "DEU"
AllData$Code[AllData$author_country_biblio2 == "ghana"] <- "GHA"
AllData$Code[AllData$author_country_biblio2 == "greece"] <- "GRC"
AllData$Code[AllData$author_country_biblio2 == "haiti"] <- "HTI"
AllData$Code[AllData$author_country_biblio2 == "hungary"] <- "HUN"
AllData$Code[AllData$author_country_biblio2 == "india"] <- "IND"
AllData$Code[AllData$author_country_biblio2 == "indonesia"] <- "IDN"
AllData$Code[AllData$author_country_biblio2 == "iran"] <- "IRN"
AllData$Code[AllData$author_country_biblio2 == "iraq"] <- "IRQ"
AllData$Code[AllData$author_country_biblio2 == "ireland"] <- "IRL"
AllData$Code[AllData$author_country_biblio2 == "israel"] <- "ISR"
AllData$Code[AllData$author_country_biblio2 == "italy"] <- "ITA"
AllData$Code[AllData$author_country_biblio2 == "malaysia"] <- "MYS"
AllData$Code[AllData$author_country_biblio2 == "malta"] <- "MLT"
AllData$Code[AllData$author_country_biblio2 == "myanmar"] <- "MMR"
AllData$Code[AllData$author_country_biblio2 == "nepal"] <- "NPL"
AllData$Code[AllData$author_country_biblio2 == "netherlands"] <- "NLD"
AllData$Code[AllData$author_country_biblio2 == "nigeria"] <- "NGA"
AllData$Code[AllData$author_country_biblio2 == "norway"] <- "NOR"
AllData$Code[AllData$author_country_biblio2 == "portugal"] <- "PRT"
AllData$Code[AllData$author_country_biblio2 == "saudi arabia"] <- "SAU"
AllData$Code[AllData$author_country_biblio2 == "south korea"] <- "KOR"
AllData$Code[AllData$author_country_biblio2 == "spain"] <- "ESP"
AllData$Code[AllData$author_country_biblio2 == "sri lanka"] <- "LKA"
AllData$Code[AllData$author_country_biblio2 == "sweden"] <- "SWE"
AllData$Code[AllData$author_country_biblio2 == "tajikistan"] <- "TJK"
AllData$Code[AllData$author_country_biblio2 == "turkey"] <- "TUR"
AllData$Code[AllData$author_country_biblio2 == "UK"] <- "GBR"







AllData<-write_csv(AllData, "./data_clean/all_data_analysis.csv")


rm(CountryData,RsrchAreas,all_first,all_data,all_data_analysis)

