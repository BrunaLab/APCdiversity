# =============================================================================================================#
# Script created by Emilio M. Bruna (embruna@ufl.edu) to import and prepare
# data for the analyses present in : PAPER CITATION TO BE ADDED
# Script created in  R version 3.6.3 (2020-02-29)
# Uses packages bibliometrix_3.0.0 , tidyverse_1.3.0 , countrycode_1.1.3
# =============================================================================================================#

# the libraries
library(tidyverse)
library(bibliometrix)
library(countrycode)
library(tictoc)



################################################################
# load and add World Bank data on national income categories
CountryData <- read.csv("data_raw/CLASS.csv", header = TRUE)
CountryData <- CountryData[-1, ]
CountryData <- CountryData %>%
  select(Code, Region, IncomeGroup = Income.group) %>%
  filter(Region == "East Asia & Pacific" |
    Region == "Europe & Central Asia" |
    Region == "Latin America & Caribbean" |
    Region == "Middle East & North Africa" |
    Region == "North America" |
    Region == "South Asia" |
    Region == "Sub-Saharan Africa")
# CountryData$Region <-droplevels(CountryData$Region)

################################################################
# Load the list of Pawall Journals and their OA mirrors
################################################################
MirrorPairs <- read_csv(file = "./data_raw/MirrorPairs.csv")

################################################################
# Load the list of Waiver Countries
################################################################
WaiverCountries <- read_csv(file = "./data_raw/ElsevierWaivers.csv")

# add to the waiver
WaiverCountries$Code <- countrycode(WaiverCountries$Country,
  "country.name",
  "iso3c",
  warn = TRUE
)

WaiverCountries <- left_join(WaiverCountries, CountryData, by = "Code")


# Note: Kosovo is not listed as an ISO standard country. 'XKX' is the unofficial code
# used by the European Commission and others until Kosovo is assigned an ISO code.

WaiverCountries$Code[WaiverCountries$Country == "Kosovo"] <- "XKX"
WaiverCountries$Region[WaiverCountries$Country == "Kosovo"] <- "Europe & Central Asia"
WaiverCountries$IncomeGroup[WaiverCountries$Country == "Kosovo"] <- "Upper middle income"

WaiverCountries <- WaiverCountries %>% select(-notes)
WaiverCountries$Country <- gsub("Micronesia (Federated States of)", "Federated States of Micronesia", WaiverCountries$Country)
WaiverCountries$Country <- gsub("Myanmar (Burma)", "Myanmar", WaiverCountries$Country)
WaiverCountries$WaiverGroup <- gsub("GroupA_FreeAccess", "GroupA", WaiverCountries$WaiverGroup)
WaiverCountries$WaiverGroup <- gsub("Group_B_LowCostAccess", "GroupB", WaiverCountries$WaiverGroup)
# save the df as .csv file in "data_clean" folder


write.csv(WaiverCountries, "./data_clean/WaiverCountries.csv", row.names = FALSE)
# remove from the environment

# Now which ones aren't covered by waivers'

NON_wavierCountries <- anti_join(CountryData, WaiverCountries, by = "Code") %>%
  arrange(IncomeGroup, Region)

NON_wavierCountries$Country <- countrycode(NON_wavierCountries$Code,
  "iso3c", "country.name",
  warn = TRUE
)

NON_wavierCountries$Country[NON_wavierCountries$Country == "Channel Islands"] <- "CHI"
NON_wavierCountries$Region[NON_wavierCountries$Code == "CHI"] <- "Europe & Central Asia"
NON_wavierCountries$IncomeGroup[NON_wavierCountries$Code == "CHI"] <- "High income"

write.csv(NON_wavierCountries, "./data_clean/NON_WavierCountries.csv", row.names = FALSE)
################################################################
# Load the data on stipends
################################################################
stipends <- read_csv(file = "./data_raw/stipends.csv")
# stipends$stipend_national<-round(stipends$stipend_national,0)
# stipends$stipend_USD<-round(stipends$stipend_USD,0)
write.csv(stipends, "./data_clean/stipends.csv", row.names = FALSE)




######################################################################
# Load and process WOS data with refsplitr
# library(devtools)
# devtools::install_github("ropensci/refsplitr")
library(refsplitr)
WOS_references <- references_read(data = "./data_raw/raw_data_wos_july2020", dir = TRUE, include_all = FALSE)
WOS <- WOS_references

########################################
# WOS CLEANING
########################################
# are there any duplicated DOIs, TI in WOS data?
# TITLES
DupeCount_wos <- WOS %>%
  group_by(TI) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(n > 1)
# This will tell you which articles(TI) are duplicated. After looking at
# the df to be sure, delete one of the duplicates.
WOS <- WOS %>% filter(refID != 6780)
WOS <- WOS %>% filter(refID != 11212)
WOS <- WOS %>% filter(refID != 31310)

# now looking for duplicate DOIs
DupeCount_wos <- WOS %>%
  group_by(DI) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(n > 1)
# look these over
dupes_wos <- WOS %>%
  select(refID, DI, TI) %>%
  filter(DI %in% DupeCount_wos$DI) %>%
  arrange(DI, TI)

# when you do, you can see that the duplicate DOIs are for different articles
# so you need to change one to something else: add _dupe to it
WOS$DI <- ifelse(duplicated(WOS$DI), paste(WOS$DI, "_dupe", sep = ""), WOS$DI)
# also need to correct any missing DOIs
WOS <- WOS %>% replace_na(list(DI = "missing_doi_"))
WOS$DI <- ifelse(WOS$DI == "missing_doi_", paste("missing_doi_", WOS$refID, sep = ""), WOS$DI)

# EDIT JOURNAL TITLES
WOS$SO <- gsub("-X", ": X", WOS$SO)
WOS$SO <- gsub("WATER RESEARCH X", "WATER RESEARCH: X", WOS$SO)
# levels(as.factor(WOS$SO))

write.csv(WOS, "./output/WOS_july2020_step1.csv")
# WOS<-read_csv("./output/WOS_july2020.csv")
# WOS<-select(WOS,-X1)

head(WOS, 10)
colnames(WOS)
WOS_cln <- authors_clean(WOS)
WOS$AU <- with(WOS, ifelse(is.na(AU), CA, AU))
WOS$AF <- with(WOS, ifelse(is.na(AF), CA, AF))
WOS_cln <- authors_clean(WOS)
# # Now save the preliminary disambiguation as a csv file
write_csv(WOS_cln$prelim, "./output/WOS_prelim.csv")
# # load to show (and avoid having to run every time)
# WOS_cln_prelim<-read.csv("./output/WOS_prelim.csv")
# # save the names suggested for review as a csv file
write_csv(WOS_cln$review, "./output/WOS_review.csv")
######################
# Accept the disambiguation or load / merge your corrections
WOS_refined <- authors_refine(WOS_cln$review, WOS_cln$prelim)
# save the disambiguated data set
write_csv(WOS_refined, "./data_clean/WOS_refined.csv")
# WOS_refined<-read.csv("./output/WOS_refined.csv")

toc() # 61042.004 sec elapsed (~17 hours)
###################################
tic()
# FINAL WOS CLEAN AND PREP
# ADD PACKAGE NAME AND PAIR_KEY
WOS$package <- "refsplitr"
WOS <- left_join(WOS, MirrorPairs, by = "SO")
missing_pk <- left_join(WOS, MirrorPairs, by = "SO") %>% filter(is.na(pair_key))
titles_needing_pairkey <- as.data.frame(summary(as.factor(missing_pk$SO)))
titles_needing_pairkey
WOS$pair_key[WOS$SO == "BIOSENSORS & BIOELECTRONICS"] <- 5
WOS$journal_cat[WOS$SO == "BIOSENSORS & BIOELECTRONICS"] <- "PW"
WOS$SO[WOS$SO == "BIOSENSORS & BIOELECTRONICS"] <- "BIOSENSORS AND BIOELECTRONICS"
WOS$pair_key[WOS$SO == "CHAOS SOLITONS & FRACTALS"] <- 6
WOS$journal_cat[WOS$SO == "CHAOS SOLITONS & FRACTALS"] <- "PW"
WOS$SO[WOS$SO == "CHAOS SOLITONS & FRACTALS"] <- "CHAOS, SOLITONS AND FRACTALS"
WOS$pair_key[WOS$SO == "DIABETES AND METABOLISM"] <- 13
WOS$journal_cat[WOS$SO == "DIABETES & METABOLISM"] <- "PW"
WOS$pair_key[WOS$SO == "EUROPEAN JOURNAL OF OBSTETRICS & GYNECOLOGY AND REPRODUCTIVE BIOLOGY"] <- 16
WOS$journal_cat[WOS$SO == "EUROPEAN JOURNAL OF OBSTETRICS & GYNECOLOGY AND REPRODUCTIVE BIOLOGY"] <- "PW"
WOS$pair_key[WOS$SO == "RESOURCES CONSERVATION AND RECYCLING"] <- 41
WOS$journal_cat[WOS$SO == "RESOURCES CONSERVATION AND RECYCLING"] <- "PW"
WOS$pair_key[WOS$SO == "WATER RESEARCH: X"] <- 39
WOS$journal_cat[WOS$SO == "WATER RESEARCH: X"] <- "OA"

levels(as.factor(WOS$SO))
levels(as.factor(WOS$pair_key))


write.csv(WOS, "./data_clean/WOS_july2020.csv")
# WOS<-read_csv("./output/WOS_july2020.csv")
# WOS<-select(WOS,-X1)
toc() # 6.65 secs
################################################################
# Use package 'bibliometrix' to convert these to dataframes
################################################################

######################################################################
# Load and process data with bibliometrix
# Load records from WOS
# articles_wos <- c('./data_raw/raw_data_wos/savedrecs1.txt',
#                   './data_raw/raw_data_wos/savedrecs2.txt',
#                   './data_raw/raw_data_wos/savedrecs3.txt',
#                   './data_raw/raw_data_wos/savedrecs4.txt',
#                   './data_raw/raw_data_wos/savedrecs5.txt',
#                   './data_raw/raw_data_wos/savedrecs6.txt',
#                   './data_raw/raw_data_wos/savedrecs7.txt',
#                   './data_raw/raw_data_wos/savedrecs8.txt',
#                   './data_raw/raw_data_wos/savedrecs9.txt',
#                   './data_raw/raw_data_wos/savedrecs10.txt',
#                   './data_raw/raw_data_wos/savedrecs11.txt',
#                   './data_raw/raw_data_wos/savedrecs12.txt',
#                   './data_raw/raw_data_wos/savedrecs13.txt',
#                   './data_raw/raw_data_wos/savedrecs14.txt')

tic()
# load records from SCOPUS
articles_scopus <- c(
  "./data_raw/raw_data_scopus/scopus1.bib",
  "./data_raw/raw_data_scopus/scopus2.bib",
  "./data_raw/raw_data_scopus/scopus3.bib",
  "./data_raw/raw_data_scopus/scopus4.bib",
  "./data_raw/raw_data_scopus/scopus5.bib",
  "./data_raw/raw_data_scopus/scopus6.bib",
  "./data_raw/raw_data_scopus/scopus7.bib",
  "./data_raw/raw_data_scopus/scopus8.bib",
  "./data_raw/raw_data_scopus/scopus9.bib",
  "./data_raw/raw_data_scopus/scopus10.bib",
  "./data_raw/raw_data_scopus/scopus11.bib",
  "./data_raw/raw_data_scopus/scopus12.bib",
  "./data_raw/raw_data_scopus/scopus13.bib",
  "./data_raw/raw_data_scopus/scopus14.bib",
  "./data_raw/raw_data_scopus/scopus15.bib",
  "./data_raw/raw_data_scopus/scopus16.bib",
  "./data_raw/raw_data_scopus/scopus17.bib",
  "./data_raw/raw_data_scopus/scopus18.bib",
  "./data_raw/raw_data_scopus/scopus19.bib",
  "./data_raw/raw_data_scopus/scopus20.bib",
  "./data_raw/raw_data_scopus/scopus21.bib",
  "./data_raw/raw_data_scopus/scopus22.bib",
  "./data_raw/raw_data_scopus/scopus23.bib",
  "./data_raw/raw_data_scopus/scopus24.bib",
  "./data_raw/raw_data_scopus/scopusOA.bib"
)


################################################################
# Use package 'bibliometrix' to convert these to dataframes
################################################################
# WOS dataframe
# articles_wos_df <- convert2df(articles_wos, dbsource = "wos", format = "plaintext")

# SCOPUS dataframe
articles_scopus_df <- convert2df(articles_scopus, dbsource = "scopus", format = "bibtex")

toc() # 134.8 secs


########################################
# SCOPUS CLEANING
########################################
# Adding package title and correcting the SO titles
scopus <- as_tibble(articles_scopus_df)
scopus$package <- "bibliometrix"
scopus <- left_join(scopus, MirrorPairs, by = "SO")
colnames(scopus)
missing_pk <- scopus %>% filter(is.na(pair_key))
titles_needing_pairkey <- as.data.frame(summary(as.factor(missing_pk$SO)))
titles_needing_pairkey


scopus$SO <- gsub("WATER RESEARCH X", "WATER RESEARCH: X", scopus$SO)


# Correct SO needing a Pair Key
scopus$pair_key[scopus$SO == "EUROPEAN JOURNAL OF OBSTETRICS AND GYNECOLOGY AND REPRODUCTIVE BIOLOGY: X"] <- 16
scopus$journal_cat[scopus$SO == "EUROPEAN JOURNAL OF OBSTETRICS AND GYNECOLOGY AND REPRODUCTIVE BIOLOGY: X"] <- "OA"
scopus$pair_key[scopus$SO == "RESOURCES, CONSERVATION AND RECYCLING: X"] <- 41
scopus$journal_cat[scopus$SO == "RESOURCES, CONSERVATION AND RECYCLING: X"] <- "OA"

# correct / remove the ones with pair_key==0
scopus %>%
  filter(pair_key == 0) %>%
  group_by(SO) %>%
  tally()
scopus$pair_key[scopus$SO == "COMPUTERS AND GRAPHICS (PERGAMON)"] <- 10
scopus$SO[scopus$SO == "COMPUTERS AND GRAPHICS (PERGAMON)"] <- "COMPUTERS AND GRAPHICS"
scopus$journal_cat[scopus$SO == "COMPUTERS AND GRAPHICS"] <- "PW"
scopus <- scopus %>% filter(pair_key != 0)

# Add a refID number that starts 1 after the last WOS
scopus$refID <- seq(max(WOS$refID) + 1, max(WOS$refID) + nrow(scopus), length = nrow(scopus))



# Eliminate any not article or review
scopus <- scopus %>% filter(DT == "ARTICLE" | DT == "REVIEW")
# are there any duplicated DOIs, TI in scopus data?

# CHECK AND ELIMINATE DUPLICATE TITLES & DOI
# there are >2K duplicated records, which you can tell by
# doing this
DupeCount_scopus <- scopus %>%
  group_by(DI) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(n > 1)

dupes_scopus <- scopus %>%
  select(refID, DI, TI) %>%
  filter(DI %in% DupeCount_scopus$DI) %>%
  arrange(DI, TI)

DupeCount_scopus <- scopus %>%
  group_by(DI, TI, PY, SO) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(n > 1)

# Need to remove the duplicated records
scopus <- scopus %>% distinct(DI, TI, .keep_all = TRUE)
# TITLES
DupeCount_scopus <- scopus %>%
  group_by(TI) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(n > 1)
# This will tell you which articles(TI) are duplicated. After looking at
# the df to be sure, delete one of the duplicates.

# Most of these appear to be cases where proof was published online in one of the
# mirror pairs, but the published in the other one with retraction of one doi
# but some others don't have formal retraction'
# 10.1016/j.cytox.2019.100015
# 10.1016/j.jnoncrysol.2019.03.032
# 10.1016/j.nocx.2019.100036
# 10.1016/j.ecoleng.2019.07.009
# 10.1016/j.jcp.2019.07.039
# 10.1016/j.ecmx.2019.100014
# 10.1016/j.athx.2019.100006
# 10.1016/j.watres.2018.12.001
scopus <- scopus %>% filter(DI != "10.1016/j.cytox.2019.100015")
scopus <- scopus %>% filter(DI != "10.1016/j.jnoncrysol.2019.03.032")
scopus <- scopus %>% filter(DI != "10.1016/j.nocx.2019.100036")
scopus <- scopus %>% filter(DI != "10.1016/j.ecoleng.2019.07.009")
scopus <- scopus %>% filter(DI != "10.1016/j.jcp.2019.07.039")
scopus <- scopus %>% filter(DI != "10.1016/j.ecmx.2019.100014")
scopus <- scopus %>% filter(DI != "10.1016/j.athx.2019.100006")
scopus <- scopus %>% filter(DI != "10.1016/j.watres.2018.12.001")


# now looking for duplicate DOIs
DupeCount_scopus <- scopus %>%
  group_by(DI) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(n > 1)
# look these over
dupes_scopus <- scopus %>%
  select(refID, DI, TI, package) %>%
  filter(DI %in% DupeCount_scopus$DI) %>%
  arrange(DI, TI)

# # when you do, you can see that the duplicate DOIs are for different articles
# # so you need to change one to something else: add _dupe to it
# scopus$DI<- ifelse(duplicated(scopus$DI),paste(scopus$DI,"_dupe",sep=""),scopus$DI)
# # also need to correct any missing DOIs
# scopus<-scopus %>% replace_na(list(DI = "missing_doi_"))
# scopus$DI<-ifelse(scopus$DI== "missing_doi_",paste("missing_doi_",scopus$refID,sep=""),scopus$DI)

write_csv(scopus, "./data_clean/scopus.csv")
# END HERE NOV 30











####################################
####################################
# PREP THE DATASETS FOR JOINING
####################################
####################################

# keep only the papers that are unique to scopus
unique_to_scopus <- setdiff(scopus$DI, WOS$DI)
# This will keep only thouse unique to scopus
scopus_new <- scopus %>%
  filter(DI %in% unique_to_scopus) %>%
  arrange(DI)

# to check use this: if zero, then there are
# none in common that need to be deleted
in_common <- intersect(WOS$DI, scopus_new$DI)
# write_csv(scopus_new,"./output/scopus_unique.csv")
# scopus_new<-read_csv("./output/scopus_unique.csv")












########
# HOW MANY ARTICLES PER JOURNAL?
WOS_count_df <- WOS %>% select(pair_key, SO, PY, journal_cat)
WOS_count_df$db <- "wos"
scopus_count_df <- scopus_new %>% select(pair_key, SO, PY, journal_cat)
scopus_count_df <- as_tibble(scopus_count_df)
WOS_count_df$PY <- as.numeric(WOS_count_df$PY)
scopus_count_df$db <- "scopus"
str(scopus_count_df)
str(WOS_count_df)
all <- bind_rows(scopus_count_df, WOS_count_df)


all_summary <- all %>%
  group_by(pair_key, SO, journal_cat) %>%
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
# write_csv(all_summary2,"./output/all_summary2.csv")



all_summary3 <- all %>%
  group_by(journal_cat) %>%
  tally()
# write_csv(all_summary3,"./output/all_summary3.csv")


#
# # 1. Find any DOI that scopus and wos have in common
# common_doi <- (intersect(scopus$DI, WOS$DI))
# # 2. Look at them.
# common_scopus <- scopus %>%
#    select(refID,DI,TI,package) %>%
#    filter(DI %in% common_doi) %>%
#    arrange(DI)
#
# common_wos <- WOS %>%
#    select(refID,DI,TI,package) %>%
#    filter(DI %in% common_doi) %>%
#    arrange(DI)
# common_wos$TI<-gsub("\n"," ",common_wos$TI)
#
# common_refs <- bind_rows(common_wos,common_scopus) %>% arrange(DI,TI)
# common_refs$TI <- tolower(common_refs$TI)
# common_refs_count<-common_refs %>%
#    group_by(DI,TI) %>%
#    tally() %>%
#    arrange(desc(n)) %>%
#    filter(n>1) %>%
#    select(DI)
#
# # Remove the ones that scopus and wos have in common from scopus
# scopus_new <- scopus %>%
#    filter(DI %in% common_doi) %>%
#    arrange(DI)
#
#
# # convert them to tibbles
# # wos_bx<-as_tibble(articles_wos_df)
# scopus_bx<-as_tibble(articles_scopus_df)
# write_csv(scopus_bx,"./output/scopus_bx.csv")
# # ID the ones that are already in WOS dataset that ws cleaned up with refsplitr
# # These aare DIs in scopus but not in wos
# unique_DI<-(setdiff(scopus_bx$DI, WOS$DI))
# # create a df with the pubs in scopus but not wos
# not_in_wos<-scopus_bx %>% filter(DI%in%unique_DI)
# # we are only using Reviews and Articles, so eliminate all other categories
# not_in_wos<-not_in_wos %>% filter(DT=="ARTICLE"|DT=="REVIEW")
# n_distinct(not_in_wos$DI,not_in_wos$TI)
#
# summary(as.factor(not_in_wos$SO))
# foo<-as_tibble(levels(as.factor(scopus_bx$SO)))
# foo<-as_tibble(levels(as.factor(not_in_wos$SO)))
# foo<-as_tibble(levels(as.factor(WOS$SO)))
# # not_in_wos

# Missing or incomplete journals (key pair):
#    9 (missing CLINICS AND RESEARCH IN HEPATOLOGY AND GASTROENTEROLOGY: X),
#    10 (missing OA,PW)
#    13 (missing DIABETES AND METABOLISM: X)
#    9: only 1 article published: https://www.sciencedirect.com/science/article/pii/S2590144319300022

#
# # remove missing journals
# missing_jrnls<-c(9,10,13)
# not_in_wos<-left_join(not_in_wos,MirrorPairs,by="SO")
# not_in_wos<-not_in_wos %>% filter(!pair_key%in% missing_jrnls)
# titles<-c("JOURNAL OF AGRICULTURAL AND FOOD CHEMISTRY","FOOD CHEMISTRY, FUNCTION AND ANALYSIS","COMPUTERS AND GRAPHICS (PERGAMON)")
# not_in_wos<-not_in_wos %>% filter(!SO%in% titles)
# colnames(not_in_wos)
#
#
# # remove missing journals
# missing_jrnls<-c(9,10,13)
# WOS<-WOS %>% filter(!pair_key%in% missing_jrnls)
# titles<-c("JOURNAL OF AGRICULTURAL AND FOOD CHEMISTRY","FOOD CHEMISTRY, FUNCTION AND ANALYSIS","COMPUTERS AND GRAPHICS (PERGAMON)")
# WOS<-WOS %>% filter(!SO%in% titles)


################################################################
# bind the SCOPUS and WOS dataframes together and
# remove any duplicate records, and save the resulting df
################################################################
# articles_scopus_df<-as_tibble(articles_scopus_df)
# articles_wos_df<-as_tibble(articles_wos_df)
# all_articles_df<-bind_rows(articles_scopus_df,articles_wos_df)
# write.csv(all_articles_df,"./data_clean/all_articles.csv", row.names = FALSE)
# colnames(all_articles_df)
# coverage_vols<-all_articles_df %>% select(SO,PY,VL) %>%
#    group_by(SO,PY,VL) %>% tally() %>% filter(row_number()==1 | row_number()==n())
# write.csv(coverage_vols,"./data_clean/coverage_vols.csv", row.names = FALSE)
################################################################

################################################################
# Extract First Author Country
################################################################
# AU1_CO Corresponding Author’s Country (disambiguated)
# AuthorFirst_wos <- metaTagExtraction(articles_wos_df, Field = "AU1_CO", sep = ";")
# AuthorFirst_scopus <- metaTagExtraction(articles_scopus_df, Field = "AU1_CO", sep = ";")
AuthorFirst_scopus <- metaTagExtraction(scopus_new, Field = "AU1_CO", sep = ";")

# AuthorFirst_wos<-as_tibble(AuthorFirst_wos)
AuthorFirst_scopus <- as_tibble(AuthorFirst_scopus)
# AuthorFirst_wos$AU1_CO
AuthorFirst_scopus$AU1_CO
# Bind them up, but first add database column
# AuthorFirst_scopus$database<-"scopus"
# AuthorFirst_wos$database<-"wos"
summary(as.factor(AuthorFirst_scopus$AU1_CO))
colnames(AuthorFirst_scopus)
AuthorFirst_scopus <- AuthorFirst_scopus %>% select(
  database, DI, refID, TI,
  PY, pair_key, SO, journal_cat, AU, AU_UN, AU1_CO
)

#
# AllData_AuthorFirst<-bind_rows(AuthorFirst_scopus,AuthorFirst_wos)
# # AllData_AuthorFirst<-bind_rows(AuthorFirst_scopus)
# AllData_AuthorFirst$AU1_CO
# colnames(AllData_AuthorFirst)
# summary(as.factor(AllData_AuthorFirst$DT))
# AllData_AuthorFirst<-AllData_AuthorFirst %>% filter(DT=="ARTICLE"|DT=="NOTE"|DT=="REVIEW")
# AllData_AuthorFirst<-AllData_AuthorFirst %>% select(database,DI,TI,
#                                                     PY,SO,AU,AU_UN,AU1_CO)
# AllData_AuthorFirst<-left_join(AllData_AuthorFirst,MirrorPairs,by="SO")
# AllData_AuthorFirst<-select(AllData_AuthorFirst,-notes) #remove notes column
# AllData_AuthorFirst<-droplevels(AllData_AuthorFirst)
# # AllData_AuthorFirst$AuthorNum<-1
# colnames(AllData_AuthorFirst)
# summary(as.factor(AllData_AuthorFirst$AU1_CO))
#
# journalsNAauthors<-AllData_AuthorFirst %>%
#    filter(is.na(AU1_CO)) %>%
#    select(journal_cat,SO,database) %>%
#    group_by(journal_cat,SO,database) %>%
#    tally()
# write.csv(AllData_AuthorFirst,"./data_clean/AllData_AuthorFirst.csv", row.names = FALSE)
#
# AllData_AuthorFirst<-AllData_AuthorFirst %>% distinct(TI,AU1_CO, .keep_all = TRUE)
#
# write.csv(journalsNAauthors,"./output/journalsNAauthors.csv", row.names = FALSE)


################################################################
# First Extract each author countries
# (the last column of the processed df)
# https://cran.r-project.org/web/packages/rscopus/vignettes/multi_author.html
# for some reason it is necessary to extract country data from
# SCOPUS and WOS files independently, then merge. That's why can't use:
# AuGeoAll <- metaTagExtraction(all_articles_df, Field = "AU_CO", sep = ";")
# AuGeoAll$AU_CO

# AuGeo_wos <- metaTagExtraction(articles_wos_df, Field = "AU_CO", sep = ";")
# AuGeo_wos$AU_CO

# AuGeo_scopus <- metaTagExtraction(articles_scopus_df, Field = "AU_CO", sep = ";")
AuGeo_scopus <- metaTagExtraction(scopus_new, Field = "AU_CO", sep = ";")
AuGeo_scopus$AU_CO
# AuGeo_scopus$database<-"scopus"

# AuGeo_wos$database<-"wos"

AuGeo_scopus <- as_tibble(AuGeo_scopus)
# AuGeo_wos<-as_tibble(AuGeo_wos)

# AllData_AuGeo<-bind_rows(AuGeo_scopus,AuGeo_wos)
# AllData_AuGeo$AU_CO
colnames(AuGeo_scopus)

AllData_AuGeo <- AuGeo_scopus %>% select(
  database, DI, refID, TI,
  PY, pair_key, SO, journal_cat,
  AU, AU_UN, AU1_UN, AU_CO
)
# write.csv(AllData_AuGeo,"./data_clean/AllData_AuGeo.csv", row.names = FALSE)
# Add pair_key (id no. for a mirror pair) & journal type (OA or PW)
# AllData_AuGeo<-left_join(AllData_AuGeo,MirrorPairs,by="SO")
# AllData_AuGeo<-select(AllData_AuGeo,-notes) #remove notes column
# AllData_AuGeo<-droplevels(AllData_AuGeo)
# colnames(AllData_AuGeo)
# AllData_AuGeo<-as_tibble(AllData_AuGeo)



colnames(WOS)
colnames(WOS_refined)
WOS_refined$author_order
WOS_refined$country
WOS_refined$address
WOS_refined <- as_tibble(WOS_refined)
WOS_refined$database <- "wos"


WOS_to_get <- WOS %>%
  select(refID, pair_key, TI, DI, journal_cat)
WOS_refined2 <- left_join(WOS_refined, WOS_to_get)
AuthorFirst_scopus$author_order <- 1
AuthorFirst_scopus <- AuthorFirst_scopus %>% rename("country" = "AU1_CO")

all <- bind_rows(WOS_refined2, AuthorFirst_scopus) %>% select(-groupID, -OI, -UT, -RI, -PU, -PT)

# Remove journals for whihc only have data from one of the pairs.
# 9: "Clinics and Research in Hepatology and
# Gastroenterology: X"
# 13: Diabetes and Metabolism: X was discontinued
# 10: none for Graphics and Visual computing

missing_jrnls <- c(9, 10, 13)
all <- all %>% filter(!pair_key %in% missing_jrnls)



all$code <-
  countrycode(all$country, "country.name", "iso3c", warn = TRUE)
all1 <- all %>% filter(author_order == 1)
all1_summary <- all1 %>%
  group_by(code, journal_cat) %>%
  tally() %>%
  arrange(journal_cat, desc(n), code)
all1_summary

# write.csv(all1_summary,"./output/all1_summary.csv", row.names = FALSE)


colnames(AllData_AuGeo)
AllData_AuGeo <- as_tibble(AllData_AuGeo)



# The countries of all authors of an article are in one cell.
# this section splits them into multiple columns, then converts
# the df from wide to long form.
tempDF <- as.data.frame(str_split(AllData_AuGeo$AU_CO, ";", simplify = TRUE))
tempDF <- tempDF %>% mutate_all(na_if, "") # replace the blanks with NA
# Need to do this next step or 'gather' won't work properly
tempDF <- data.frame(lapply(tempDF, as.character), stringsAsFactors = FALSE)
# bind the new dataframe of countries in wide form to original
AllData_AuGeo <- cbind(AllData_AuGeo, tempDF)
rm(tempDF) # remove the tempdf from environment
# gather into long form
colnames(AllData_AuGeo)
AllData_AuGeo <- AllData_AuGeo %>%
  gather(CountryNum, country, V1:ncol(AllData_AuGeo))
# remove the 'V' from cells in author column
# This also adds the order of authors for each paper
AllData_AuGeo$CountryNum <- gsub("V", "", AllData_AuGeo$CountryNum)
AllData_AuGeo$CountryNum <- as.numeric(AllData_AuGeo$CountryNum)
# head(AllData,10)
# remove the NA rows of country column
AllData_AuGeo <- AllData_AuGeo %>% drop_na("country")
# organize the df by article, with authors in order from 1...N
AllData_AuGeo <- AllData_AuGeo %>% arrange(DI, TI, CountryNum)
AllData_AuGeo$DI <- as.factor(AllData_AuGeo$DI)
# delete the column with all countries in a single cell
AllData_AuGeo$AU_CO <- NULL
# write.csv(AllData_AuGeo,"./data_clean/AllData_AuGeo.csv", row.names = FALSE)


# are all the countries for an article the same?
unique_countries_scopus <- AllData_AuGeo %>%
  group_by(DI, TI) %>%
  summarize(n_countries = n_distinct(country)) %>%
  arrange(desc(n_countries))
nrow(unique_countries_scopus)
# n_countries=1, that means that country is the lead author
one_country_scopus <- unique_countries_scopus %>%
  filter(n_countries == 1) %>%
  select(-n_countries)

multi_country_scopus <- unique_countries_scopus %>%
  filter(n_countries > 1) %>%
  select(-n_countries)

nrow(multi_country_scopus) + nrow(one_country_scopus)
nrow(scopus_new)

Scopus_1st <- semi_join(AllData_AuGeo, one_country_scopus)
nrow(one_country_scopus) / (nrow(multi_country_scopus) + nrow(one_country_scopus)) # 73% of them whew!!!!
Scopus_1st$status1st <- "all_au_same_country"
n_distinct(AllData_AuGeo$DI)
n_distinct(one_country_scopus$DI)
n_distinct(Scopus_1st$DI)

Scopus_Verify1st <- semi_join(AllData_AuGeo, multi_country_scopus)
nrow(Scopus_Verify1st) # 70% of them whew!!!!
Scopus_Verify1st$status1st <- "au_diff_country"

n_distinct(AllData_AuGeo$DI)
n_distinct(multi_country_scopus$DI)
n_distinct(Scopus_Verify1st$DI)

Scopus_AuGeo <- bind_rows(Scopus_1st, Scopus_Verify1st)
Scopus_AuGeo$package <- "bibliometrix"
colnames(Scopus_AuGeo)
Scopus_AuGeo_slim <- Scopus_AuGeo %>% select(
  database, DI, refID, TI, PY, pair_key, SO, journal_cat,
  CountryNum, country, status1st, package
)

AuthorFirst_scopus_slim <- AuthorFirst_scopus %>% select(DI, TI, PY, SO, AU1_CO)
AuthorFirst_scopus_slim$AU1_CO_no <- 1
AuthorFirst_scopus_slim$package <- "bibliometrix"

scopus_1st_comparison <- full_join(AuthorFirst_scopus_slim, Scopus_AuGeo_slim) %>%
  filter(CountryNum == 1)
scopus_1st_comparison$test <- scopus_1st_comparison$AU1_CO == scopus_1st_comparison$country
scopus_1st_comparison
MISMATCH <- scopus_1st_comparison %>%
  group_by(SO, status1st, test) %>%
  tally()

WOS$package <- "refsplitr"
WOS_refined$package <- "refsplitr"
colnames(WOS)
WOS_slim <- WOS %>% select(DI, TI, pair_key, SO, journal_cat, PY, refID, package)
colnames(WOS_refined)
WOS_refined_slim <- WOS_refined %>%
  filter(author_order == 1) %>%
  select(refID, PY, author_order, author_name, package)
WOS_join <- full_join(WOS_slim, WOS_refined_slim)
WOS_join <- as_tibble(WOS_join)
first_authors <- bind_rows(scopus_1st_comparison, WOS_join)
colnames(first_authors)
coverage1 <- first_authors %>%
  group_by(pair_key, PY, SO, package) %>%
  tally()
# write.csv(coverage1,"./output/coverage1.csv", row.names = FALSE)
coverage2 <- first_authors %>%
  group_by(pair_key, PY, SO) %>%
  tally()
# write.csv(coverage2,"./output/coverage2.csv", row.names = FALSE)
coverage3 <- first_authors %>%
  group_by(pair_key, SO) %>%
  tally()
# write.csv(coverage3,"./output/coverage3.csv", row.names = FALSE)

summary(as.factor(first_authors$package))

#############
# EXTRACT CELLS OF INTEREST
#############
# # AU_UN Author’s Affiliations (disambiguated)
# Affiliations_wos <- metaTagExtraction(articles_wos_df, Field = "AU_UN", sep = ";")
# Affiliations_scopus <- metaTagExtraction(articles_scopus_df, Field = "AU_UN", sep = ";")
# Affiliations_wos$AU_UN
# Affiliations_scopus$AU_UN
# Affiliations_scopus$database<-"scopus"
# Affiliations_wos$database<-"wos"
#
# ##############################################
# # AUTHORS
# ##############################################
# articles_scopus_df$database<-"scopus"
# articles_wos_df$database<-"wos"
# AllData_Authors<-bind_rows(articles_scopus_df,articles_wos_df)
# AllData_Authors$AU
#
# AllData_Authors<-AllData_Authors %>% select(database,DI,TI,
#                                                 PY,SO,AU)
# write.csv(AllData_Authors,"./data_clean/AllData_Authors.csv", row.names = FALSE)
#
# AllData_Authors<-left_join(AllData_Authors,MirrorPairs,by="SO")
# AllData_Authors<-select(AllData_Authors,-notes) #remove notes column
# AllData_Authors<-droplevels(AllData_Authors)
# colnames(AllData_Authors)
#
#
# # The affiliations of all authors of an article are in one cell.
# # this section splits them into multiple columns, then converts
# # the df from wide to long form.
# tempDF<- as.data.frame(str_split(AllData_Authors$AU, ";", simplify = TRUE))
# tempDF <- tempDF %>% mutate_all(na_if,"")  #replace the blanks with NA
# # Need to do this next step or 'gather' won't work properly
# tempDF <- data.frame(lapply(tempDF, as.character), stringsAsFactors=FALSE)
# #bind the new dataframe of countries in wide form to original
# AllData_Authors<-cbind(AllData_Authors,tempDF)
# rm(tempDF) #remove the tempdf from environment
# # gather into long form
# colnames(AllData_Authors)
# AllData_Authors<-AllData_Authors %>%
#    gather(AuthorNum,author_name,V1:ncol(AllData_Authors))
# # remove the 'V' from cells in author column
# # This also adds the order of authors for each paper
# AllData_Authors$AuthorNum<-gsub("V","",AllData_Authors$AuthorNum)
# AllData_Authors$AuthorNum<-as.numeric(AllData_Authors$AuthorNum)
# # head(AllData,10)
# # remove the NA rows of country column
# AllData_Authors<-AllData_Authors %>%  drop_na("author_name")
# # organize the df by article, with authors in order from 1...N
# AllData_Authors<-AllData_Authors %>% arrange(TI,AuthorNum)
#
#
# AllData_Authors$DI<-as.factor(AllData_Authors$DI)
# # delete the column with all countries in a single cell
# AllData_Authors$AU<-NULL
# write.csv(AllData_CorrAffil,"./data_clean/AllData_CorrAffil.csv", row.names = FALSE)
#
#
# ##############################################
# # AUTHOR AFFILIATIONS - ALL AUTHORS
# ##############################################
# AllData_Affiliations<-bind_rows(Affiliations_scopus,Affiliations_wos)
# AllData_Affiliations$AU_UN
#
# AllData_Affiliations<-AllData_Affiliations %>% select(database,DI,TI,
#                                                       PY,SO,AU,AU_UN,AU1_UN)
# write.csv(AllData_Affiliations,"./data_clean/AllData_Affiliations.csv", row.names = FALSE)
#
# AllData_Affiliations<-left_join(AllData_Affiliations,MirrorPairs,by="SO")
# AllData_Affiliations<-select(AllData_Affiliations,-notes) #remove notes column
# AllData_Affiliations<-droplevels(AllData_Affiliations)
# colnames(AllData_Affiliations)
# # The affiliations of all authors of an article are in one cell.
# # this section splits them into multiple columns, then converts
# # the df from wide to long form.
# tempDF<- as.data.frame(str_split(AllData_Affiliations$AU_UN, ";", simplify = TRUE))
# tempDF <- tempDF %>% mutate_all(na_if,"")  #replace the blanks with NA
# # Need to do this next step or 'gather' won't work properly
# tempDF <- data.frame(lapply(tempDF, as.character), stringsAsFactors=FALSE)
# #bind the new dataframe of countries in wide form to original
# AllData_Affiliations<-cbind(AllData_Affiliations,tempDF)
# rm(tempDF) #remove the tempdf from environment
# # gather into long form
# colnames(AllData_Affiliations)
# AllData_Affiliations<-AllData_Affiliations %>%
#    gather(AffiliationNum,Inst,V1:ncol(AllData_Affiliations))
# # remove the 'V' from cells in author column
# # This also adds the order of authors for each paper
# AllData_Affiliations$AffiliationNum<-gsub("V","",AllData_Affiliations$AffiliationNum)
# AllData_Affiliations$AffiliationNum<-as.numeric(AllData_Affiliations$AffiliationNum)
# # head(AllData,10)
# AllData_Affiliations<-AllData_Affiliations %>%  drop_na("Inst")
# # organize the df by article, with authors in order from 1...N
# AllData_Affiliations<-AllData_Affiliations %>% arrange(TI,AffiliationNum)
#
# # remove the NA rows of country column
# AllData_Affiliations$DI<-as.factor(AllData_Affiliations$DI)
# # delete the column with all countries in a single cell
# AllData_Affiliations$AU_UN<-NULL
# write.csv(AllData_Affiliations,"./data_clean/AllData_Affiliations.csv", row.names = FALSE)

##############################################
# AUTHOR AFFILIATIONS - CORRESPONDING AUTHORS
##############################################
Affiliations_wos <- metaTagExtraction(articles_wos_df, Field = "AU_UN", sep = ";")
Affiliations_scopus <- metaTagExtraction(articles_scopus_df, Field = "AU_UN", sep = ";")

Affiliations_wos <- as_tibble(Affiliations_scopus)
Affiliations_wos$database <- "wos"
Affiliations_scopus <- as_tibble(Affiliations_scopus)
Affiliations_scopus$database <- "scopus"
AllData_CorrAffil <- bind_rows(Affiliations_scopus, Affiliations_wos)
AllData_CorrAffil$AU1_UN

AllData_CorrAffil <- AllData_CorrAffil %>% select(
  database, DI, TI,
  PY, SO, AU, AU1_UN
)
# write.csv(AllData_CorrAffil,"./data_clean/AllData_CorrAffil.csv", row.names = FALSE)

AllData_CorrAffil <- left_join(AllData_CorrAffil, MirrorPairs, by = "SO")
AllData_CorrAffil <- select(AllData_CorrAffil, -notes) # remove notes column
AllData_CorrAffil <- droplevels(AllData_CorrAffil)
colnames(AllData_CorrAffil)


# The affiliations of all authors of an article are in one cell.
# this section splits them into multiple columns, then converts
# the df from wide to long form.
tempDF <- as.data.frame(str_split(AllData_CorrAffil$AU1_UN, ";", simplify = TRUE))
tempDF <- tempDF %>% mutate_all(na_if, "") # replace the blanks with NA
# Need to do this next step or 'gather' won't work properly
tempDF <- data.frame(lapply(tempDF, as.character), stringsAsFactors = FALSE)
# bind the new dataframe of countries in wide form to original
AllData_CorrAffil <- cbind(AllData_CorrAffil, tempDF)
rm(tempDF) # remove the tempdf from environment
# gather into long form
colnames(AllData_CorrAffil)
AllData_CorrAffil <- AllData_CorrAffil %>%
  gather(author, corr_affil, V1:ncol(AllData_CorrAffil))
# remove the 'V' from cells in author column
# This also adds the order of authors for each paper
AllData_CorrAffil$author <- gsub("V", "", AllData_CorrAffil$author)
AllData_CorrAffil$author <- as.numeric(AllData_CorrAffil$author)
# head(AllData,10)

# AUTHOR IS MEANINGLESS, the order in the bibliometrix dataframe is NOT
# NECESSARILY THE ORDER OF AUTHORSHIP IN THE PAPER
AllData_CorrAffil <- AllData_CorrAffil %>%
  select(-author) %>%
  filter(corr_affil != "NOTREPORTED")

# organize the df by article, with authors in order from 1...N
AllData_CorrAffil <- AllData_CorrAffil %>% arrange(TI, SO, corr_affil)

# remove the NA rows of country column
AllData_CorrAffil <- AllData_CorrAffil %>% drop_na("corr_affil")
AllData_CorrAffil$DI <- as.factor(AllData_CorrAffil$DI)
# delete the column with all countries in a single cell
AllData_CorrAffil$AU1_UN <- NULL
# write.csv(AllData_CorrAffil,"./data_clean/AllData_CorrAffil.csv", row.names = FALSE)


####################
view(AllData_AuthorFirst)
# view(AllData_Affiliations)
view(AllData_AuGeo)
view(AllData_CorrAffil)
###################
colnames(AllData_AuthorFirst)
# colnames(AllData_Affiliations)
colnames(AllData_AuGeo)
colnames(AllData_CorrAffil)




#####################
# Bind up 1st author df and all author df
# this will allow you to see how often the first author is the same as
# first country when extracting all authors

AllData <- full_join(AllData_AuGeo, AllData_AuthorFirst,
  by = c(
    "database", "DI", "TI", "PY", "SO", "AU", "AU_UN",
    "pair_key", "journal_cat", "apc", "waiver"
  )
)

AllData <- full_join(AllData, AllData_CorrAffil,
  by = c(
    "database", "DI", "TI", "PY", "SO", "AU",
    "pair_key", "journal_cat", "apc", "waiver"
  )
)


AllData$country_check <- (AllData$AU1_CO == AllData$country)
country_check_summary <- AllData %>%
  filter(CountryNum == 1) %>%
  group_by(database, country_check) %>%
  tally()
#################
# # remove missing journals
# # missing_jrnls<-c(9,13,16)
# missing_jrnls<-c(9,13,16)
# AllData<-AllData %>% filter(!pair_key%in% missing_jrnls)

colnames(AllData)
AllData$country <- tolower(AllData$country)
AllData$AU1_CO <- tolower(AllData$AU1_CO)

AllData$country <- gsub("england", "UK", AllData$country)
AllData$country <- gsub("north ireland", "UK", AllData$country)
AllData$country <- gsub("papua n guinea", "PNG", AllData$country)
AllData$country <- gsub("scotland", "UK", AllData$country)
AllData$country <- gsub("wales", "UK", AllData$country)

AllData$AU1_CO <- gsub("england", "UK", AllData$AU1_CO)
AllData$AU1_CO <- gsub("north ireland", "UK", AllData$AU1_CO)
AllData$AU1_CO <- gsub("papua n guinea", "PNG", AllData$AU1_CO)
AllData$AU1_CO <- gsub("scotland", "UK", AllData$AU1_CO)
AllData$AU1_CO <- gsub("wales", "UK", AllData$AU1_CO)

AllData$AU1CO_Code <-
  countrycode(AllData$AU1_CO, "country.name", "iso3c", warn = TRUE)

AllData$Code <-
  countrycode(AllData$country, "country.name", "iso3c", warn = TRUE)

################################################################
# ADD DATA ON COUNTRIES REGION AND INCOME TO AllData
AllData <- merge(AllData, CountryData, by = "Code", all.x = TRUE) # merge
colnames(AllData)
# remove CountryData from the environment
rm(CountryData)

AllData$country_check <- (AllData$AU1_CO == AllData$country)
country_check_summary <- AllData %>%
  filter(CountryNum == 1) %>%
  group_by(database, country_check) %>%
  tally()



###############
###############


#################
# Bibliometrix datasets
missing_jrnls <- c(9, 13, 16)
# AllData_Authors<-read_csv("./data_clean/AllData_Authors.csv")
AllData_Authors <- left_join(AllData_Authors, MirrorPairs, by = "SO")
AllData_Authors <- select(AllData_Authors, -notes) # remove notes column
AllData_Authors <- AllData_Authors %>% filter(!pair_key %in% missing_jrnls)
#####
# AllData_CorrAffil<-read_csv("./data_clean/AllData_CorrAffil.csv")
AllData_CorrAffil <- AllData_CorrAffil %>% filter(!pair_key %in% missing_jrnls)

colnames(AllData_CorrAffil)
AllData_CorrAffil <- AllData_CorrAffil %>%
  select(database,
    DOI = DI, pair_key, Journal = SO,
    JrnlType = journal_cat, Year = PY, Title = TI, Authors = AU,
    corr_affil
  )
####
# AllData_Affiliations<-read_csv("./data_clean/AllData_Affiliations.csv")
AllData_Affiliations <- AllData_Affiliations %>% filter(!pair_key %in% missing_jrnls)

colnames(AllData_Affiliations)
AllData_Affiliations <- AllData_Affiliations %>%
  select(database,
    DOI = DI, pair_key, Journal = SO,
    JrnlType = journal_cat, Year = PY, Title = TI,
    Authors = AU, AU1_UN, AffiliationNum, Inst
  )
####
missing_jrnls <- c(9, 13, 16)
# AllData_AuthorFirst<-read_csv("./data_clean/AllData_AuthorFirst.csv")
AllData_AuthorFirst <- left_join(AllData_AuthorFirst, MirrorPairs, by = "SO")
AllData_AuthorFirst <- select(AllData_AuthorFirst, -notes) # remove notes column
AllData_AuthorFirst <- AllData_AuthorFirst %>% filter(!pair_key %in% missing_jrnls)
AllData_AuthorFirst$AuthorNum <- 1
colnames(AllData_AuthorFirst)
AllData_AuthorFirst <- AllData_AuthorFirst %>%
  select(database,
    DOI = DI, pair_key, Journal = SO,
    JrnlType = journal_cat, Year = PY, Title = TI, Authors = AU,
    Institutions = AU_UN, AuthorNum, Country = AU1_CO
  )

AllData_AuthorFirst$Country <- gsub("england", "UK", AllData_AuthorFirst$Country)
AllData_AuthorFirst$Country <- gsub("north ireland", "UK", AllData_AuthorFirst$Country)
AllData_AuthorFirst$Country <- gsub("papua n guinea", "PNG", AllData_AuthorFirst$Country)
AllData_AuthorFirst$Country <- gsub("scotland", "UK", AllData_AuthorFirst$Country)
AllData_AuthorFirst$Country <- gsub("wales", "UK", AllData_AuthorFirst$Country)

AllData_AuthorFirst$Code <-
  countrycode(AllData_AuthorFirst$Country, "country.name", "iso3c", warn = TRUE)
############
# AllData_AuGeo<-read_csv("./data_clean/AllData_AuGeo.csv")
AllData_AuGeo <- AllData_AuGeo %>% filter(!pair_key %in% missing_jrnls)
colnames(AllData_AuGeo)
AllData_AuGeo <- AllData_AuGeo %>%
  select(database,
    DOI = DI, pair_key, Journal = SO,
    JrnlType = journal_cat, Year = PY, Title = TI, Authors = AU,
    Institutions = AU_UN, Corr_Author_Inst = AU1_UN,
    Country = country
  )

AllData_AuGeo$Country <- gsub("england", "UK", AllData_AuGeo$Country)
AllData_AuGeo$Country <- gsub("north ireland", "UK", AllData_AuGeo$Country)
AllData_AuGeo$Country <- gsub("papua n guinea", "PNG", AllData_AuGeo$Country)
AllData_AuGeo$Country <- gsub("scotland", "UK", AllData_AuGeo$Country)
AllData_AuGeo$Country <- gsub("wales", "UK", AllData_AuGeo$Country)
AllData_AuGeo$Code <-
  countrycode(AllData_AuGeo$Country, "country.name", "iso3c", warn = TRUE)
##########







































# ADD FROM OTHER EXTRACTIONS
# AllData<-AllData_AuthorFirst
# AllData$AU_CO<-AuGeo$AU_CO
# AllData$key<-seq(1:nrow(AllData))
# write.csv(AllData,"./data_clean/AllData.csv", row.names = FALSE)


# remove from the environment
rm(
  all_articles_df,
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
  AllData_AuthorFirst
)



################################################################
# Add  ISO 3-digit code for each country with 'countrycode'
################################################################
# Setting "warn=TRUE" tells you of any it couldn't convert
# because of spelling mistakes, etc.
AllData$country_code <-
  countrycode(AllData$Country, "country.name", "iso3c", warn = TRUE)

# convert variables to factor
AllData$country_code <- as.factor(AllData$country_code)
AllData$journal_cat <- as.factor(AllData$journal_cat)
AllData$country <- as.factor(AllData$country)
AllData$SO <- as.factor(AllData$SO)

# rename columns
colnames(AllData)

AllData <- AllData %>%
  select(key, database,
    DOI = DI, pair_key, Journal = SO,
    JrnlType = journal_cat, Year = PY, Title = TI, Authors = AU,
    Institutions = AU_UN, Corr_Author_Inst = AU1_UN, First_Author_Country = AU1_CO,
    AuthorNum = author, Country = country, Code = country_code
  )

AllData <- AllData %>% drop_na("Code")
colnames(AllData)

################################################################
# ADD DATA ON COUNTRIES REGION AND INCOME TO AllData
AllData <- merge(AllData, CountryData, by = "Code", all.x = TRUE) # merge
# remove CountryData from the environment
rm(CountryData)

# make IncomeGroup an ordered factor (Low to High)
AllData$IncomeGroup <- ordered(AllData$IncomeGroup, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))
levels(AllData$IncomeGroup)[levels(AllData$IncomeGroup) == "High income"] <- "High"
levels(AllData$IncomeGroup)[levels(AllData$IncomeGroup) == "Low income"] <- "Low"
levels(AllData$IncomeGroup)[levels(AllData$IncomeGroup) == "Lower middle income"] <- "Lower middle"
levels(AllData$IncomeGroup)[levels(AllData$IncomeGroup) == "Upper middle income"] <- "Upper middle"
str(AllData)

# convert to factor
AllData$pair_key <- as.factor(AllData$pair_key)
AllData$pair_key <- droplevels(AllData$pair_key)
# levels(AllData$pair_key)
# There are some with missing DOI values, if you don't replace missing DOI
# they will be excluded when grouping
AllData$DOI <- as.character(AllData$DOI)
AllData$DOI <- AllData$DOI %>% replace_na("missing_DOI")
AllData$DOI <- as.factor(AllData$DOI)
AllData <- AllData %>% arrange(Title, AuthorNum)
AllData$IncomeGroup <- as.factor(AllData$IncomeGroup)
AllData$IncomeGroup <- ordered(AllData$IncomeGroup, levels = c("High", "Upper middle", "Lower middle", "Low"))
# head(AllData)

############################################################
# Remove any journal pairs for whichc data re incomplete
############################################################
# TODO: no articles for "Clinics and Research in Hepatology and
# Gastroenterology: X (9)" so exclude it and mirror
# Diabetes and Metabolism: X (13) is missing (no info on page)
# Europ. J Obsterics, Gynecology: X (16) none published)
missing_jrnls <- c(9, 13, 16)
AllData <- AllData %>% filter(!pair_key %in% missing_jrnls)
rm(missing_jrnls)

# save the csv
write.csv(AllData, "./data_clean/AllData_Clean_Bibliometrix.csv", row.names = FALSE)


################################################################
# Change column names of Mirror Pairs and save csv to 'data_clean'
################################################################
MirrorPairs <- MirrorPairs %>%
  select(Journal = SO, JrnlType = journal_cat, APC = apc, pair_key) %>%
  filter(pair_key > 0) %>%
  arrange(pair_key, JrnlType)

# save the df as .csv file in "data_clean" folder
write_csv(MirrorPairs, "./data_clean/MirrorPairs.csv")
# remove from the environment
rm(MirrorPairs)


toc()








#
#
#
# ################################################################
# # ALL DATA without papers that have a USA or CHN first or last author
#
# # ID papers with a first author in USA or CHN
# first_author_no_USA_CHN<-AllData %>%
#    group_by(TI) %>%
#    filter(AuthorNum==1 & Country!="CHINA") %>%
#    filter (AuthorNum==1 & Country!="USA") %>%
#    select(TI)
#
# # REMOVE THESE FROM THE AllData df
# NO_first_author_USA_CHN<-semi_join(AllData,first_author_no_USA_CHN) %>%
#    arrange(Journal,TI,AuthorNum)
#
# # Now find the ones in this new reduced df that have
# # china or usa as last author
# last_author_no_USA_CHN<-NO_first_author_USA_CHN %>%
#    group_by(TI) %>%
#    filter(AuthorNum == max(AuthorNum)) %>%
#    filter(Country!="CHINA") %>%
#    filter(Country!="USA") %>%
#    select(TI)
#
# # Remove them from the reduced df
# NO_USA_CHN_FL<-semi_join(NO_first_author_USA_CHN,last_author_no_USA_CHN) %>%
#    arrange(Journal,TI,AuthorNum)
#
# write.csv(NO_USA_CHN_FL,"./data_clean/NO_USA_CHN_FL.csv", row.names = FALSE)
#
#
#
# one_author_pubs <- AllData %>%
#    group_by(TI) %>%
#    summarize(n=n_distinct(AuthorNum)) %>%
#    filter(n==1) %>%
#    select(-n) %>%
#    left_join(AllData,by="TI")
# one_author_pubs$Dataset<-"All Countries"
# one_author_pubs$author<-"solo"
#
# write.csv(one_author_pubs,"./data_clean/one_author_pubs_ALL.csv", row.names = FALSE)
#
# coauthor_pubs<- AllData %>%
#    group_by(TI) %>%
#    summarize(n=n_distinct(AuthorNum)) %>%
#    filter(n>=2) %>%
#    left_join(AllData,by="TI")
# coauthor_pubs$Dataset<-"All Countries"
# coauthor_pubs$author<-"CoAuthored"
#
# write.csv(coauthor_pubs,"./data_clean/coauthor_pubs_ALL.csv", row.names = FALSE)
#
# one_author_pubsNOCHNUSA <- NO_USA_CHN_FL %>%
#    group_by(TI) %>%
#    summarize(n=n_distinct(AuthorNum)) %>%
#    filter(n==1) %>%
#    select(-n) %>%
#    left_join(NO_USA_CHN_FL,by="TI")
# one_author_pubsNOCHNUSA$Dataset<-"CHN & USA excluded"
# one_author_pubsNOCHNUSA$author<-"solo"
#
# write.csv(one_author_pubsNOCHNUSA,"./data_clean/one_author_pubsNOCHNUSA.csv", row.names = FALSE)
#
# coauthor_pubsNOCHNUSA<- NO_USA_CHN_FL %>%
#    group_by(TI) %>%
#    summarize(n=n_distinct(AuthorNum)) %>%
#    filter(n>=2) %>%
#    left_join(NO_USA_CHN_FL,by="TI")
# coauthor_pubsNOCHNUSA$Dataset<-"CHN & USA excluded"
# coauthor_pubsNOCHNUSA$author<-"CoAuthored"
#
# write.csv(coauthor_pubsNOCHNUSA,"./data_clean/coauthor_pubsNOCHNUSA.csv", row.names = FALSE)
#
