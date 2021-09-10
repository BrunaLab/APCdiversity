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

CountryData <- read_csv("data_raw/CLASS.csv")
CountryData <- CountryData[-1,]
CountryData <- CountryData %>%
  select(Code,Region, IncomeGroup = `Income group`) %>% 
  filter(Region=="East Asia & Pacific"|
           Region=="Europe & Central Asia"|
           Region=="Latin America & Caribbean"|
           Region== "Middle East & North Africa"|
           Region== "North America"|
           Region=="South Asia"|
           Region== "Sub-Saharan Africa")
CountryData$Region <- as.factor(CountryData$Region)
CountryData$Region <- droplevels(CountryData$Region)
levels(CountryData$Region)
write_csv(CountryData, "./data_clean/CountryData.csv")


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
write_csv(WaiverCountries, "./data_clean/WaiverCountries.csv")

WaiverCountries
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
write_csv(NON_wavierCountries, "./data_clean/NON_WavierCountries.csv")

################################################################
# Load the data on stipends
################################################################
stipends <- read_csv(file = "./data_raw/stipends.csv")
# stipends$stipend_national<-round(stipends$stipend_national,0)
# stipends$stipend_USD<-round(stipends$stipend_USD,0)
write_csv(stipends, "./data_clean/stipends.csv")

# remove from memory
rm(stipends,WaiverCountries,NON_wavierCountries)



######################################################################
# Load and process WOS data with refsplitr
# library(devtools)
# devtools::install_github("ropensci/refsplitr")
library(refsplitr)
WOS_references <- references_read(data = "./data_raw/raw_data_wos_july2020", dir = TRUE, include_all = FALSE)
write_csv(WOS_references, "./output/WOS_references.csv")
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

write_csv(WOS, "./output/WOS_july2020_step1.csv")
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
write_csv(WOS_refined, "./output/WOS_refined.csv")
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


write_csv(WOS, "./output/WOS_july2020.csv")
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

write_csv(scopus, "./output/scopus.csv")



