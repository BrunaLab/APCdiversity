# =============================================================================================================#
# Script created by Emilio M. Bruna (embruna@ufl.edu) to import and prepare
# WOS data with package `refsplitr` for the analyses in:
# PAPER CITATION TO BE ADDED
# Script created in  R version 3.6.3
# Uses packages tidyverse_1.3.0 , countrycode_1.1.3
# =============================================================================================================#

# load the libraries
library(tidyverse)
library(countrycode)
# library(devtools)
# devtools::install_github("ropensci/refsplitr")
library(refsplitr)

################################################################
# Load the list of Pawall Journals and their OA mirrors
################################################################
MirrorPairs <- read_csv(file = "./data_raw/MirrorPairs.csv")

######################################################################
# Load and process WOS data with refsplitr
WOS_references <- references_read(data = "./data_raw/raw_data_wos_july2020", dir = TRUE, include_all = FALSE)
write_csv(WOS_references, "./output/WOS_references.csv")
# WOS_references<-read_csv('./output/WOS_references.csv')
# WOS_references<-select(WOS_references,-X1)
WOS <- WOS_references
########################################
# CLEANING
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


# need to correct any missing DOIs
WOS <- WOS %>% replace_na(list(DI = "missing_doi_"))
WOS$DI <- ifelse(WOS$DI == "missing_doi_",
                 paste("missing_doi_", WOS$refID, sep = ""), WOS$DI)

# when you do check search for dupe DOI, you can see that the
# duplicate DOIs are for different articles
# so you need to change one to something else: add _dupe to it
WOS$DI <- ifelse(duplicated(WOS$DI), paste(WOS$DI, "_dupe", sep = ""), WOS$DI)
summary(as.factor(WOS$DI))


# NOW CLEAN AND DISAMBIGUATE
# THIS TAKES HOURS
# WOS_cln<-authors_clean(WOS)
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

###################################
# ADD PACKAGE NAME,database, AND PAIR_KEY
WOS$package <- "refsplitr"
WOS$db <- "wos"
WOS <- left_join(WOS, MirrorPairs, by = "SO")
missing_pk <- WOS %>% filter(is.na(pair_key))
titles_needing_pairkey <- as.data.frame(summary(as.factor(missing_pk$SO)))
titles_needing_pairkey

# ADDING PAIR KEYS


WOS$SO <- gsub("-X", ": X", WOS$SO)
WOS$SO[WOS$SO == "WATER RESEARCH X"] <- "WATER RESEARCH: X"

WOS$pair_key[WOS$SO == "WATER RESEARCH: X"] <- 39
WOS$journal_cat[WOS$SO == "WATER RESEARCH: X"] <- "OA"

WOS$pair_key[WOS$SO == "ATMOSPHERIC ENVIRONMENT: X"] <- 3
WOS$journal_cat[WOS$SO == "ATMOSPHERIC ENVIRONMENT: X"] <- "OA"

WOS$pair_key[WOS$SO == "FOOD CHEMISTRY: X"] <- 18
WOS$journal_cat[WOS$SO == "FOOD CHEMISTRY: X"] <- "OA"

WOS$pair_key[WOS$SO == "INTERNATIONAL JOURNAL OF PHARMACEUTICS: X"] <- 20
WOS$journal_cat[WOS$SO == "INTERNATIONAL JOURNAL OF PHARMACEUTICS: X"] <- "OA"


WOS$pair_key[WOS$SO == "BIOSENSORS & BIOELECTRONICS"] <- 5
WOS$journal_cat[WOS$SO == "BIOSENSORS & BIOELECTRONICS"] <- "PW"
WOS$SO[WOS$SO == "BIOSENSORS & BIOELECTRONICS"] <- "BIOSENSORS AND BIOELECTRONICS"

WOS$SO[WOS$SO == "CHAOS SOLITONS & FRACTALS"] <- "CHAOS SOLITONS AND FRACTALS"
WOS$SO[WOS$SO == "CHAOS, SOLITONS & FRACTALS"] <- "CHAOS SOLITONS AND FRACTALS"
WOS$SO[WOS$SO == "CHAOS, SOLITONS AND FRACTALS"] <- "CHAOS SOLITONS AND FRACTALS"
WOS$pair_key[WOS$SO == "CHAOS SOLITONS AND FRACTALS"] <- 6
WOS$journal_cat[WOS$SO == "CHAOS SOLITONS AND FRACTALS"] <- "PW"



WOS$pair_key[WOS$SO == "DIABETES & METABOLISM"] <- 13
WOS$journal_cat[WOS$SO == "DIABETES & METABOLISM"] <- "PW"
WOS$SO[WOS$SO == "DIABETES & METABOLISM"] <- "DIABETES AND METABOLISM"
WOS$pair_key[WOS$SO == "DIABETES AND METABOLISM"] <- 13
WOS$journal_cat[WOS$SO == "DIABETES AND METABOLISM"] <- "PW"

WOS$pair_key[WOS$SO == "EUROPEAN JOURNAL OF OBSTETRICS & GYNECOLOGY AND
             REPRODUCTIVE BIOLOGY"] <- 16
WOS$journal_cat[WOS$SO == "EUROPEAN JOURNAL OF OBSTETRICS & GYNECOLOGY AND
                REPRODUCTIVE BIOLOGY"] <- "PW"

WOS$pair_key[WOS$SO == "RESOURCES CONSERVATION AND RECYCLING"] <- 41
WOS$journal_cat[WOS$SO == "RESOURCES CONSERVATION AND RECYCLING"] <- "PW"
WOS$SO[WOS$SO == "RESOURCES, CONSERVATION AND RECYCLING"] <- "PW"


WOS$pair_key[WOS$SO == "CHEMICAL ENGINEERING SCIENCE"] <- 7
WOS$journal_cat[WOS$SO == "CHEMICAL ENGINEERING SCIENCE"] <- "PW"

levels(as.factor(WOS$SO))
levels(as.factor(WOS$pair_key))
# foo<-WOS %>% group_by(SO,journal_cat) %>% summarize(n())
write.csv(WOS, "./output/WOS_july2020.csv")
# WOS<-read_csv("./output/WOS_july2020.csv")
# WOS<-select(WOS,-X1)
