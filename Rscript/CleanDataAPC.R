#=============================================================================================================#
# Script created by Emilio M. Bruna (embruna@ufl.edu) to import and prepare
# data for the analyses present in : PAPER CITATION TO BE ADDED 
# Script created in  R version 3.6.3 (2020-02-29)
# Uses packages bibliometrix_3.0.0 , tidyverse_1.3.0 , countrycode_1.1.3
#=============================================================================================================#

# the libraries
library(tidyverse)
library(bibliometrix)
library(countrycode) 
################################################################
# Load the list of Pawall Journals and their OA mirrors 
################################################################
MirrorPairs<-read_csv(file="./data_raw/MirrorPairs.csv")

################################################################
# Load and process publication records. These were downloaded
# from Web of Science (WOS) and SCOPUS.
################################################################
# Load records from WOS 
articles_wos <- c('./data_raw/raw_data_wos/savedrecs1.txt',
                  './data_raw/raw_data_wos/savedrecs2.txt',
                  './data_raw/raw_data_wos/savedrecs3.txt',
                  './data_raw/raw_data_wos/savedrecs4.txt',
                  './data_raw/raw_data_wos/savedrecs5.txt',
                  './data_raw/raw_data_wos/savedrecs6.txt',
                  './data_raw/raw_data_wos/savedrecs7.txt',
                  './data_raw/raw_data_wos/savedrecs8.txt',
                  './data_raw/raw_data_wos/savedrecs9.txt',
                  './data_raw/raw_data_wos/savedrecs10.txt',
                  './data_raw/raw_data_wos/savedrecs11.txt',
                  './data_raw/raw_data_wos/savedrecs12.txt',
                  './data_raw/raw_data_wos/savedrecs13.txt',
                  './data_raw/raw_data_wos/savedrecs14.txt')

# load records from SCOPUS  
articles_scopus <- c('./data_raw/raw_data_scopus/scopus1.bib',
                          './data_raw/raw_data_scopus/scopus2.bib',
                          './data_raw/raw_data_scopus/scopus3.bib',
                          './data_raw/raw_data_scopus/scopus4.bib',
                          './data_raw/raw_data_scopus/scopus5.bib',
                          './data_raw/raw_data_scopus/scopus6.bib',
                          './data_raw/raw_data_scopus/scopus7.bib',
                          './data_raw/raw_data_scopus/scopus8.bib',
                          './data_raw/raw_data_scopus/scopus9.bib',
                          './data_raw/raw_data_scopus/scopus10.bib',
                          './data_raw/raw_data_scopus/scopus11.bib',
                          './data_raw/raw_data_scopus/scopus12.bib',
                          './data_raw/raw_data_scopus/scopus13.bib',
                          './data_raw/raw_data_scopus/scopus14.bib',
                          './data_raw/raw_data_scopus/scopus15.bib',
                          './data_raw/raw_data_scopus/scopus16.bib',
                          './data_raw/raw_data_scopus/scopus17.bib',
                          './data_raw/raw_data_scopus/scopus18.bib',
                          './data_raw/raw_data_scopus/scopus19.bib',
                          './data_raw/raw_data_scopus/scopus20.bib',
                          './data_raw/raw_data_scopus/scopus21.bib',
                          './data_raw/raw_data_scopus/scopus22.bib',
                          './data_raw/raw_data_scopus/scopus23.bib',
                          './data_raw/raw_data_scopus/scopus24.bib',
                 './data_raw/raw_data_scopus/scopusOA.bib')


################################################################
# Use package 'bibliometrix' to convert these to dataframes                         
################################################################
# WOS dataframe
articles_wos_df <- convert2df(articles_wos, dbsource = "wos", format = "plaintext")
# Remove columns from the dataframe that only have NA values in them 
articles_wos_df<-articles_wos_df[colSums(!is.na(articles_wos_df)) > 0]
# Search for any duplicate records using the article DOI
dupes<-duplicated(articles_wos_df$DI) # IDs the dupes
summary(dupes) # TRUE tells you how many dupes there are
# Keep only one of the duplicated records
articles_wos_df<-articles_wos_df[!duplicated(articles_wos_df$DI), ]
# Check again to ensure there are no duplicates remaining
dupes<-duplicated(articles_wos_df$DI)
summary(dupes) # all will be FALSE if no duplicates
rm(dupes)

# SCOPUS dataframe
articles_scopus_df <- convert2df(articles_scopus, dbsource = "scopus", format = "bibtex")
# Remove columns from the dataframe that only have NA values in them 
articles_scopus_df<-articles_scopus_df[colSums(!is.na(articles_scopus_df)) > 0]
# Search for any duplicate records using the article DOI
dupes<-duplicated(articles_scopus_df$DI) # IDs the dupes
summary(dupes) # TRUE tells you how many dupes there are
# Keep only one of the duplicated records
articles_scopus_df<-articles_scopus_df[!duplicated(articles_scopus_df$DI), ]
# Check again to ensure there are no duplicates remaining
dupes<-duplicated(articles_scopus_df$DI)
summary(dupes) # all will be FALSE if no duplicates
rm(dupes)

################################################################
# bind the SCOPUS and WOS dataframes together and
# remove any duplicate records, and save the resulting df
################################################################
all_articles_df<-bind_rows(articles_scopus_df,articles_wos_df)
# Remove columns from the dataframe that only have NA values in them 
all_articles_df<-all_articles_df[colSums(!is.na(all_articles_df)) > 0]
# Search for any duplicate records using the article DOI
dupes<-duplicated(all_articles_df$DI) # IDs the dupes
summary(dupes) # TRUE tells you how many dupes there are
# Keep only one of the duplicated records
all_articles_df<-all_articles_df[!duplicated(all_articles_df$DI), ]
# Check again to ensure there are no duplicates remaining
dupes<-duplicated(all_articles_df$DI)
summary(dupes) # all will be FALSE if no duplicates
rm(dupes)
# save the df of records as .csv file in "data_clean" folder
write.csv(all_articles_df,"./data_clean/all_articles.csv", row.names = FALSE)
################################################################



################################################################
# process the dataframe with 'bibliometrix'
# (it's commented out because it's not necessary for us)
################################################################
#results_all <- biblioAnalysis(all_articles_df, sep = ";")


################################################################
# Prep the and save the datafile that will be used in analyses
# select only columns needed, then add:
# ISO 3 digit codes for each country, world bank income classes
# for each country, info on the journal category
################################################################


################################################################
# First Extract each authors country of affiliation 
# (the last column of the processed df) 
################################################################
# for some reason it is necessary to extract country data from 
# SCOPUS and WOS files independently, then merge. That's why can't use: 
# AuGeoAll <- metaTagExtraction(all_articles_df, Field = "AU_CO", sep = ";")
AuGeo_wos <- metaTagExtraction(articles_wos_df, Field = "AU_CO", sep = ";")
AuGeo_scopus <- metaTagExtraction(articles_scopus_df, Field = "AU_CO", sep = ";")
AllData<-bind_rows(AuGeo_scopus,AuGeo_wos)

# remove from the environment
rm(all_articles_df,
   articles_wos,
   articles_scopus,
   articles_scopus_df,
   articles_wos_df,
   AuGeo_scopus,
   AuGeo_wos)

################################################################
# select columns: article DOI, journal, year published, author country.
################################################################
AllData<-AllData %>% select(DI,SO,PY,AU_CO)

# Add pair_key (id no. for a mirror pair) & journal type (OA or PW)
AllData<-left_join(AllData,MirrorPairs,by="SO") 
AllData<-select(AllData,-notes) #remove notes column
AllData<-droplevels(AllData)

# remove from the environment
rm(AuGeoAll)
################################################################
# The countries of all authors of an article are in one cell. 
# this section splits them into multiple columns, then converts
# the df from wide to long form.
tempDF<- as.data.frame(str_split(AllData$AU_CO, ";", simplify = TRUE))
tempDF <- tempDF %>% mutate_all(na_if,"")  #replace the blanks with NA
# Need to do this next step or 'gather' won't work properly
tempDF <- data.frame(lapply(tempDF, as.character), stringsAsFactors=FALSE) 
#bind the new dataframe of countries in wide form to original
AllData<-cbind(AllData,tempDF)
rm(tempDF) #remove the tempdf from environment
# gather into long form 
AllData<-AllData %>%
  gather(author,country,7:ncol(AllData))
# remove the 'V' from cells in author column
# This also adds the order of authors for each paper 
AllData$author<-gsub("V","",AllData$author) 
AllData$author<-as.numeric(AllData$author)
# head(AllData,10)
# organize the df by article, with authors in order from 1...N
AllData<-AllData %>% arrange(DI,author)
# remove any that are incomplete
AllData<-AllData[complete.cases(AllData), ]
# convert DOI to a factor
AllData$DI<-as.factor(AllData$DI)
# delete the column with all countries in a single cell
AllData$AU_CO<-NULL 

################################################################
# Add  ISO 3-digit code for each country with 'countrycode'
################################################################
# Setting "warn=TRUE" tells you of any it couldn't convert 
# because of spelling mistakes, etc.
AllData$country_code<-
  countrycode(AllData$country,"country.name", "iso3c", warn = TRUE)

# convert variables to factor
AllData$country_code<-as.factor(AllData$country_code)
AllData$journal_cat<-as.factor(AllData$journal_cat)
AllData$country<-as.factor(AllData$country)
AllData$SO<-as.factor(AllData$SO)

# rename columns
AllData <- AllData %>%
  select(DOI = DI, Journal = SO, Year = PY, AuthorNum = author,
         Country = country, Code = country_code, JrnlType = journal_cat, pair_key)



################################################################
# load and add World Bank data on national income categories
CountryData <- read.csv("data_raw/CLASS.csv", header = TRUE)
CountryData <- CountryData[-1,]
CountryData <- CountryData %>%
  select(Code,Region, IncomeGroup = Income.group)
# add it to the dataframe
AllData <- merge(AllData, CountryData, by="Code", all.x=TRUE) # merge 
# remove CountryData from the environment
rm(CountryData)

# make IncomeGroup an ordered factor (Low to High)
AllData$IncomeGroup<- ordered(AllData$IncomeGroup, levels = c("Low income", "Lower middle income", "Upper middle income","High income"))
levels(AllData$IncomeGroup)[levels(AllData$IncomeGroup)=="High income"] <- "High"
levels(AllData$IncomeGroup)[levels(AllData$IncomeGroup)=="Low income"] <- "Low"
levels(AllData$IncomeGroup)[levels(AllData$IncomeGroup)=="Lower middle income"] <- "Lower middle"
levels(AllData$IncomeGroup)[levels(AllData$IncomeGroup)=="Upper middle income"] <- "Upper middle"
str(AllData)

# convert to factor
AllData$pair_key<-as.factor(AllData$pair_key)
AllData$pair_key<-droplevels(AllData$pair_key)
# levels(AllData$pair_key)
# There are some with missing DOI values, if you don't replace missing DOI
# they will be excluded when grouping
AllData$DOI<- as.character(AllData$DOI)
AllData$DOI<- AllData$DOI %>% replace_na("missing_DOI")
AllData$DOI<- as.factor(AllData$DOI)
AllData<-AllData %>% arrange(DOI,AuthorNum)
AllData$IncomeGroup <- as.factor(AllData$IncomeGroup)
AllData$IncomeGroup <- ordered(AllData$IncomeGroup, levels = c("High", "Upper middle","Lower middle","Low"))
# head(AllData)

############################################################
# Remove any journal pairs for whihc data re incomplete
############################################################
# TODO: no articles for "Clinics and Research in Hepatology and
# Gastroenterology: X (9)" so exclude it and mirror
# Diabetes and Metabolism: X (13) is missing (no info on page) 
# Europ. J Obsterics, Gynecology: X (16) none published) 
missing_jrnls<-c(9,13,16)
AllData<-AllData %>% filter(!pair_key%in% missing_jrnls)
rm(missing_jrnls)


# save the csv
write.csv(AllData,"./data_clean/AllData.csv", row.names = FALSE)


################################################################
# Change column names of Mirror Pairs and save csv to 'data_clean'
################################################################
MirrorPairs <- MirrorPairs%>%
  select(Journal = SO, JrnlType = journal_cat, pair_key) %>% 
  filter(pair_key>0)
# save the df as .csv file in "data_clean" folder
write.csv(MirrorPairs,"./data_clean/MirrorPairs.csv", row.names = FALSE)
# remove from the environment
rm(MirrorPairs)
################################################################
