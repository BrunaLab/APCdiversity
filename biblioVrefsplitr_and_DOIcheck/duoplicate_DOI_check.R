# the libraries
library(tidyverse)
library(bibliometrix)
library(countrycode) 

# SCOPUS
unique_articles_scopus_df<-articles_scopus_df %>% distinct(DI, .keep_all = FALSE)
scopus_dupe_DOI<-setdiff(articles_scopus_df,unique_articles_scopus_df) #Set difference between x and y, consisting of all elements of x that are not in y
scopus_dupe_DOI<-select(scopus_dupe_DOI,DI)

duplicated_scopus<-articles_scopus_df %>% 
  filter(DI %in% scopus_dupe_DOI$DI) %>% 
  arrange(DI)
duplicated_scopus$DI


# WOS
unique_articles_wos_df<-articles_wos_df %>% distinct(DI, .keep_all = TRUE)
wos_dupe_DOI<-setdiff(articles_wos_df,unique_articles_wos_df) #Set difference between x and y, consisting of all elements of x that are not in y
wos_dupe_DOI<-select(wos_dupe_DOI,DI)

duplicated_wos<-articles_wos_df %>% 
  filter(DI %in% wos_dupe_DOI$DI) %>% 
  arrange(DI)
duplicated_wos$DI

# ALL
unique_all_articles_df<-all_articles_df %>% distinct(DI, .keep_all = TRUE)
all_dupe_DOI<-setdiff(all_articles_df,unique_all_articles_df) #Set difference between x and y, consisting of all elements of x that are not in y
all_dupe_DOI<-select(all_dupe_DOI,DI)

duplicated_all<-all_articles_df %>% 
  filter(DI %in% all_dupe_DOI$DI) %>% 
  arrange(DI)
duplicated_all$DI

nrow(duplicated_all)/nrow(all_articles_df)

# WOS savedrecs10.txt:
# Mantel: Seasonal Influenza Vaccination in Middle-Income Countries: Assessment of Immunization Practices
# in Belarus, Morocco, and Thailand
# downloaded WOS record has this as DOI 10.1016/j.vaccine.2019.07.028
# Correct DOI is 10.1016/j.vaccine.2019.10.028
# DOI 10.1016/j.vaccine.2019.07.028 belongs to Chowdhury et al Newcastle Disease Virus Vectors Expressing 
# Consensus Sequence of the H7 HA Protein Protect Broiler Chickens and Turkeys Against Highly Pathogenic H7N8 Virus
library(bibliometrix)
extract_test <- c('./data_raw/country_test/extract_test.txt',
                  './data_raw/country_test/extract_test2.txt')
# extract_test <- c('./data_raw/country_test/extract_test2.txt')
extract_test_df <- convert2df(extract_test, dbsource = "wos", format = "plaintext")
extract_test_df<-extract_test_df[colSums(!is.na(extract_test_df)) > 0]
dupes<-duplicated(extract_test_df$AU) # IDs the dupes
summary(dupes) # TRUE tells you how many dupes there are
# # Keep only one of the duplicated records
extract_test_df<-extract_test_df[!duplicated(extract_test_df$AU), ]
# # Check again to ensure there are no duplicates remaining
dupes<-duplicated(extract_test_df$AU)
summary(dupes) # all will be FALSE if no duplicates
rm(dupes)
extract_test_df$DI

results <- biblioAnalysis(extract_test_df, sep = ";")
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)
country_check_wos <- metaTagExtraction(extract_test_df, Field = "AU_CO", sep = ";")

country_check_wos$AU_CO

country_check_wos<-country_check_wos %>% select(DI,SO,PY,AU_CO)
tempDF<- as.data.frame(str_split(country_check_wos$AU_CO, ";", simplify = TRUE))
tempDF <- tempDF %>% mutate_all(na_if,"")  #replace the blanks with NA
# Need to do this next step or 'gather' won't work properly
tempDF <- data.frame(lapply(tempDF, as.character), stringsAsFactors=FALSE) 
#bind the new dataframe of countries in wide form to original
country_check_wos<-cbind(country_check_wos,tempDF)
rm(tempDF) #remove the tempdf from environment
# gather into long form 
country_check_wos<-country_check_wos %>%
  gather(author,country,5:ncol(country_check_wos))
colnames(country_check_wos)
head(country_check_wos)
# remove the 'V' from cells in author column
# This also adds the order of authors for each paper 
country_check_wos$author<-gsub("V","",country_check_wos$author) 
country_check_wos$author<-as.numeric(country_check_wos$author)
# head(country_check_wos,10)
# organize the df by article, with authors in order from 1...N
country_check_wos<-country_check_wos %>% arrange(DI,author)
# remove any that are incomplete
country_check_wos<-country_check_wos[complete.cases(country_check_wos), ]
# convert DOI to a factor
country_check_wos$DI<-as.factor(country_check_wos$DI)
# delete the column with all countries in a single cell
country_check_wos$AU_CO<-NULL 



country_check_wos1 <- metaTagExtraction(extract_test_df, Field = "AU1_CO", sep = ";")
country_check_wos <- metaTagExtraction(extract_test_df, Field = "AU_CO", sep = ";")
country_check_wos1$AU1_CO
AUCO<-as.data.frame(country_check_wos$AU_CO)
names(AUCO)<-c("AU_CO")
country_check_wos1<-bind_cols(country_check_wos1,AUCO)
colnames(country_check_wos1)
country_check_wos1<-country_check_wos1 %>% select(TI,DI,SO,PY,AU_CO,AU1_CO)
tempDF<- as.data.frame(str_split(country_check_wos1$AU_CO, ";", simplify = TRUE))
tempDF <- tempDF %>% mutate_all(na_if,"")  #replace the blanks with NA
# Need to do this next step or 'gather' won't work properly
tempDF <- data.frame(lapply(tempDF, as.character), stringsAsFactors=FALSE) 
#bind the new dataframe of countries in wide form to original
country_check_wos1<-cbind(country_check_wos1,tempDF)
rm(tempDF) #remove the tempdf from environment
# gather into long form 
country_check_wos1<-country_check_wos1 %>%
  gather(author,country,7:ncol(country_check_wos1))
colnames(country_check_wos1)
head(country_check_wos1)
# remove the 'V' from cells in author column
# This also adds the order of authors for each paper 
country_check_wos1$author<-gsub("V","",country_check_wos1$author) 
country_check_wos1$author<-as.numeric(country_check_wos1$author)
# head(country_check_wos,10)
# organize the df by article, with authors in order from 1...N
country_check_wos1<-country_check_wos1 %>% arrange(TI,author)
# remove any that are incomplete
country_check_wos1<-country_check_wos1[complete.cases(country_check_wos1), ]
# convert DOI to a factor
country_check_wos1$DI<-as.factor(country_check_wos1$DI)
# delete the column with all countries in a single cell
country_check_wos1$AU_CO<-NULL 








# PROBLEMS WITH BIBLIOMETRIX!!!
#   https://doi.org/10.1016/j.jbi.2019.103371
# Masud et al.: first authors affiliations are 1=Korea, 2=bangladesh BUT 
# puts Bangladesh as Country when doing metatag extraction!
# DOESNT EXTRACT ALL COUNTRIES AUTHI AFFILIATION
# See below for example from their sample dataset or above from our own.
data(scientometrics)
#FIRST AUTHOR EXTRACTION
scientometrics_AU1_CO <- metaTagExtraction(scientometrics, Field = "AU1_CO", sep = ";")
scientometrics_AU1_CO$key<-seq(1:nrow(scientometrics_AU1_CO))
#ALL AUTHOR EXTRACTION
scientometrics_AU_CO <- metaTagExtraction(scientometrics, Field = "AU_CO", sep = ";")
scientometrics_AU_CO$key<-seq(1:nrow(scientometrics_AU_CO))
authors_count<-scientometrics_AU_CO %>% select(key,AU_CO)
temp_au<- as.data.frame(str_split(authors_count$AU_CO, ";", simplify = TRUE))
key<-as.data.frame(authors_count$key)
names(key)<-c("key")
temp_au<-bind_cols(key,temp_au)
temp_au<-temp_au %>%
  gather(author,country,2:ncol(temp_au))
# remove the 'V' from cells in author column
# This also adds the order of authors for each paper 
temp_au$author<-gsub("V","",temp_au$author) 
temp_au$author<-as.numeric(temp_au$author)
temp_au<-arrange(temp_au,key)

# compare "1st author" from AU_CO and "1st author" from AU1_CO 
AU_CO_first <- temp_au %>% 
  group_by(key) %>% 
  filter(author==1) %>% 
  select(key,country)
str(AU_CO_first)

AU1_CO_first<-scientometrics_AU1_CO %>% select(key,AU1_CO)
str(AU1_CO_first)
#How do they compare?
summary(AU_CO_first$country==AU1_CO_first$AU1_CO)


foo1<-scientometrics[1,]
colnames(foo1)
foo1$AU_CO
foo1$C1
foo1$AU
foo1$TI
# ALSO
# 
# Maxes out author extraction at 48(?) 
# look at 10.1016/j.vaccine.2019.06.068
# last one extracted is "Cale"

