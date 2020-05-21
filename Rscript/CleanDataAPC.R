# the libraries
library(tidyverse)
library(bibliometrix)

################################################################
# THIS IS FOR ÅLL DOWNLOADED FROM SCOPUS BY EB
################################################################

# load journal list and pairs
MirrorPairs<-read_csv(file="./data/MirrorPairs.csv")

######################
# Process the data
######################
###################################
# PW: 2 sets
###################################


articles_wos <- c('./data/raw_data_wos/savedrecs1.txt',
                  './data/raw_data_wos/savedrecs2.txt',
                  './data/raw_data_wos/savedrecs3.txt',
                  './data/raw_data_wos/savedrecs4.txt',
                  './data/raw_data_wos/savedrecs5.txt',
                  './data/raw_data_wos/savedrecs6.txt',
                  './data/raw_data_wos/savedrecs7.txt',
                  './data/raw_data_wos/savedrecs8.txt',
                  './data/raw_data_wos/savedrecs9.txt',
                  './data/raw_data_wos/savedrecs10.txt',
                  './data/raw_data_wos/savedrecs11.txt',
                  './data/raw_data_wos/savedrecs12.txt',
                  './data/raw_data_wos/savedrecs13.txt',
                  './data/raw_data_wos/savedrecs14.txt')

articles_wos_df <- convert2df(articles_wos, dbsource = "wos", format = "plaintext")
articles_wos <- biblioAnalysis(articles_wos_df, sep = ";")
summary_wos <- summary(object = articles_wos, k = 10, pause = FALSE)
# # # The have some nice plots
plot(x = articles_wos14may, k = 10, pause = FALSE)
# # # We need to extract the countries for each author
# # # These data will be the last column of the processed df
AuGeo_wos <- metaTagExtraction(articles_wos_df, Field = "AU_CO", sep = ";")
str(AuGeo_wos)
head(AuGeo_wos,20)

write.csv(articles_wos_df,"./output/articles_wos_df.csv")
write.csv(AuGeo_wos,"./output/AuGeo_wos.csv")
###################################
# read in the SCOPUS DATA
articles_scopus <- c('./data/raw_data_scopus/scopus1.bib',
                          './data/raw_data_scopus/scopus2.bib',
                          './data/raw_data_scopus/scopus3.bib',
                          './data/raw_data_scopus/scopus4.bib',
                          './data/raw_data_scopus/scopus5.bib',
                          './data/raw_data_scopus/scopus6.bib',
                          './data/raw_data_scopus/scopus7.bib',
                          './data/raw_data_scopus/scopus8.bib',
                          './data/raw_data_scopus/scopus9.bib',
                          './data/raw_data_scopus/scopus10.bib',
                          './data/raw_data_scopus/scopus11.bib',
                          './data/raw_data_scopus/scopus12.bib',
                          './data/raw_data_scopus/scopus13.bib',
                          './data/raw_data_scopus/scopus14.bib',
                          './data/raw_data_scopus/scopus15.bib',
                          './data/raw_data_scopus/scopus16.bib',
                          './data/raw_data_scopus/scopus17.bib',
                          './data/raw_data_scopus/scopus18.bib',
                          './data/raw_data_scopus/scopus19.bib',
                          './data/raw_data_scopus/scopus20.bib',
                          './data/raw_data_scopus/scopus21.bib',
                          './data/raw_data_scopus/scopus22.bib',
                          './data/raw_data_scopus/scopus23.bib',
                          './data/raw_data_scopus/scopus24.bib',
                 './data/raw_data_scopus/scopusOA.bib')
                          
articles_scopus_df <- convert2df(articles_scopus, dbsource = "scopus", format = "bibtex")
articles_scopus <- biblioAnalysis(articles_scopus_df, sep = ";")

AuGeo_scopus <- metaTagExtraction(articles_scopus_df, Field = "AU_CO", sep = ";")
str(AuGeo_scopus)
head(AuGeo_scopus,20)
write.csv(articles_scopus_df,"./output/articles_scopus_df.csv")
write.csv(AuGeo_scopus,"./output/AuGeo_scopus.csv")


colnames(AuGeo_scopus)
colnames(AuGeo_wos)


# #########################
# # x_articles_df$C1
# #########################
# 
# ########################################
# x_articles_df <-bind_rows(articles_x_df,articles_x2_df)
all_articles_df<-bind_rows(articles_scopus_df,articles_wos_df)
# all_articles_df$SO<-as.factor(all_articles_df$SO)
# all_articles_df$DI<-as.factor(all_articles_df$DI)
head(all_articles_df,20)
all_articles_df<-all_articles_df[colSums(!is.na(all_articles_df)) > 0]
head(all_articles_df,20)

# After doing analyses I discovered some duplicates were not being eliminated
# by bibliometrix, so search and clear them
# ID the dupes and tell you how many there are
dupes<-duplicated(all_articles_df$DI)
summary(dupes)

# eliminate the duplicates
all_articles_df<-all_articles_df[!duplicated(all_articles_df$DI), ]
dupes<-duplicated(all_articles_df$DI)
summary(dupes)


# save as a csv file
write.csv(all_articles_df,"./output/all_articles.csv")
# all_articles_df<-read_csv("./output/all_articles.csv")

# pw_articles<-filter(all_articles_df, journal_cat=="PW")
# oa_articles<-filter(all_articles_df, journal_cat=="OA")

results_all <- biblioAnalysis(all_articles_df, sep = ";")

levels(as.factor(results_all$CO))
options(width=100)
# a summary of their analyses

summary_all <- summary(object = results_all, k = 10, pause = FALSE)
# The have some nice plots
plot(x = results_all, k = 10, pause = FALSE)

# We need to extract the countries for each author
# These data will be the last column of the processed df
AuGeoAll <- metaTagExtraction(all_articles_df, Field = "AU_CO", sep = ";")
str(AuGeoAll)
head(AuGeoAll,20)
write.csv(AuGeoAll,"./output/AuGeoAll.csv")

# TO STREAMLINE, select only the doi of the article, the journal, the year published, and the information on author country
# note that all author countries are in a single column
AuGeoAll<-bind_rows(AuGeo_scopus,AuGeo_wos)
all_articles_geodata<-AuGeoAll %>% select(DI,SO,PY,AU_CO)
# Add the pair key and journal type
all_articles_geodata<-left_join(all_articles_geodata,MirrorPairs,by="SO") 
all_articles_geodata<-select(all_articles_geodata,-notes)
colnames(all_articles_geodata)

all_articles_geodata<-droplevels(all_articles_geodata)
str(all_articles_geodata)

# this splits up the author countries - currently in a single column - into multiple columns (each country in its own column)
tempDF<- as.data.frame(str_split(all_articles_geodata$AU_CO, ";", simplify = TRUE))
tempDF <- tempDF %>% mutate_all(na_if,"")  #replace the blanks with NA

tempDF <- data.frame(lapply(tempDF, as.character), stringsAsFactors=FALSE) # Need to do this or gather won't work properly
all_articles_geodata<-cbind(all_articles_geodata,tempDF)
rm(tempDF)
all_articles_geodata<-all_articles_geodata %>% gather(author,country,7:ncol(all_articles_geodata))
all_articles_geodata$author<-gsub("V","",all_articles_geodata$author) # remove the V, now have the author order
all_articles_geodata$author<-as.numeric(all_articles_geodata$author)
head(all_articles_geodata,10)
all_articles_geodata<-all_articles_geodata %>% arrange(DI,author)
all_articles_geodata<-all_articles_geodata[complete.cases(all_articles_geodata), ]
head(all_articles_geodata,10)
all_articles_geodata$DI<-as.factor(all_articles_geodata$DI)
all_articles_geodata$AU_CO<-NULL #delete the column with all countries in a single cell

# You can add the ISO three digit code for each country using library(countrycode)
library(countrycode) 
all_articles_geodata$country_code<-countrycode(all_articles_geodata$country,"country.name", "iso3c", warn = TRUE)
#By setting "warn=TRUE" it will tell you which ones it couldn't convert. Because of spelling mistakes, etc.
all_articles_geodata$country_code<-as.factor(all_articles_geodata$country_code)
summary(all_articles_geodata$country_code)
head(all_articles_geodata,10)
summary(all_articles_geodata)
colnames(all_articles_geodata)

write.csv(all_articles_geodata,"./output/all_Journal_author_countries.csv")
