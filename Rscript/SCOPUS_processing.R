#=============================================================================================================#
# Script created by Emilio M. Bruna (embruna@ufl.edu) to import and prepare
# SCOPUS data with package `refsplitr` for the analyses in: 
# PAPER CITATION TO BE ADDED 
# Script created in  R version 3.6.3
# Uses packages tidyverse_1.3.0 , countrycode_1.1.3
#=============================================================================================================#

# load the libraries
library(tidyverse)
library(countrycode)
library(bibliometrix)


################################################################
# Load the list of Pawall Journals and their OA mirrors 
################################################################
MirrorPairs<-read_csv(file="./data_raw/MirrorPairs.csv")


################################################################
# load records from SCOPUS  
################################################################
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
# SCOPUS dataframe
articles_scopus_df <- convert2df(articles_scopus, dbsource = "scopus", format = "bibtex")

################################################################
# SCOPUS CLEANING
################################################################
# Adding package title and correcting the SO titles
scopus<-as_tibble(articles_scopus_df)
scopus$package<-"bibliometrix"
scopus$db<-"scopus"
scopus<-left_join(scopus,MirrorPairs,by="SO")
colnames(scopus) 
missing_pk<-scopus %>% filter(is.na(pair_key))
titles_needing_pairkey<-as.data.frame(summary(as.factor(missing_pk$SO)))
titles_needing_pairkey

# Correct SO needing a Pair Key
scopus$pair_key[scopus$SO == 'EUROPEAN JOURNAL OF OBSTETRICS AND GYNECOLOGY AND REPRODUCTIVE BIOLOGY: X'] <- 16
scopus$journal_cat[scopus$SO == 'EUROPEAN JOURNAL OF OBSTETRICS AND GYNECOLOGY AND REPRODUCTIVE BIOLOGY: X'] <- 'OA'

scopus$SO[scopus$SO == 'EUROPEAN JOURNAL OF OBSTETRICS, GYNECOLOGY, AND REPRODUCTIVE BIOLOGY'] <- "EUROPEAN JOURNAL OF OBSTETRICS AND GYNECOLOGY AND REPRODUCTIVE BIOLOGY"
scopus$pair_key[scopus$SO == 'EUROPEAN JOURNAL OF OBSTETRICS AND GYNECOLOGY AND REPRODUCTIVE BIOLOGY'] <- 16
scopus$journal_cat[scopus$SO == 'EUROPEAN JOURNAL OF OBSTETRICS AND GYNECOLOGY AND REPRODUCTIVE BIOLOGY'] <- 'PW'


scopus$SO[scopus$SO == "CHAOS, SOLITONS AND FRACTALS"] <- "CHAOS SOLITONS AND FRACTALS"
scopus$SO[scopus$SO == "CHAOS, SOLITONS AND FRACTALS: X"] <- "CHAOS SOLITONS AND FRACTALS: X"

scopus$pair_key[scopus$SO == 'RESOURCES, CONSERVATION AND RECYCLING: X'] <- 41
scopus$journal_cat[scopus$SO == 'RESOURCES, CONSERVATION AND RECYCLING: X'] <- 'OA'
scopus$SO[scopus$SO == 'RESOURCES, CONSERVATION AND RECYCLING: X'] <- 'RESOURCES CONSERVATION AND RECYCLING: X'



# scopus$pair_key[scopus$SO == 'COMPUTERS AND GRAPHICS (PERGAMON)'] <- 10
scopus$SO[scopus$SO == 'COMPUTERS AND GRAPHICS (PERGAMON)'] <- 'COMPUTERS AND GRAPHICS'
scopus$journal_cat[scopus$SO == 'COMPUTERS AND GRAPHICS'] <- 'PW'

scopus$SO[scopus$SO == 'WATER RESEARCH X'] <- "WATER RESEARCH: X"
scopus$pair_key[scopus$SO == 'WATER RESEARCH: X'] <- 39
scopus$journal_cat[scopus$SO == 'WATER RESEARCH: X'] <- "OA"
levels(as.factor(scopus$SO))

# foo<-scopus %>% group_by(SO,journal_cat) %>% summarize(n())
# correct / remove the ones with pair_key==0 
scopus %>% filter(is.na(pair_key)) %>% group_by(SO) %>% tally()
scopus<-scopus %>% filter(!is.na(pair_key))

# Add a refID number that starts 1 after the last WOS
scopus$refID<-NA

# Eliminate any not article or review
scopus <- scopus %>% filter(DT=="ARTICLE"|DT=="REVIEW")
# are there any duplicated DOIs, TI in scopus data?

# CHECK AND ELIMINATE DUPLICATE TITLES & DOI
# there are >2K duplicated records, which you can 
# tell by doing this
DupeCount_scopus<-scopus %>% 
  group_by(DI) %>%
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(n>1)

dupes_scopus <- scopus %>% 
  select(refID,DI,TI) %>% 
  filter(DI %in% DupeCount_scopus$DI) %>% 
  arrange(DI,TI)

DupeCount_scopus<-scopus %>% 
  group_by(DI,TI,PY,SO) %>%
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(n>1)

# Need to remove the duplicated records
scopus<-scopus %>% distinct(DI,TI, .keep_all = TRUE)
# TITLES   
DupeCount_scopus<-scopus %>% 
  group_by(TI) %>%
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(n>1)

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
scopus<-scopus %>% filter(DI!="10.1016/j.cytox.2019.100015")
scopus<-scopus %>% filter(DI!="10.1016/j.jnoncrysol.2019.03.032")
scopus<-scopus %>% filter(DI!="10.1016/j.nocx.2019.100036")
scopus<-scopus %>% filter(DI!="10.1016/j.ecoleng.2019.07.009")
scopus<-scopus %>% filter(DI!="10.1016/j.jcp.2019.07.039")
scopus<-scopus %>% filter(DI!="10.1016/j.ecmx.2019.100014")
scopus<-scopus %>% filter(DI!="10.1016/j.athx.2019.100006")
scopus<-scopus %>% filter(DI!="10.1016/j.watres.2018.12.001")

# now looking for duplicate DOIs
DupeCount_scopus<-scopus %>% 
  group_by(DI) %>%
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(n>1)
DupeCount_scopus
# look these over
dupes_scopus <- scopus %>% 
  select(refID,DI,TI,package) %>% 
  filter(DI %in% DupeCount_scopus$DI) %>% 
  arrange(DI,TI)
dupes_scopus
# # when you do, you can see that the duplicate DOIs are for different articles 
# # so you need to change one to something else: add _dupe to it
# scopus$DI<- ifelse(duplicated(scopus$DI),paste(scopus$DI,"_dupe",sep=""),scopus$DI)
# # also need to correct any missing DOIs
# scopus<-scopus %>% replace_na(list(DI = "missing_doi_"))
# scopus$DI<-ifelse(scopus$DI== "missing_doi_",paste("missing_doi_",scopus$refID,sep=""),scopus$DI)

write_csv(scopus,"./output/scopus.csv")
# scopus<-read_csv("./output/scopus.csv")
