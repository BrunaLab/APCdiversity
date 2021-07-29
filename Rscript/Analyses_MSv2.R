#=============================================================================================================#
# Script created by Jesse Borden and Emilio M. Bruna (embruna@ufl.edu) analyze
# data and generate figures/tables for : PAPER CITATION TO BE ADDED 
# Script created in  R version 3.6.3 (2020-02-29)
# Uses packages tidyverse_1.3.0 , vegan_2.5-6, reshape0.8.8
#=============================================================================================================#
# libraries
# library(purrr)
# library(stringr)
# library(sampling)
# library(reshape)
# library(vegan)
library(tidyverse)

# NEED TO STANDARDIZE COUNTRYIES IN UK IN COUNTRY COLUMN
MirrorPairs<-read.csv("./data_clean/MirrorPairs.csv")
WaiverCountries<-read.csv("./data_clean/WaiverCountries.csv")
NON_WavierCountries<-read.csv("./data_clean/NON_WavierCountries.csv")


# World Bank data on national income categories
CountryData <- read.csv("data_raw/CLASS.csv", header = TRUE)
CountryData <- CountryData[-1,]
CountryData <- CountryData %>%
  select(Code,Region, IncomeGroup = Income.group) %>% 
  filter(Region=="East Asia & Pacific"|
           Region=="Europe & Central Asia"|
           Region=="Latin America & Caribbean"|
           Region== "Middle East & North Africa"|
           Region== "North America"|
           Region=="South Asia"|
           Region== "Sub-Saharan Africa")
CountryData$Region <-droplevels(CountryData$Region)
levels(CountryData$Region)
write_csv(CountryData, "./data_clean/CountryData.csv")

# These are the OA articles that were published in PW journals
# they are NOT in the AllData file - that file includes ONLY
# PW articles in PW journals
OA_in_PW<-read_csv(file="./data_clean/OA_in_PW.csv")
OA_in_PW<-ungroup(OA_in_PW)
# OA_in_PW$JrnlType<-"PW"
# OA_in_PW$ArticleType<-"OA"

# load the rest of the data, bind to the OA in PW
AllData<-read_csv(file="./data_clean/all_data_analysis_noOAinPW.csv")
AllData<-ungroup(AllData)
as.factor(AllData$ArticleType)

# anti_join(OA_in_PW,AllData,by="refID")
# distinct(AllData)

AllData<-bind_rows(AllData,OA_in_PW)
as.factor(AllData$ArticleType)

AllData$pair_key<-as.factor(AllData$pair_key)
AllData$pair_key<-droplevels(AllData$pair_key)

AllData$First_Author_Country[AllData$First_Author_Country=="uk"|
                               AllData$First_Author_Country=="UK"|
                               AllData$First_Author_Country=="GBR"]<-"gbr"

levels(as.factor(AllData$First_Author_Country))
# 56 NA articles
# all_data_analysis<-all_data_analysis %>%  drop_na("Code")

# n_distinct(AllData$refID)
# n_distinct(AllData$Title)
# n_distinct(AllData$DOI)

AllData_missing<-AllData %>% 
  filter(AuthorNum==1) %>% 
  filter(is.na(First_Author_Country)) %>% 
  select(refID) 
AllData<-AllData %>% filter(!refID %in% AllData_missing$refID) 
# n_distinct(AllData$refID)
# n_distinct(AllData$Title)
# n_distinct(AllData$DOI)
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


# AllData$JrnlType<-as.factor(AllData$JrnlType)
#

AllData<-AllData %>%
  group_by(pair_key) %>%
  mutate(JrnlType = ifelse(JrnlType=="OAinPW", "PW", JrnlType))
AllData$JrnlType<-as.factor(AllData$JrnlType)

AllData<-AllData %>%
  group_by(pair_key) %>%
  mutate(ArticleType = ifelse(ArticleType=="OAinPW", "OA", ArticleType))
AllData$ArticleType<-as.factor(AllData$ArticleType)


############ read in rsrch_area ############
RsrchAreas<-read_csv("./data_raw/journal_areas.csv") %>% 
  select(pair_key,Journal,wos_cat1,wos_cat2,subcat_1)
as.factor(RsrchAreas$wos_cat1)
as.factor(RsrchAreas$subcat_1)
RsrchAreas$pair_key<-as.factor(RsrchAreas$pair_key)
levels(as.factor(AllData$Journal))==levels(as.factor(RsrchAreas$Journal))
AllData<-left_join(AllData,RsrchAreas)
AllData$pair_key<-as.factor(AllData$pair_key)
AllData$Journal<-as.factor(AllData$Journal)
AllData$ArticleType<-as.factor(AllData$ArticleType)


AllData<-write_csv(AllData, "./data_clean/all_data_analysis.csv")

###########
# papers by rsrch cat
AllData$wos_cat1
AllPapers_for_cat1<-AllData %>% 
  group_by(refID) %>% 
  slice(1) %>% 
  group_by(wos_cat1) %>% 
  summarize(n())
AllPapers_for_cat1

AllPapers_for_cat2<-AllData %>% 
  group_by(refID) %>% 
  slice(1) %>% 
  group_by(JrnlType,wos_cat1) %>% 
  summarize(n()) 
AllPapers_for_cat2

AllPapers_for_cat3<-AllData %>% 
  group_by(refID) %>% 
  slice(1) %>% 
  group_by(ArticleType,wos_cat1) %>% 
  summarize(n())
AllPapers_for_cat3
levels(as.factor(AllData$wos_cat1))
AllPapers_for_cat4<-AllData %>% 
  group_by(refID) %>% 
  slice(1) %>% 
  group_by(JrnlType,ArticleType,wos_cat1) %>% 
  summarize(n=n()) %>% 
group_by(JrnlType,ArticleType) %>% 
  mutate(tot=sum(n)) %>% 
  mutate(perc=(n/tot*100)) %>% 
  arrange(wos_cat1,JrnlType,ArticleType)
AllPapers_for_cat4  









  
  AllData %>%
  group_by(refID,JrnlType,ArticleType) %>%
  slice(1) %>%
  group_by(JrnlType,ArticleType) %>%
  summarize(n())


# Need to generate subsets of sole-author pubs, coauthor pubs 
sole_ALL <- AllData %>%
  filter(author=="solo") %>% 
  mutate(Dataset="All Countries") %>% 
  drop_na("Code")
# n_distinct(sole_author_pubs_ALL$refID)

first_ALL<- AllData %>%
  filter(author=="coauthored") %>% 
  filter(AuthorNum==1) %>% 
  mutate(author="author_first") 

# subset of data with no first authors form CHN or USA
sole_NOCHNUSA <- AllData %>%
  filter(author=="solo") %>% 
  filter(Code!="CHN") %>% 
  filter(Code!="USA") %>% 
  mutate(Dataset="CHN & USA excluded")

first_NOCHNUSA<- AllData %>%
  filter(author=="coauthored") %>% 
  filter(AuthorNum==1) %>% 
  filter(Code!="CHN") %>% 
  filter(Code!="USA") %>% 
  mutate(Dataset="CHN & USA excluded") %>% 
  mutate(author="author_first")

###############################################################################


OAvOA<-AllData %>% 
  group_by(JrnlType,ArticleType) %>% 
  summarize(n_distinct(refID))

OAvOA_byYear<-AllData %>% 
  group_by(Year,JrnlType,ArticleType) %>% 
  summarize(n=n_distinct(refID))

OAvOA_byJournal<-AllData %>% 
  filter(ArticleType!="PW") %>% 
  group_by(Journal,pair_key,JrnlType,ArticleType) %>% 
  summarize(n=n_distinct(refID)) %>% 
  arrange(pair_key)

OAvOA_byJournalxYear<-AllData %>% 
  group_by(Year,pair_key, Journal,JrnlType,ArticleType) %>% 
  summarize(n=n_distinct(refID)) %>% 
  arrange(pair_key,Year)
OAvOA_byJournalxYear$cat2<-paste(OAvOA_byJournalxYear$JrnlType, OAvOA_byJournalxYear$ArticleType, sep=" in ")
write_csv(OAvOA_byJournalxYear,"./output/OAvOA_byJournalxYear.csv")


OAvOA_byYear$cat2<-paste(OAvOA_byYear$JrnlType, OAvOA_byYear$ArticleType, sep=" in ")
plot_OAvOA_byYear <- OAvOA_byYear %>%
  ggplot(aes(x=Year, y=n, group=cat2, color=cat2)) +
  geom_line()


countries_characteristics <- AllData %>% 
  filter(AuthorNum==1) %>% 
  select(Code,IncomeGroup,Region) %>% 
  group_by(IncomeGroup,Region) %>% 
  arrange(Region)

countries_characteristics <- countries_characteristics %>% 
  ungroup() %>% 
  select(-pair_key) %>% 
  unique() %>% 
  group_by(Region,IncomeGroup) %>% 
  summarize(n=n()) %>%
  group_by(Region) %>% 
  mutate(nR=sum(n)) %>%
  mutate(perc=(n/nR*100)) 

# 
# 
# ###############################################################################
# # COMPARE OBSERVED OA DIV/RICH with SUBSAMPLES of PW ARTICLES
# # PW ARTICLES WERE SAMPLED IN FREQUENCY BY JOURNAL MATCHING
# # OA JOURNALS
# 
# # RETURNS 2 DF: 
# # [1]: a value of div and rich from each bootstrp run
# # [2]: df of each country slected in each run (with freq)
# 
# ###############################################################################
# ################################
# WTF<-AllData %>% 
#   group_by(refID) %>% 
#   slice(1) %>% 
#   group_by(pair_key,ArticleType) %>% 
#   summarize(n=n()) %>% 
#   pivot_wider(names_from = ArticleType,values_from=n) %>% 
#   mutate(diff=PW-OA)
# sum(WTF$OA)
# sum(WTF$PW)
#   


####################################
####################################
####################################
# Permutation tests
# generate the results of permutation tests comparing mirror and oa in parent
source("./Rscript/functions_ms/permutation_tests_OA.R")
All_boots_wide <- permutation_tests_OA(AllData)

########################################
# SAMPLING WITHOUT REPLACEMENT
# THIS CHOOSES NUMBER OF PW ARTICLES BASED ON ALL OA (OA IN PW AND IN OA)
# ALL AUTHORS (solo and first)

# # Ecludes journal 22
# source("./Rscript/functions/DivRichCalc.R")
# AllData_no22_first <-AllData %>%
#   group_by(refID) %>%
#   slice(1) %>%
#   filter(pair_key!=22) %>%
#   filter(author=="coauthored")
# SamplingPW_no22_first<-bootstrap_PW(AllData_no22_first,"author_first")
# # 
# AllData_no22_solo <-AllData %>%
#   group_by(refID) %>%
#   slice(1) %>%
#   filter(pair_key!=22) %>%
#   filter(author=="solo")

# AllData_no22_first_OA<-AllData %>%
#   group_by(refID) %>%
#   slice(1) %>%
#   filter(pair_key!=22) %>%
#   filter(ArticleType=="OA")


# 
# AllData_no22_solo <-AllData %>%
#   group_by(refID) %>%
#   slice(1) %>%
#   filter(pair_key!=22) %>% # exclude only for overall, first
#   filter(pair_key!=16) %>% # for all
#   filter(pair_key!=11) %>% # for solo onlu
#   filter(pair_key!=28) %>% # for solo onlu
#   filter(pair_key!=36) %>% # for solo onlu
#   filter(pair_key!=30) %>% # for solo onlu
#   filter(pair_key!=32) %>% # for solo onlu
#   filter(pair_key!=38) %>% # for solo onlu
#   filter(author=="solo")
#   
# AllData_no22_solo$pair_key<-droplevels(AllData_no22_solo$pair_key)
# 
# AllData_no22_noCHNUSA_solo<-AllData_no22_solo %>% 
#   group_by(refID) %>% 
#   slice(1) %>% 
#   filter(pair_key!=5) %>% # for solo no chn usa only
#   filter(pair_key!=7) %>% # for solo no chn usa only
#   filter(pair_key!=1) %>% # for solo no chn usa only
#   filter(Code!="USA") %>% 
#   filter(Code!="CHN")  %>% 
#   filter(author=="solo")
# AllData_no22_noCHNUSA_solo$pair_key<-droplevels(AllData_no22_noCHNUSA_solo$pair_key)
# 
# source("./Rscript/functions/bootstrap_PW.R")
# Data_no22_solo<-bootstrap_PW(AllData_no22_solo,"author_first")
# Div_NoReplace_no22_solo<-as.data.frame(Data_no22_solo[1])
# Countries_NoReplace_no22_solo<-as.data.frame(Data_no22_solo[2])
# hist(Div_NoReplace_no22_solo$InvSimp)
# 
# source("./Rscript/functions/bootstrap_PW.R")
# Data_no22_noCHNUSA_solo<-bootstrap_PW(AllData_no22_noCHNUSA_solo,"author_first")
# Div_NoReplace_no22_NoChinaUSA_solo<-as.data.frame(Data_no22_noCHNUSA_solo[1])
# Countries_NoReplace_no22_NoChinaUSA_solo<-as.data.frame(Data_no22_noCHNUSA_solo[2])
# hist(Div_NoReplace_no22_NoChinaUSA_solo$InvSimp)
# 
# 
# NoReplacePW_RichDiv_solo<-bind_rows(Div_NoReplace_no22_solo,
#                                Div_NoReplace_no22_NoChinaUSA_solo)
# NoReplacePW_RichDiv_solo$BootType<-"solo"
# NoReplacePW_RichDiv_solo$data<-Sys.Date()
# 
# NoReplacePW_Countries_solo<-bind_rows(Countries_NoReplace_no22_solo,
#                                  Countries_NoReplace_no22_NoChinaUSA_solo)
# NoReplacePW_Countries_solo$BootType<-"solo"
# NoReplacePW_Countries_solo$date<-Sys.Date()
# 
# # summary(boot_results_countries)
# write.csv(NoReplacePW_Countries_solo, "./output/Boot_Countries_solo.csv", row.names = FALSE)
# # Boot_Countries<-read_csv("./output/Boot_Countries.csv")
# write.csv(NoReplacePW_RichDiv_solo,'./output/Boot_RichDiv_solo.csv',row.names = FALSE)
# # Boot_RichDiv<-read_csv("./output/Boot_RichDiv.csv")




# 
# results_OA_no22<-DivRichCalc(AllData_no22_OA,"author_first","both","OA")
# results_OA_no22[1]
# results_OA_no22[2]
# 
# results_OA_no22_noCHNUSA<-DivRichCalc(AllData_no22_noCHNUSA,"author_first","both","OA")
# results_OA_no22_noCHNUSA[1]
# results_OA_no22_noCHNUSA[2]
##############################
# 
# 
# AllData_no22_noCHNUSA_first<-AllData_no22_solo %>% 
#   filter(Code!="USA") %>% 
#   filter(Code!="CHN") 
# 
# 
# 
# 
# 
# 
# NoReplacePW_Countries
# NoReplacePW_RichDiv
# results_OA_no22[1]
# results_OA_no22[2]
# results_OA_no22_noCHNUSA[1]
# results_OA_no22_noCHNUSA[2]
# 
# 
# ######################################################################
# BootPW_SoloAll1<-bootstrap_PW(AllData_no22,"author_first")[1]
# BootPW_SoloAll2<-bootstrap_PW(AllData_no22,"author_first")[1]
# BootPW_SoloAll1<-as.data.frame(BootPW_SoloAll2)
# BootPW_SoloAll2<-as.data.frame(BootPW_SoloAll2)
# BootPW_SoloAll3<-bootstrap_PW(AllData_no22,"author_first")[1]
# BootPW_SoloAll4<-bootstrap_PW(AllData_no22,"author_first")[1]
# BootPW_SoloAll3<-as.data.frame(BootPW_SoloAll2)
# BootPW_SoloAll4<-as.data.frame(BootPW_SoloAll2)
# 
# DivRichCalc(AllData_no22,"author_first","both","OA")
# hist(BootPW_SoloAll1$InvSimp)
# hist(BootPW_SoloAll2$InvSimp)
# 
# hist(BootPW_SoloAll3$InvSimp)
# hist(BootPW_SoloAll4$InvSimp)
# 
# range(BootPW_SoloAll$InvSimp)
# range(BootPW_SoloAll2$InvSimp)
# range(BootPW_SoloAll3$InvSimp)
# range(BootPW_SoloAll4$InvSimp)
# 




# THIS WILL DO THE BOOTSTRAPPING BASED ON 
# THE NUMBER OF ARTICLES IN ***MIRROR JOURNALS***
  
# source("./Rscript/functions/bootstrap_PW.R")
# # SAMPLED DIV/RICH: SOLE AUTHOR PUBS, ALL COUNTRIES
# BootPW_SoloAll<-bootstrap_PW(sole_ALL,"author_first")
# # SAMPLED DIV/RICH: 1st Author of Coauthored Pubs, All Countries
# BootPW_CoAll<-bootstrap_PW(first_ALL,"author_first")
# # SAMPLED DIV/RICH: SOLE AUTHOR PUBS, NO USA OR CHINA
# # NO CHN AND USA REQUIRES SAMPLING WITH REPLACEMENT DUE TO SMALLER SAMPLE SIZES
# BootPW_SoloNoUSACHN<-bootstrap_PW(sole_NOCHNUSA,"author_first")
# # SAMPLED DIV/RICH: FIRST AUTHORS COAUTHORED PUBS, NO USA OR CHINA
# BootPW_CoNoUSACHN<-bootstrap_PW(first_NOCHNUSA,"author_first")
# 
# BootOA_PW_RichDiv<-bind_rows(BootPW_SoloAll[1],
#                                 BootPW_CoAll[1],
#                                 BootPW_SoloNoUSACHN[1],
#                                 BootPW_CoNoUSACHN[1])
# BootOA_PW_RichDiv$BootType<-"allOA"
# BootOA_PW_RichDiv$date_run<-Sys.Date()
# 
# BootOA_PW_Countries<-bind_rows(BootPW_SoloAll[2],
#                                   BootPW_CoAll[2],
#                                   BootPW_SoloNoUSACHN[2],
#                                   BootPW_CoNoUSACHN[2])
# BootOA_PW_Countries$BootType<-"allOA"
# BootOA_PW_Countries$date_run<-Sys.Date()
# 
# Boot_RichDiv<-BootOA_PW_RichDiv %>% 
#   # mutate(OA_group = ifelse(OA_group == "OA", "PW", author)) %>% 
#   mutate(author = ifelse(Dataset == "sole_NOCHNUSA", "solo", author)) %>% 
#   mutate(author = ifelse(Dataset == "sole_ALL", "solo", author)) %>% 
#   mutate(author = ifelse(Dataset == "first_NOCHNUSA", "author_first", author)) %>% 
#   mutate(author = ifelse(Dataset == "first_ALL", "author_first", author)) %>% 
#   # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset)) %>%
#   # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset))
#   mutate(Dataset = ifelse(Dataset == "sole_ALL", "all", Dataset)) %>%
#   mutate(Dataset = ifelse(Dataset == "first_ALL", "all", Dataset)) %>%
#   mutate(Dataset = ifelse(Dataset == "sole_NOCHNUSA","no_CHN_USA", Dataset)) %>%
#   mutate(Dataset = ifelse(Dataset == "first_NOCHNUSA","no_CHN_USA", Dataset))
# Boot_RichDiv$Dataset<-as.factor(Boot_RichDiv$Dataset)
# Boot_RichDiv$author<-as.factor(Boot_RichDiv$author)
# # as.factor(Boot_RichDiv$ArticleCat)
# 
# Boot_Countries<-BootOA_PW_Countries %>%
#   # mutate(OA_group = ifelse(OA_group == "OA", "PW", author)) %>% 
#   mutate(author = ifelse(Dataset == "sole_NOCHNUSA", "solo", author)) %>% 
#   mutate(author = ifelse(Dataset == "sole_ALL", "solo", author)) %>% 
#   mutate(author = ifelse(Dataset == "first_NOCHNUSA", "author_first", author)) %>% 
#   mutate(author = ifelse(Dataset == "first_ALL", "author_first", author)) %>% 
#   # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset)) %>%
#   # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset))
#   mutate(Dataset = ifelse(Dataset == "sole_ALL", "all", Dataset)) %>%
#   mutate(Dataset = ifelse(Dataset == "first_ALL", "all", Dataset)) %>%
#   mutate(Dataset = ifelse(Dataset == "sole_NOCHNUSA","no_CHN_USA", Dataset)) %>%
#   mutate(Dataset = ifelse(Dataset == "first_NOCHNUSA","no_CHN_USA", Dataset))
# # 
# summary(as.factor(Boot_RichDiv$author))
# summary(as.factor(Boot_RichDiv$Dataset))
# summary(as.factor(Boot_RichDiv$ArticleCat))
# 
# Boot_Countries$author<-as.factor(Boot_Countries$author)
# Boot_Countries$Dataset<-as.factor(Boot_Countries$Dataset)
# 
# # summary(boot_results_countries)
# write.csv(Boot_Countries, "./output/Boot_Countries.csv", row.names = FALSE)
# # Boot_Countries<-read_csv("./output/Boot_Countries.csv")
# write.csv(Boot_RichDiv,'./output/Boot_RichDiv.csv',row.names = FALSE)
# # Boot_RichDiv<-read_csv("./output/Boot_RichDiv.csv")



source("./Rscript/functions_ms/RegionPlot_MSv2.R")
# countries= "all" or "no_CHNUSA"
RegionPlot_MSv2<-RegionPlot_MSv2(Boot_RichDiv,
                          Boot_Countries,
                          "All",
                          sole_ALL,
                          sole_NOCHNUSA,
                          first_ALL,
                          first_NOCHNUSA)
RegionPlot_MSv2[2]

source("./Rscript/functions_ms/IncomePlot_MSv2.R")
# countries= "all" or "no_CHNUSA"
IncomePlot_MSv2<-IncomePlot_MSv2(Boot_RichDiv,
                                 Boot_Countries,
                                 "All",
                                 sole_ALL,
                                 sole_NOCHNUSA,
                                 first_ALL,
                                 first_NOCHNUSA)
IncomePlot_MSv2[2]
##################
##################
##################
##################


#####################
source("./Rscript/functions_ms/WaiverGrpPlot_MSv2.R")
# countries= "all" or "no_CHNUSA"
WaiverGrpPlot_MSv2<-WaiverGrpPlot_MSv2(BootMirror_RichDiv,
                                       BootMirror_Countries,
                                       "All",
                                       sole_ALL,
                                       sole_NOCHNUSA,
                                       first_ALL,
                                       first_NOCHNUSA,
                                       WaiverCountries)
WaiverGrpPlot_MSv2[2]
WaiverGrpPlot_MSv2[1]

WaiverPlot<-WaiverGrpPlot_MSv2(BootMirror_RichDiv,BootMirror_Countries,"All",sole_ALL,sole_NOCHNUSA,first_ALL,first_NOCHNUSA,WaiverCountries)


##########################################
# SAMPLED DIV/RICH: OAinPW vs PW
##########################################

source("./Rscript/functions_ms/bootstrap_OA_in_PW.R")
Boot_OAinPW_SoloAll<-bootstrap_OA_in_PW(sole_ALL,"author_first","OAinPW")
Boot_OAinPW_CoAll<-bootstrap_OA_in_PW(first_ALL,"author_first","OAinPW")
Boot_OAinPW_SoloNoUSACHN<-bootstrap_OA_in_PW(sole_NOCHNUSA,"author_first","OAinPW")
Boot_OAinPW_CoNoUSACHN<-bootstrap_OA_in_PW(first_NOCHNUSA,"author_first","OAinPW")


BootOAinPW_RichDiv<-bind_rows(Boot_OAinPW_SoloAll[1],
                             Boot_OAinPW_CoAll[1],
                             Boot_OAinPW_SoloNoUSACHN[1],
                             Boot_OAinPW_CoNoUSACHN[1])
BootOAinPW_RichDiv$BootType<-"OAinPWvPW"

BootOAinPW_Countries<-bind_rows(Boot_OAinPW_SoloAll[2],
                               Boot_OAinPW_CoAll[2],
                               Boot_OAinPW_SoloNoUSACHN[2],
                               Boot_OAinPW_CoNoUSACHN[2])
BootOAinPW_Countries$BootType<-"OAinPWvPW"


BootOAinPW_RichDiv<-BootOAinPW_RichDiv %>% 
  # mutate(OA_group = ifelse(OA_group == "OA", "PW", author)) %>% 
  mutate(author = ifelse(Dataset == "sole_NOCHNUSA", "solo", author)) %>% 
  mutate(author = ifelse(Dataset == "sole_ALL", "solo", author)) %>% 
  mutate(author = ifelse(Dataset == "first_NOCHNUSA", "author_first", author)) %>% 
  mutate(author = ifelse(Dataset == "first_ALL", "author_first", author)) %>% 
  # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset)) %>%
  # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset))
  mutate(Dataset = ifelse(Dataset == "sole_ALL", "All Countries", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "first_ALL", "All Countries", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "sole_NOCHNUSA","CHN & USA excluded", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "first_NOCHNUSA","CHN & USA excluded", Dataset))
as.factor(BootOAinPW_RichDiv$Dataset)
as.factor(BootOAinPW_RichDiv$author)


BootOAinPW_Countries<-BootOAinPW_Countries %>%
  # mutate(OA_group = ifelse(OA_group == "OA", "PW", author)) %>% 
  mutate(author = ifelse(Dataset == "sole_NOCHNUSA", "solo", author)) %>% 
  mutate(author = ifelse(Dataset == "sole_ALL", "solo", author)) %>% 
  mutate(author = ifelse(Dataset == "first_NOCHNUSA", "author_first", author)) %>% 
  mutate(author = ifelse(Dataset == "first_ALL", "author_first", author)) %>% 
  # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset)) %>%
  # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset))
  mutate(Dataset = ifelse(Dataset == "sole_ALL", "All Countries", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "first_ALL", "All Countries", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "sole_NOCHNUSA","CHN & USA excluded", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "first_NOCHNUSA","CHN & USA excluded", Dataset))
# 
summary(as.factor(BootOAinPW_Countries$author))
summary(as.factor(BootOAinPW_Countries$Dataset))
# summary(as.factor(Boot_RichDiv$ArticleCat))

summary(as.factor(BootOAinPW_Countries$author))
summary(as.factor(BootOAinPW_Countries$Dataset))
summary(as.factor(BootOAinPW_Countries$Dataset))


write_csv(BootOAinPW_RichDiv, "./output/BootOAinPW_RichDiv.csv")
write_csv(BootOAinPW_Countries,'./output/BootOAinPW_Countries.csv')

###############################
source("./Rscript/functions_ms/bootstrap_OA_in_PW.R")
Boot_mirror_SoloAll<-bootstrap_OA_in_PW(sole_ALL,"author_first","mirror")
Boot_mirror_CoAll<-bootstrap_OA_in_PW(first_ALL,"author_first","mirror")
Boot_mirror_SoloNoUSACHN<-bootstrap_OA_in_PW(sole_NOCHNUSA,"author_first","mirror")
Boot_mirrow_CoNoUSACHN<-bootstrap_OA_in_PW(first_NOCHNUSA,"author_first","mirror")


BootMirror_RichDiv<-bind_rows(Boot_mirror_SoloAll[1],
                              Boot_mirror_CoAll[1],
                              Boot_mirror_SoloNoUSACHN[1],
                              Boot_mirrow_CoNoUSACHN[1])
BootMirror_RichDiv$BootType<-"MirrorvPW"

BootMirror_Countries<-bind_rows(Boot_mirror_SoloAll[2],
                                Boot_mirror_CoAll[2],
                                Boot_mirror_SoloNoUSACHN[2],
                                Boot_mirrow_CoNoUSACHN[2])
BootMirror_Countries$BootType<-"MirrorvPW"


BootMirror_RichDiv<-BootMirror_RichDiv %>% 
  # mutate(OA_group = ifelse(OA_group == "OA", "PW", author)) %>% 
  mutate(author = ifelse(Dataset == "sole_NOCHNUSA", "solo", author)) %>% 
  mutate(author = ifelse(Dataset == "sole_ALL", "solo", author)) %>% 
  mutate(author = ifelse(Dataset == "first_NOCHNUSA", "author_first", author)) %>% 
  mutate(author = ifelse(Dataset == "first_ALL", "author_first", author)) %>% 
  # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset)) %>%
  # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset))
  mutate(Dataset = ifelse(Dataset == "sole_ALL", "All Countries", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "first_ALL", "All Countries", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "sole_NOCHNUSA","CHN & USA excluded", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "first_NOCHNUSA","CHN & USA excluded", Dataset))
as.factor(BootMirror_RichDiv$Dataset)
as.factor(BootMirror_RichDiv$author)


BootMirror_Countries<-BootMirror_Countries %>%
  # mutate(OA_group = ifelse(OA_group == "OA", "PW", author)) %>% 
  mutate(author = ifelse(Dataset == "sole_NOCHNUSA", "solo", author)) %>% 
  mutate(author = ifelse(Dataset == "sole_ALL", "solo", author)) %>% 
  mutate(author = ifelse(Dataset == "first_NOCHNUSA", "author_first", author)) %>% 
  mutate(author = ifelse(Dataset == "first_ALL", "author_first", author)) %>% 
  # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset)) %>%
  # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset))
  mutate(Dataset = ifelse(Dataset == "sole_ALL", "All Countries", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "first_ALL", "All Countries", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "sole_NOCHNUSA","CHN & USA excluded", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "first_NOCHNUSA","CHN & USA excluded", Dataset))
# 
summary(as.factor(BootMirror_Countries$author))
summary(as.factor(BootMirror_Countries$Dataset))
# summary(as.factor(Boot_RichDiv$ArticleCat))

summary(as.factor(BootMirror_Countries$author))
summary(as.factor(BootMirror_Countries$Dataset))
summary(as.factor(BootMirror_Countries$Dataset))




write_csv(BootMirror_RichDiv, "./output/BootMirror_RichDiv.csv")
write_csv(BootMirror_Countries,'./output/BootMirror_Countries.csv')
# Boot_RichDiv<-read_csv("./output/Boot_RichDiv.csv")
# Boot_Countries<-read_csv("./output/Boot_Countries.csv")







































# ############################
# # SAMPLED DIV/RICH: Mirror OA vs OAinPW 
# # note - that no structuring by Journal because not always enough to do that
# source("./Rscript/functions/bootstrap_MirrorOA_v_OAinPW.R")
# Boot_MirrorvOA_SoloAll<-bootstrap_MirrorOA_v_OAinPW(sole_ALL,"author_first")
# Boot_MirrorvOA_CoAll<-bootstrap_MirrorOA_v_OAinPW(first_ALL,"author_first")
# Boot_MirrorvOA_SoloNoUSACHN<-bootstrap_MirrorOA_v_OAinPW(sole_NOCHNUSA,"author_first")
# Boot_MirrorvOA_CoNoUSACHN<-bootstrap_MirrorOA_v_OAinPW(first_NOCHNUSA,"author_first")
# 
# Boot_MirrorvOA_RichDiv<-bind_rows(Boot_MirrorvOA_SoloAll[1],
#                                   Boot_MirrorvOA_CoAll[1],
#                                   Boot_MirrorvOA_SoloNoUSACHN[1],
#                                   Boot_MirrorvOA_CoNoUSACHN[1])
# Boot_MirrorvOA_RichDiv$BootType<-"MirrorvOAinPW"
# 
# Boot_MirrorvOA_Countries<-bind_rows(Boot_MirrorvOA_SoloAll[2],
#                                     Boot_MirrorvOA_CoAll[2],
#                                     Boot_MirrorvOA_SoloNoUSACHN[2],
#                                     Boot_MirrorvOA_CoNoUSACHN[2])
# BootOAinPW_Countries$BootType<-"MirrorvOAinPW"
# 
# 
# Boot_RichDiv<-bind_rows(
#                         BootOA_PW_RichDiv,
#                         # Boot_mirror_RichDiv,
#                         BootOAinPW_RichDiv,
#                         Boot_MirrorvOA_RichDiv)
# Boot_Countries<-bind_rows(
#                           BootOA_PW_Countries,
#                           # Boot_mirror_Countries,
#                           BootOAinPW_Countries,
#                           Boot_MirrorvOA_Countries)
# 
# 
# 
# Boot_RichDiv<-Boot_RichDiv %>% 
#   # mutate(OA_group = ifelse(OA_group == "OA", "PW", author)) %>% 
#   mutate(author = ifelse(Dataset == "sole_NOCHNUSA", "solo", author)) %>% 
#   mutate(author = ifelse(Dataset == "sole_ALL", "solo", author)) %>% 
#   mutate(author = ifelse(Dataset == "first_NOCHNUSA", "author_first", author)) %>% 
#   mutate(author = ifelse(Dataset == "first_ALL", "author_first", author)) %>% 
#   # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset)) %>%
#   # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset))
#   mutate(Dataset = ifelse(Dataset == "sole_ALL", "All Countries", Dataset)) %>%
#   mutate(Dataset = ifelse(Dataset == "first_ALL", "All Countries", Dataset)) %>%
#   mutate(Dataset = ifelse(Dataset == "sole_NOCHNUSA","CHN & USA excluded", Dataset)) %>%
#   mutate(Dataset = ifelse(Dataset == "first_NOCHNUSA","CHN & USA excluded", Dataset))
# as.factor(Boot_RichDiv$Dataset)
# as.factor(Boot_RichDiv$author)
# as.factor(Boot_RichDiv$ArticleCat)
# 
# Boot_Countries<-Boot_Countries %>%
#   # mutate(OA_group = ifelse(OA_group == "OA", "PW", author)) %>% 
#   mutate(author = ifelse(Dataset == "sole_NOCHNUSA", "solo", author)) %>% 
#   mutate(author = ifelse(Dataset == "sole_ALL", "solo", author)) %>% 
#   mutate(author = ifelse(Dataset == "first_NOCHNUSA", "author_first", author)) %>% 
#   mutate(author = ifelse(Dataset == "first_ALL", "author_first", author)) %>% 
#   # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset)) %>%
#   # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset))
#   mutate(Dataset = ifelse(Dataset == "sole_ALL", "All Countries", Dataset)) %>%
#   mutate(Dataset = ifelse(Dataset == "first_ALL", "All Countries", Dataset)) %>%
#   mutate(Dataset = ifelse(Dataset == "sole_NOCHNUSA","CHN & USA excluded", Dataset)) %>%
#   mutate(Dataset = ifelse(Dataset == "first_NOCHNUSA","CHN & USA excluded", Dataset))
# # 
# summary(as.factor(Boot_RichDiv$author))
# summary(as.factor(Boot_RichDiv$Dataset))
# summary(as.factor(Boot_RichDiv$ArticleCat))
# 
# summary(as.factor(Boot_Countries$author))
# summary(as.factor(Boot_Countries$Dataset))
# summary(as.factor(Boot_Countries$Dataset))


# summary(boot_results_countries)
write.csv(Boot_Countries, "./output/Boot_Countries.csv", row.names = FALSE)
# Boot_Countries<-read_csv("./output/Boot_Countries.csv")
write.csv(NoReplacePW_RichDiv,'./output/Boot_RichDiv.csv',row.names = FALSE)
# Boot_RichDiv<-read_csv("./output/Boot_RichDiv.csv")
#############################################
# AUTHOR DIVERSITY & RICHNESS: ALL PAPERS POOLED
#################################################

source("./Rscript/functions_ms/DivRichCalcTable_Solo.R")
# Table2_Solo_PW<-DivRichCalcTable_Solo(sole_ALL,
#                                    sole_NOCHNUSA,
#                                    BootPW_SoloAll[1],
#                                    BootPW_SoloNoUSACHN[1],
#                                    "OA")
source("./Rscript/functions_ms/DivRichCalcTable_Solo.R")
Table2_Solo_PW<-DivRichCalcTable_Solo(sole_ALL,
                                      sole_NOCHNUSA,
                                      BootPW_SoloAll[1],
                                      BootPW_SoloNoUSACHN[1],
                                      "OA")
Table2_Solo_PW
# source("./Rscript/functions/DivRichCalcSummaryTable_sampled.R")
# Table2_CoAuthored_PW<-DivRichCalcSummaryTable_sampled(first_ALL,
#                                                    first_NOCHNUSA,
#                                                    BootPW_CoAll[1],
#                                                    BootPW_CoNoUSACHN[1],
#                                                    "OA")

source("./Rscript/functions_ms/DivRichCalcSummaryTable_sampled.R")
Table2_CoAuthored_PW<-DivRichCalcSummaryTable_sampled(first_ALL,
                                                      first_NOCHNUSA,
                                                      BootPW_CoAll[1],
                                                      BootPW_CoNoUSACHN[1],
                                                      "OA")
Table2_PW<-bind_rows(Table2_CoAuthored_PW,Table2_Solo_PW)
# 
# source("./Rscript/functions/DivRichCalcTable_Solo.R")
# Table2_Solo_OAinPW<-DivRichCalcTable_Solo(sole_ALL,
#                                    sole_NOCHNUSA,
#                                    BootOAinPW_SoloAll[1],
#                                    BootOAinPW_SoloNoUSACHN[1])
# source("./Rscript/functions/DivRichCalcSummaryTable_sampled.R")
# Table2_CoAuthored_OAinPW<-DivRichCalcSummaryTable_sampled(first_ALL,
#                                                    first_NOCHNUSA,
#                                                    BootOAinPW_CoAll[1],
#                                                    BootOAinPW_CoNoUSACHN[1])

# Table2_OAinPW<-bind_rows(Table2_CoAuthored_OAinPW,Table2_Solo_OAinPW) %>% 
#   dplyr::rename("OAinPW_AllCountries"="PW_AllCountries", "OAinPW_noCHNorUSA"="PW_noUSAorCHN")
# 


write.csv(Table2_PW, "./tables_figs/Table2_PW.csv", row.names = FALSE)
# Table2_Joint<-read_csv("./tables_figs/Table2.csv")
write.csv(Table2_OAinPW, "./tables_figs/Table2_OAinPW.csv", row.names = FALSE)



# Table 2  OA v PW
source("./Rscript/functions_figures/Table2.R")

Table2_PW<-Table2(sole_ALL,
                sole_NOCHNUSA,
                first_ALL,
                first_NOCHNUSA,
                BootPW_SoloAll,
                BootPW_SoloNoUSACHN,
                BootPW_CoAll,
                BootPW_CoNoUSACHN,
                "OA")
names(Table2_PW)<-c("Metric","Author","OA","Mean PW",
                    "PW 95% CI", "OA","Mean PW","PW 95% CI",
                    "P_Hat","P_Hat")
Table2_PW





# Table 2 OA v OAinPW
source("./Rscript/functions_ms/Table2_Mirror_v_OAinPW.R")
Table2_OAinPW<-Table2_Mirror_v_OAinPW(AllData,
                                      Boot_MirrorvOA_SoloAll,
                                      Boot_MirrorvOA_SoloNoUSACHN,
                                      Boot_MirrorvOA_CoAll,
                                      Boot_MirrorvOA_CoNoUSACHN,
                                      "OA")
Table2_OAinPW


# FIGURE 2

BootOAinPW_RichDiv<-bind_rows(Boot_MirrorvOA_SoloAll[1],
                              Boot_MirrorvOA_SoloNoUSACHN[1],
                              Boot_MirrorvOA_CoAll[1],
                              Boot_MirrorvOA_CoNoUSACHN[1])
BootOAinPW_RichDiv$ArticleCat<-"OAinPW"



BootOAinPW_RichDiv<-BootOAinPW_RichDiv %>% 
  # mutate(OA_group = ifelse(OA_group == "OA", "PW", author)) %>% 
  mutate(author = ifelse(Dataset == "sole_NOCHNUSA", "solo", author)) %>% 
  mutate(author = ifelse(Dataset == "sole_ALL", "solo", author)) %>% 
  mutate(author = ifelse(Dataset == "first_NOCHNUSA", "author_first", author)) %>% 
  mutate(author = ifelse(Dataset == "first_ALL", "author_first", author)) %>% 
  # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset)) %>%
  # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset))
  mutate(Dataset = ifelse(Dataset == "sole_ALL", "All Countries", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "first_ALL", "All Countries", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "sole_NOCHNUSA","CHN & USA excluded", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "first_NOCHNUSA","CHN & USA excluded", Dataset))
as.factor(BootOAinPW_RichDiv$Dataset)
as.factor(BootOAinPW_RichDiv$author)
as.factor(BootOAinPW_RichDiv$ArticleCat)


sole_ALL_Mirror <- AllData %>%
  filter(author=="solo") %>% 
  filter(JrnlType=="OA") %>% 
  mutate(Dataset="All Countries") %>% 
  drop_na("Code")
AuPosition<-"author_first"
ArticleType<-"OA"
Div_OA_solo<-DivRichCalc(sole_ALL_Mirror,AuPosition,ArticleType)

# Binding together
DivMetrics_Mirror_solo <- as.data.frame(cbind(Div_OA_solo[1],
                                              Div_OA_solo[2]))
DivMetrics_Mirror_solo<-DivMetrics_Mirror_solo %>% 
  dplyr::rename(OA_richness_solo=V1,
                OA_invSimp_solo=V2)

# Mirror OA STATS - SOLO-NO CHN USA
sole_NOCHNUSA_Mirror <- AllData %>%
  filter(author=="solo") %>%
  filter(JrnlType=="OA") %>% 
  filter(Code!="CHN") %>% 
  filter(Code!="USA") %>% 
  mutate(Dataset="CHN & USA excluded")


first_ALL_Mirror<- AllData %>%
  filter(author=="coauthored") %>% 
  filter(ArticleType=="OA") %>% 
  filter(JrnlType=="OA") %>% 
  filter(AuthorNum==1) %>% 
  mutate(author="author_first") 



first_NOCHNUSA_Mirror<- AllData %>%
  filter(author=="coauthored") %>% 
  filter(AuthorNum==1) %>% 
  filter(ArticleType=="OA") %>% 
  filter(JrnlType=="OA") %>% 
  filter(Code!="CHN") %>% 
  filter(Code!="USA") %>% 
  mutate(Dataset="CHN & USA excluded") %>% 
  mutate(author="author_first")
source("./Rscript/functions_figures/DivBootFig_MSv2.R")
DivBootFig_MSv2(BootOAinPW_RichDiv,sole_ALL_Mirror,first_ALL_Mirror,sole_NOCHNUSA_Mirror, first_NOCHNUSA_Mirror)





################
##############################################################
# END 30 NOV
##############################################################


OADivRichSolo<-DivRichCalc(sole_ALL,"author_first","OA")
OAdivFA<-as.numeric((OADivRichSolo)[2])
OARichFA<-as.numeric((OADivRichSolo)[1])

OADivRichSoloNo<-DivRichCalc(sole_NOCHNUSA,"author_first","OA")
OAdivFANo<-as.numeric((OADivRichSoloNo)[2])
OARichFANo<-as.numeric((OADivRichSoloNo)[1])

OADivRichFA<-DivRichCalc(coauthor_ALL,"author_first","OA")
OAdivFA<-as.numeric((OADivRichFA)[2])
OARichFA<-as.numeric((OADivRichFA)[1])

OADivRichFN<-DivRichCalc(coauthor_NOCHNUSA,"author_first","OA")
OAdivFN<-as.numeric((OADivRichFN)[2])
OARichFN<-as.numeric((OADivRichFN)[1])













# bootstrap_results<-bind_rows(bootstrapped_PW_combined,
#                              bootstrapped_OA_combined)

summary(bootstrap_results)
write.csv(bootstrap_results,'./output/bootstrap_results.csv',row.names = FALSE)

# bootstrap_results<-read_csv('./output/bootstrap_results.csv')
#################

# New facet label names for country variable
# not needed 
Table2<-ungroup(Table2_PW)
Table2<-Table2 %>% arrange(desc(metric),author)
colnames(Table2)
colnames(bootstrap_results_fig_data)

author<-rep(Table2$author,1,strings.as.factors=TRUE)
# metric<-rep(c("Richness","Richness","Richness","Diversity","Diversity","Diversity"),4)

CIlow_Rich<-c(rep(NA,4),Table2$CIlow[1:2],Table2$CIlow1[1:2])
CIlow_Div<-c(rep(NA,4),Table2$CIlow[3:4],Table2$CIlow1[3:4])
CIhigh_Rich<-c(rep(NA,4),Table2$CIhigh[1:2],Table2$CIhigh1[1:2])
CIhigh_Div<-c(rep(NA,4),Table2$CIhigh[3:4],Table2$CIhigh1[3:4])

#add the means, remove the +/- and everything after, convert to numeric
Mean_Rich<-c(Table2$PW_AllCountries[1:2],Table2$PW_noUSAorCHN[1:2])
Mean_Rich<-gsub(" ","",Mean_Rich)
Mean_Rich<-gsub("\\+.*","",Mean_Rich)
Mean_Rich<-as.numeric(Mean_Rich)
Mean_Rich<-c(rep(NA,4),Mean_Rich)

Mean_Div<-c(Table2$PW_AllCountries[3:4],Table2$PW_noUSAorCHN[3:4])
Mean_Div<-gsub(" ","",Mean_Div)
Mean_Div<-gsub("\\+.*","",Mean_Div)
Mean_Div<-as.numeric(Mean_Div)
Mean_Div<-c(rep(NA,4),Mean_Div)

RichnessOA<-c(as.numeric(Table2$OA_AllCountries[1:2]),as.numeric(Table2$OA_noCHNorUSA[1:2]),
              rep(NA,4))
InvSimpOA<-c(as.numeric(Table2$OA_AllCountries[3:4]),as.numeric(Table2$OA_noCHNorUSA[3:4]),
             rep(NA,4))

JrnlType<-c(rep("OA",4),rep("PW",4))
labeltext <- rep(c("OA['obs']","PW['boot. mean']"), each=2)

dataset<-rep((rep(c("All Countries","CHN & USA excluded"),each=2)),1)
Table2.2<-as.data.frame(cbind(author=as.character(author),
                              Dataset=as.character(dataset),
                              JrnlType,InvSimpOA,RichnessOA,
                              Mean_Div,
                              Mean_Rich,
                              CIlow_Rich,CIhigh_Rich,CIlow_Div,
                              CIhigh_Div,labeltext),
                        stringsAsFactors = TRUE)


colnames(bootstrap_results_fig_data)
str(Table2.2)
Table2.2$RichnessOA<-as.numeric(as.character(Table2.2$RichnessOA))
Table2.2$InvSimpOA<-as.numeric(as.character(Table2.2$InvSimpOA))
Table2.2$Mean_Rich<-as.numeric(as.character(Table2.2$Mean_Rich))
Table2.2$Mean_Div<-as.numeric(as.character(Table2.2$Mean_Div))

str(Table2.2$author)
# bootstrap_results_fig_data$author
Table2.2$author<-ordered(Table2.2$author, levels = c("solo", "first"))

# brewer.pal(3, "Set1") gets the hex codes from palette
# [1] "#E41A1C" "#377EB8" "#4DAF4A"

str(Table2.2)

Table2.2$CIlow_Div<-as.integer(as.character(Table2.2$CIlow_Div))
Table2.2$CIlow_Rich<-as.integer(as.character(Table2.2$CIlow_Rich))
Table2.2$CIhigh_Div<-as.integer(as.character(Table2.2$CIhigh_Div))
Table2.2$CIhigh_Rich<-as.integer(as.character(Table2.2$CIhigh_Rich))
Table2.2$facet_title<-paste(Table2.2$author,Table2.2$Dataset,sep=' , ')
str(Table2.2)
Table2.2$author<-as.factor(Table2.2$author)

write.csv(Table2.2,'./output/Table2.2.csv',row.names = FALSE)
# Table2.2<-read_csv('./output/Table2.2.csv')








###################
# Figure - Diversity
source("./Rscript/functions_figures/DivBootFig.R")
DivBootFig(bootstrap_results)


# numbers for the paper = countries by category, waivers, etc. 
# DIVERSITY
# significance test (prop below)
# 
# # single, all
# crit3<-as.numeric(Table2.2[2,4])
# percPWlessthanOA3<-bootstrap_results %>% 
#   filter(Dataset=="All Countries") %>% 
#   filter(author=="solo") %>% 
#   tally(InvSimp>crit3)/1000
# percPWlessthanOA3
# rm(crit3)
# 
# # first of coauthored, all
# crit1<-as.numeric(Table2.2[1,4])
# percPWlessthanOA1<-bootstrap_results %>% 
#        filter(Dataset=="All Countries") %>% 
#        filter(author=="author_first") %>%
#         tally(InvSimp>crit1)/1000
# rm(crit1)
# percPWlessthanOA1
# 
# # single, no CHN USA
# crit4<-as.numeric(Table2.2[4,4])
# percPWlessthanOA4<-bootstrap_results %>% 
#        filter(Dataset=="CHN & USA excluded") %>% 
#        filter(author=="solo") %>% 
#        tally(InvSimp>crit4)/1000
# percPWlessthanOA4
# rm(crit4)
# 
# 
# # first of coauthored, no CHN USA
# crit2<-as.numeric(Table2.2[3,4])
# percPWlessthanOA2<-percPWlessthanOA<-bootstrap_results %>% 
#   filter(Dataset=="CHN & USA excluded") %>% 
#   filter(author=="author_first") %>% 
#   tally(InvSimp>crit2)/1000
# percPWlessthanOA2
# rm(crit2)
# 
# col1<-rep(NA,4)
# col2<-rep(NA,4)
# probs<-bind_cols(col1,col2)
# names(probs)<-c("all","without")
# 
# probs$all<-as.numeric(probs$all)
# probs$without<-as.numeric(probs$without)
# probs[4,1]<-percPWlessthanOA1
# probs[4,2]<-percPWlessthanOA2
# probs[3,1]<-percPWlessthanOA3
# probs[3,2]<-percPWlessthanOA4
# probs
# # RICHNESS
# # significance test (prop below)
# 
# 
# # single, all
# crit7<-as.numeric(Table2.2[2,5])
# percPWlessthanOA7<-bootstrap_results %>% 
#   filter(Dataset=="All Countries") %>% 
#   filter(author=="solo") %>% 
#   tally(Richness<38)/1000
# percPWlessthanOA7
# rm(crit7)
# 
# # first of coauthored, all
# crit5<-as.numeric(Table2.2[1,5])
# percPWlessthanOA5<-bootstrap_results %>% 
#        filter(Dataset=="All Countries") %>% 
#        filter(author=="author_first") %>% 
#        tally(Richness<crit5)/1000
# percPWlessthanOA5
# rm(crit5)
# 
# 
# 
# # single, no CHN USA
# crit8<-as.numeric(Table2.2[4,5])
# percPWlessthanOA8<-bootstrap_results %>% 
#   filter(Dataset=="CHN & USA excluded") %>% 
#   filter(author=="solo") %>% 
#   tally(Richness<36)/1000
# percPWlessthanOA8
# rm(crit8)
# 
# 
# 
# # first of coauthored, no CHN USA
# crit6<-as.numeric(Table2.2[3,5])
# percPWlessthanOA6<-
# bootstrap_results %>% 
#        filter(Dataset=="CHN & USA excluded") %>% 
#        filter(author=="author_first") %>% 
#        tally(Richness<crit6)/1000
# percPWlessthanOA6
# rm(crit6)
# 
# probs[2,1]<-percPWlessthanOA5
# probs[2,2]<-percPWlessthanOA8
# probs[1,1]<-percPWlessthanOA7
# probs[1,2]<-percPWlessthanOA6
# probs




SubsampledPW.results_Solo<-read_csv('./output/SubsampledPW.results_RichDiv_SOLO_ALL.csv')
SubsampledPW.results_First<-read_csv('./output/SubsampledPW.results_CO_ALL.csv')
SubsampledPW.results_Solo_NoUSACHN<-read_csv('output/SubsampledPW.results_RichDiv_SOLO_NoUSACHN.csv')
SubsampledPW.results_First_NoUSACHN<-read_csv('output/SubsampledPW.results_RichDiv_CO_NOUSACHN.csv')

sole_ALL<-read_csv("./data_clean/sole_author_pubs_ALL_first_author.csv")
sole_NOCHNUSA<-read_csv("./data_clean/one_author_pubsNOCHNUSA.csv")
coauthor_ALL<-read_csv("./data_clean/coauthor_pubs_ALL_first_author.csv")
coauthor_NOCHNUSA<-read_csv("./data_clean/coauthor_pubsNOCHNUSA.csv")

source("./Rscript/functions_ms/DivRichCalc.R")
crit_solo_all<-DivRichCalc(sole_ALL,"author_first","OA")
crit_solo_no_CHNUSA<-DivRichCalc(sole_NOCHNUSA,"author_first","OA")
crit_first_all<-DivRichCalc(coauthor_ALL,"author_first","OA")
crit_first_no_CHNUSA<-DivRichCalc(coauthor_NOCHNUSA,"author_first","OA")

# SOLO, ALL, RICH
R_crit_solo_all<-as.numeric(crit_solo_all[1])
perc_R_SOLO_ALL<-SubsampledPW.results_Solo %>% 
  tally(Richness>R_crit_solo_all)/1000
perc_R_SOLO_ALL
perc_R_SOLO_ALL$Author<-"Single"
perc_R_SOLO_ALL$Dataset<-"All Countries"
perc_R_SOLO_ALL$Metric<-"Richness"

# SOLO, NO CHN USA, RICH
R_crit_solo_no_CHNUSA<-as.numeric(crit_solo_no_CHNUSA[1])
perc_R_SOLO_NOCHNUSA<-SubsampledPW.results_Solo_NoUSACHN %>% 
  tally(Richness>R_crit_solo_no_CHNUSA)/1000
perc_R_SOLO_NOCHNUSA
perc_R_SOLO_NOCHNUSA$Author<-"Single"
perc_R_SOLO_NOCHNUSA$Dataset<-"Without China and USA"
perc_R_SOLO_NOCHNUSA$Metric<-"Richness"



# FIRST, ALL, RICH
R_crit_first_all<-as.numeric(crit_first_all[1])
perc_R_first_all<-SubsampledPW.results_First %>% 
  tally(Richness>R_crit_first_all)/1000
perc_R_first_all
perc_R_first_all$Author<-"First"
perc_R_first_all$Dataset<-"All Countries"
perc_R_first_all$Metric<-"Richness"


# FIRST, NO CHN USA, RICH
R_crit_first_no_CHNUSA<-as.numeric(crit_first_no_CHNUSA[1])
perc_R_first_NOCHNUSA<-SubsampledPW.results_First_NoUSACHN %>% 
  tally(Richness>R_crit_first_no_CHNUSA)/1000
perc_R_first_NOCHNUSA
perc_R_first_NOCHNUSA$Author<-"First"
perc_R_first_NOCHNUSA$Dataset<-"Without China and USA"
perc_R_first_NOCHNUSA$Metric<-"Richness"


# SOLO, ALL, DIV
Div_crit_solo_all<-as.numeric(crit_solo_all[2])
perc_Div_SOLO_ALL<-SubsampledPW.results_Solo %>% 
  tally(InvSimp>Div_crit_solo_all)/1000
perc_Div_SOLO_ALL
perc_Div_SOLO_ALL$Author<-"Single"
perc_Div_SOLO_ALL$Dataset<-"All Countries"
perc_Div_SOLO_ALL$Metric<-"Diversity"

# SOLO, NO CHN USA, DIV
Div_crit_solo_no_CHNUSA<-as.numeric(crit_solo_no_CHNUSA[2])
perc_D_SOLO_NOCHNUSA<-SubsampledPW.results_Solo_NoUSACHN %>% 
  tally(InvSimp>Div_crit_solo_no_CHNUSA)/1000
perc_D_SOLO_NOCHNUSA
perc_D_SOLO_NOCHNUSA$Author<-"Single"
perc_D_SOLO_NOCHNUSA$Dataset<-"Without China and USA"
perc_D_SOLO_NOCHNUSA$Metric<-"Diversity"



# FIRST, ALL, DIV
Div_crit_first_all<-as.numeric(crit_first_all[2])
perc_Div_first_ALL<-SubsampledPW.results_First %>% 
  tally(InvSimp>Div_crit_first_all)/1000
perc_Div_first_ALL<-perc_Div_first_ALL
perc_Div_first_ALL$Author<-"First"
perc_Div_first_ALL$Dataset<-"Without China & USA"
perc_Div_first_ALL$Metric<-"Diversity"


# FIRST, NO CHN USA, DIV
Div_crit_first_no_CHNUSA<-as.numeric(crit_first_no_CHNUSA[2])
perc_D_first_NOCHNUSA<-SubsampledPW.results_First_NoUSACHN %>% 
  tally(InvSimp>Div_crit_first_no_CHNUSA)/1000
perc_D_first_NOCHNUSA<-perc_D_first_NOCHNUSA
perc_D_first_NOCHNUSA$Author<-"First"
perc_D_first_NOCHNUSA$Dataset<-"Without China & USA"
perc_D_first_NOCHNUSA$Metric<-"Diversity"


probs<-bind_rows(perc_R_SOLO_ALL,
                 perc_R_SOLO_NOCHNUSA,
                 perc_R_first_all,
                 perc_R_first_NOCHNUSA,
                 perc_Div_SOLO_ALL,
                 perc_D_SOLO_NOCHNUSA,
                 perc_Div_first_ALL,
                 perc_D_first_NOCHNUSA)

probs<-probs %>% dplyr::rename("phat"="n")
write_csv(probs,"./output/probs.csv")


###

AllData_withWaivers<-AllData %>% 
  filter(AuthorNum==1) %>% 
  left_join(WaiverCountries)
# Most common COuntries
# PW
foo<-
  AllData_withWaivers %>%
  filter(JrnlType=="OA") %>% 
  filter(AuthorNum==1) %>% 
  filter(Code!="CHN") %>%
  filter(Code!="USA") %>%
  select(Code,IncomeGroup) %>%
  group_by(Code,IncomeGroup) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  View

# foo<-ungroup(foo) 
# foo %>%  mutate(cumsum(n)/sum(n)*100)

# OA
#foo<-
AllData_withWaivers %>%
  filter(JrnlType=="OA") %>% 
  filter(AuthorNum==1) %>% 
  filter(Code!="CHN") %>% 
  filter(Code!="USA") %>% 
  select(Code,IncomeGroup) %>% 
  group_by(Code,IncomeGroup) %>% 
  drop_na(Code,IncomeGroup) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  View

# foo<-ungroup(foo) 
# foo %>%  mutate(perc<-cumsum(n)/sum(n)*100)

# by country group/journal type
AllData_withWaivers %>%
  filter(JrnlType=="OA") %>% 
  filter(AuthorNum==1) %>% 
  filter(Code!="CHN") %>% 
  filter(Code!="USA") %>% 
  select(IncomeGroup,WaiverGroup) %>% 
  group_by(IncomeGroup,WaiverGroup) %>% 
  drop_na(IncomeGroup) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(n/sum(n)*100)


# by country group/journal type
AllData_withWaivers %>%
  filter(JrnlType=="OA") %>% 
  filter(AuthorNum==1) %>% 
  filter(Code!="CHN") %>% 
  filter(Code!="USA") %>% 
  select(Code,WaiverGroup) %>% 
  group_by(Code,WaiverGroup) %>% 
  drop_na(Code,WaiverGroup) %>% 
  tally() %>% 
  arrange(desc(WaiverGroup),desc(n)) %>% 
  mutate(n/sum(n)*100)


AllData_withWaivers %>%
  filter(JrnlType=="PW") %>% 
  filter(AuthorNum==1) %>%
  # filter(Code!="CHN") %>% 
  # filter(Code!="USA") %>% 
  select(IncomeGroup,WaiverGroup) %>% 
  group_by(IncomeGroup,WaiverGroup) %>% 
  drop_na(IncomeGroup) %>%  
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(n/sum(n)*100)



########################################
# load and add World Bank data on national income categories
WB_prop_waived<-as_tibble(WaiverCountries)

WB_prop_waived$IncomeGroup<-ordered(WB_prop_waived$IncomeGroup, 
                                    levels = c("Low income",
                                               "Lower middle income",
                                               "Upper middle income",
                                               "High income"))

WB_prop_waived$WaiverGroup<-gsub("GroupA","Group A (100% waiver)",WB_prop_waived$WaiverGroup)
WB_prop_waived$WaiverGroup<-gsub("GroupB","Group B (50% waiver)",WB_prop_waived$WaiverGroup)

WB_prop_waived$WaiverGroup<-ordered(WB_prop_waived$WaiverGroup, 
                                    levels = c("Group A (100% waiver)",
                                               "Group B (50% waiver)"))



WB_prop_waived <-WB_prop_waived %>% 
  drop_na(IncomeGroup) %>%
  filter(IncomeGroup=="High income"|IncomeGroup=="Low income"|
           IncomeGroup=="Lower middle income"|IncomeGroup=="Upper middle income") %>% 
  arrange(IncomeGroup,desc(WaiverGroup),Region,Country) 
write_csv(WB_prop_waived,"./data_clean/waiver_table.csv")




waiver_table<-read_csv(file="./data_clean/waiver_table.csv")
waiver_table<-waiver_table %>%
  select(WaiverGroup,IncomeGroup,Region,Country) %>%
  drop_na(WaiverGroup) 
waiver_table$group<-waiver_table$group<-NA
waiver_table<-waiver_table %>%
  group_by(WaiverGroup,IncomeGroup,Region) %>% 
  mutate(group = replace(group, row_number() <6, "1")) %>% 
  mutate(group = replace(group, (row_number() >5&row_number() <11), "2")) %>% 
  mutate(group = replace(group, (row_number() >10&row_number() <16), "3")) %>% 
  mutate(group = replace(group, (row_number() >15&row_number() <21), "4")) %>% 
  mutate(group = replace(group, row_number() >20, "5")) %>% 
  group_by(WaiverGroup,IncomeGroup,Region,group) %>% 
  summarize(CountryGroups = str_c(Country, collapse = ", ")) #this line takes the individual
#cells and collapses them into a single one
waiver_table$group<-NULL

waiver_table<-waiver_table %>% 
  group_by(WaiverGroup,IncomeGroup,Region) %>% 
  mutate(Region = replace(Region, row_number() > 1, "")) %>% 
  group_by(WaiverGroup,IncomeGroup) %>% 
  mutate(IncomeGroup = replace(IncomeGroup, row_number() > 1, "")) %>% 
  group_by(WaiverGroup) %>% 
  mutate(WaiverGroup = replace(WaiverGroup, row_number() > 1, "")) 



waiver_table<-waiver_table %>% 
  group_by(WaiverGroup,IncomeGroup,Region) %>% 
  mutate(Region = replace(Region, row_number() > 1, "")) %>% 
  group_by(WaiverGroup,IncomeGroup) %>% 
  mutate(IncomeGroup = replace(IncomeGroup, row_number() > 1, "")) %>% 
  group_by(WaiverGroup) %>% 
  mutate(WaiverGroup = replace(WaiverGroup, row_number() > 1, "")) 

waiver_table<-as.data.frame(waiver_table)

names(waiver_table)<-c("Waiver Group","Income Group","Region", "Countries")


waiver_table$Countries<-gsub("Chad","\nChad",waiver_table$Countries)
foo<-rbind(waiver_table, waiver_table[rep(6, 1), ])





tot_income<-WB_prop_waived %>% 
  select(IncomeGroup,Code) %>% 
  group_by(IncomeGroup) %>% 
  drop_na(IncomeGroup,Code) %>%  
  tally()

prop_waived<-WB_prop_waived %>% 
  select(IncomeGroup,Code,WaiverGroup) %>% 
  group_by(IncomeGroup,WaiverGroup) %>% 
  tally() %>% 
  arrange(desc(WaiverGroup)) %>% 
  left_join(tot_income,by="IncomeGroup") %>% 
  mutate(prop_in_cat_waived=(n.x/n.y*100)) 
prop_waived



####################
# WAIVER TABLE 2
###################WaiverCountries<-read.csv("./data_clean/WaiverCountries.csv")
WaiverCountries<-read.csv("./data_clean/WaiverCountries.csv")
NON_WavierCountries<-read.csv("./data_clean/NON_WavierCountries.csv")


NON_WavierCountries$WaiverGroup<-"no waiver"

waiver_table2<-bind_rows(WaiverCountries,NON_WavierCountries) %>% 
  arrange(IncomeGroup,desc(WaiverGroup),Region,Country) 

waiver_table2$IncomeGroup<-as.character(waiver_table2$IncomeGroup)
waiver_table2$IncomeGroup<-gsub("Upper middle income", "Middle income",waiver_table2$IncomeGroup)
waiver_table2$IncomeGroup<-gsub("Lower middle income", "Middle income",waiver_table2$IncomeGroup)

waiver_table2$IncomeGroup<-ordered(waiver_table2$IncomeGroup, 
                                   levels = c("Low income",
                                              "Middle income",
                                              "High income"))

waiver_table2$WaiverGroup<-gsub("GroupA","A - 100%",waiver_table2$WaiverGroup)
waiver_table2$WaiverGroup<-gsub("GroupB","B - 50%",waiver_table2$WaiverGroup)

waiver_table2$WaiverGroup<-ordered(waiver_table2$WaiverGroup, 
                                   levels = c("A - 100%",
                                              "B - 50%",
                                              "no waiver"))
# 
# 
waiver_table2$group<-NA

waiver_table2<-waiver_table2 %>%
  select(IncomeGroup,Region,Country,WaiverGroup,group) %>% 
  arrange(Region,WaiverGroup,Country,IncomeGroup) %>% 
  group_by(Region,WaiverGroup,IncomeGroup) %>% 
  mutate(group = replace(group, row_number() < 9, "1")) %>%
  mutate(group = replace(group, (row_number() > 9 &row_number() <19), "2")) %>%
  mutate(group = replace(group, (row_number() > 18 &row_number() <28), "3")) %>%
  mutate(group = replace(group, (row_number() > 27 &row_number() <37), "4")) %>%
  mutate(group = replace(group, (row_number() > 36 &row_number() <46), "5")) %>%
  mutate(group = replace(group, (row_number() > 45 &row_number() <55), "6")) %>%
  mutate(group = replace(group, row_number() > 54, "7")) %>%
  group_by(Region,group,WaiverGroup,IncomeGroup) %>%
  summarize(CountryGroups = str_c(Country, collapse = ", ")) #this line takes the individual
#cells and collapses them into a single one
waiver_table2$group<-NULL


waiver_table2<-waiver_table2 %>% spread(WaiverGroup,CountryGroups)
waiver_table3<-waiver_table2 %>%
  arrange(Region,IncomeGroup) %>% 
  group_by(Region,IncomeGroup) %>% 
  mutate(IncomeGroup = replace(IncomeGroup, row_number() > 1, "")) %>% 
  group_by(Region) %>% 
  mutate(Region = replace(Region, row_number() > 1, "")) 




waiver_table3<-waiver_table2 %>%
  arrange(Region,WaiverGroup,IncomeGroup) %>% 
  group_by(Region,WaiverGroup,IncomeGroup) %>% 
  mutate(IncomeGroup = replace(IncomeGroup, row_number() > 1, "")) %>% 
  group_by(Region,WaiverGroup) %>% 
  mutate(WaiverGroup = replace(WaiverGroup, row_number() > 1, "")) %>% 
  group_by(Region) %>% 
  mutate(Region = replace(Region, row_number() > 1, "")) 


# 
# waiver_table4<-waiver_table3 %>%
#   group_by(Region,WaiverGroup,IncomeGroup) %>% 
#   mutate(Region = replace(Region, row_number() > 1, "")) %>% 
#   group_by(Region,WaiverGroup) %>% 
#   mutate(WaiverGroup = replace(WaiverGroup, row_number() > 1, "")) %>% 
#   group_by(Region) %>% 
#   mutate(Region = replace(Region, row_number() > 1, "")) 


waiver_table3<-as.data.frame(waiver_table3)
waiver_table3<-waiver_table3 %>% select(Region, WaiverGroup,IncomeGroup,CountryGroups)

names(waiver_table3)<-c("Region", "Waiver Group","Income Group","Countries")


waiver_table3$Countries<-gsub("French part","FRA",waiver_table3$Countries)
waiver_table3$Countries<-gsub("Federated","Fed.",waiver_table3$Countries)
waiver_table3$Countries<-gsub("of","",waiver_table3$Countries)
waiver_table3$Countries<-gsub("Lao People’s Democratic Republic","Laos",waiver_table3$Countries)
waiver_table3$Countries<-gsub("United Republic of Tanzania","Tanzania",waiver_table3$Countries)
waiver_table3$Countries<-gsub(" and "," & ",waiver_table3$Countries)
waiver_table3$Countries<-gsub(" SAR China","",waiver_table3$Countries)
waiver_table3$Countries<-gsub("Democratic Republic","Dem. Repub.",waiver_table3$Countries)


# waiver_table3$Country<-gsub("U.","\nU.",waiver_table3$Country)
# waiver_table3$Countries<-gsub("Turks","\nTurks",waiver_table3$Countries)
# waiver_table3$Countries<-gsub("Sudan","\nSudan",waiver_table3$Countries)
# waiver_table3$Country<-gsub("Somalia","\nSomalia",waiver_table3$Country)
# waiver_table3$Countries<-gsub("Sierra","\nSierra",waiver_table3$Countries)

waiver_table3$Region<-gsub("East Asia","E. Asia",waiver_table3$Region)
waiver_table3$Region<-gsub("North","N.",waiver_table3$Region)
waiver_table3$Region<-gsub("America","Am.",waiver_table3$Region)

waiver_table3 <- sapply(waiver_table3, as.character)
waiver_table3[is.na(waiver_table3)] <- ""














####################################
# Figure - Richness
source("./Rscript/functions_figures/RichBootFig.R")
RichBootFig(bootstrap_results)

############################


bootstrap_results %>% 
  group_by(author,Dataset) %>%
  summarize(mean(Richness),mean(InvSimp),mean(Shannon),mean(Even))


############################################################################
# FIGURE 1: For each journal category, the % of articles by 
# 1st authors from different national income classes
############################################################################
source("./Rscript/functions_figures/AppFig3.R") 
AppFig3<-AppFig3(AllData,"author_first")
AppFig3





###################################3
# Bray Curtis
#####################################


# dune2<-slice(dune,1:2)
# simpson<-diversity(dune2,index = "simpson")
# bray = vegdist(dune2, "bray") 
# gower = vegdist(dune2, "gower")

##################
# BC SOLO ALL
##################
bootstrap_results_countries<-read_csv("./output/bootstrap_results_countries.csv")
bootstrap_results_countries
foo<-bootstrap_results_countries %>% 
  filter(author=="solo"& Dataset=="All Countries") %>% 
  select(n,Code,replicate) %>% 
  arrange(n) %>% 
  spread(Code,n) 
foo[is.na(foo)] = 0
foo$cat<-"PW"
foo$cat<-NULL
foo$replicate<-NULL
bray = vegdist(foo, "bray")
str(bray)
bray<-as.matrix(bray)
bray[lower.tri(bray)] <- 0
bray[lower.tri(bray,diag=TRUE)] <- NA
bray<-as.vector(bray)
bray<-as.vector(na.omit(bray))
hist(bray)



# sole_NOCHNUSA
# coauthor_NOCHNUSA
# sole_ALL
# coauthor_ALL

source("./Rscript/functions_ms/DivRichCalc.R") 
OA_data<-DivRichCalc(sole_ALL,'author_first','OA')
OA_countries<-as.data.frame(OA_data[3])
colnames(OA_countries)
names(OA_countries)<-c("Code","n")
OA_countries<-OA_countries %>%spread(Code,n)
OA_countries[is.na(OA_countries)] = 0
OA_countries$cat<-"OA"

data<-bind_rows(OA_countries,foo)
data[is.na(data)] = 0
# first row is oa
data$cat<-NULL
data$replicate<-NULL

bray2 = vegdist(data, "bray") 
#hist(bray2)
str(bray2)

bray2<-as.matrix(bray2)
bray2[lower.tri(bray2)] <- 0
bray2[lower.tri(bray2,diag=TRUE)] <- NA

bray2<-as_tibble(bray2)
OAvPW<-slice(bray2,1)
OAvPW<-as.vector(OAvPW)
OAvPW<-as.numeric(OAvPW)
PWvPW<-slice(bray2,2:1001)

PWvPW<-c(PWvPW)
PWvPW<-unlist(PWvPW)
PWvPW<-as.vector(PWvPW)
PWvPW<-as_tibble(PWvPW)
PWvPW<-na.omit(PWvPW)
summary(PWvPW)

OAvPW<-na.omit(OAvPW)
OAvW<-unlist(OAvPW)
OAvPW<-as_tibble(OAvPW)
summary(OAvPW)
summary(PWvPW)

names(OAvPW)<-c("bray")
names(PWvPW)<-c("bray")
hist(PWvPW$bray)
hist(OAvPW$bray)
OAvPW$cat<-"OA vs. PW"
PWvPW$cat<-"PW vs. PW"
data2<-bind_rows(OAvPW,PWvPW)
data2.1<-data2
data2.1$authors<-"Single Authors"
data2.1$dataset<-"All Countries"

test1<-t.test(bray,bray2)




##################
# BC 1st of CO ALL
##################

bootstrap_results_countries<-read_csv("./output/bootstrap_results_countries.csv")
foo<-bootstrap_results_countries %>% 
  filter(author=="author_first"& Dataset=="All Countries") %>% 
  select(n,Code,replicate) %>% 
  arrange(n) %>% 
  spread(Code,n) 
foo[is.na(foo)] = 0
foo$cat<-"PW"
foo$cat<-NULL
foo$replicate<-NULL
bray = vegdist(foo, "bray")
str(bray)
bray<-as.matrix(bray)
bray[lower.tri(bray)] <- 0
bray[lower.tri(bray,diag=TRUE)] <- NA
bray<-as.vector(bray)
bray<-as.vector(na.omit(bray))
hist(bray)



# sole_NOCHNUSA
# coauthor_NOCHNUSA
# sole_ALL
# coauthor_ALL

source("./Rscript/functions_ms/DivRichCalc.R") 
OA_data<-DivRichCalc(coauthor_ALL,'author_first','OA')
OA_countries<-as.data.frame(OA_data[3])
colnames(OA_countries)
names(OA_countries)<-c("Code","n")
OA_countries<-OA_countries %>%spread(Code,n)
OA_countries[is.na(OA_countries)] = 0
OA_countries$cat<-"OA"

data<-bind_rows(OA_countries,foo)
data[is.na(data)] = 0
# first row is oa
data$cat<-NULL
data$replicate<-NULL

bray2 = vegdist(data, "bray") 
#hist(bray2)
str(bray2)

bray2<-as.matrix(bray2)
bray2[lower.tri(bray2)] <- 0
bray2[lower.tri(bray2,diag=TRUE)] <- NA


bray2<-as_tibble(bray2)
OAvPW<-slice(bray2,1)
OAvPW<-as.vector(OAvPW)
OAvPW<-as.numeric(OAvPW)
PWvPW<-slice(bray2,2:1001)

PWvPW<-c(PWvPW)
PWvPW<-unlist(PWvPW)
PWvPW<-as.vector(PWvPW)
PWvPW<-as_tibble(PWvPW)
PWvPW<-na.omit(PWvPW)
summary(PWvPW)

OAvPW<-na.omit(OAvPW)
OAvW<-unlist(OAvPW)
OAvPW<-as_tibble(OAvPW)
summary(OAvPW)
summary(PWvPW)

names(OAvPW)<-c("bray")
names(PWvPW)<-c("bray")
hist(PWvPW$bray)
hist(OAvPW$bray)
OAvPW$cat<-"OA vs. PW"
PWvPW$cat<-"PW vs. PW"
data2<-bind_rows(OAvPW,PWvPW)
data2.2<-data2
data2.2$authors<-"First Authors"
data2.2$dataset<-"All Countries"


test2<-t.test(bray,bray2)




##################
# BC Solo no CHN USA
##################

bootstrap_results_countries<-read_csv("./output/bootstrap_results_countries.csv")
foo<-bootstrap_results_countries %>% 
  filter(author=="solo"& Dataset=="CHN & USA excluded") %>% 
  select(n,Code,replicate) %>% 
  arrange(n) %>% 
  spread(Code,n) 
foo[is.na(foo)] = 0
foo$cat<-"PW"
foo$cat<-NULL
foo$replicate<-NULL
bray = vegdist(foo, "bray")
str(bray)
bray<-as.matrix(bray)
bray[lower.tri(bray)] <- 0
bray[lower.tri(bray,diag=TRUE)] <- NA
bray<-as.vector(bray)
bray<-as.vector(na.omit(bray))
hist(bray)



# sole_NOCHNUSA
# coauthor_NOCHNUSA
# sole_ALL
# coauthor_ALL

source("./Rscript/functions_ms/DivRichCalc.R") 
OA_data<-DivRichCalc(sole_NOCHNUSA,'author_first','OA')
OA_countries<-as.data.frame(OA_data[3])
colnames(OA_countries)
names(OA_countries)<-c("Code","n")
OA_countries<-OA_countries %>%spread(Code,n)
OA_countries[is.na(OA_countries)] = 0
OA_countries$cat<-"OA"

data<-bind_rows(OA_countries,foo)
data[is.na(data)] = 0
# first row is oa
data$cat<-NULL
data$replicate<-NULL

bray2 = vegdist(data, "bray") 
#hist(bray2)
str(bray2)

bray2<-as.matrix(bray2)
bray2[lower.tri(bray2)] <- 0
bray2[lower.tri(bray2,diag=TRUE)] <- NA


bray2<-as_tibble(bray2)
OAvPW<-slice(bray2,1)
OAvPW<-as.vector(OAvPW)
OAvPW<-as.numeric(OAvPW)
PWvPW<-slice(bray2,2:1001)

PWvPW<-c(PWvPW)
PWvPW<-unlist(PWvPW)
PWvPW<-as.vector(PWvPW)
PWvPW<-as_tibble(PWvPW)
PWvPW<-na.omit(PWvPW)
summary(PWvPW)

OAvPW<-na.omit(OAvPW)
OAvW<-unlist(OAvPW)
OAvPW<-as_tibble(OAvPW)
summary(OAvPW)
summary(PWvPW)

names(OAvPW)<-c("bray")
names(PWvPW)<-c("bray")
hist(PWvPW$bray)
hist(OAvPW$bray)
OAvPW$cat<-"OA vs. PW"
PWvPW$cat<-"PW vs. PW"
data2<-bind_rows(OAvPW,PWvPW)
data2.3<-data2
data2.3$authors<-"Single Authors"
data2.3$dataset<-"China and USA Excluded"



test3<-t.test(bray,bray2)



##################
# BC first of co no CHN USA
##################

bootstrap_results_countries<-read_csv("./output/bootstrap_results_countries.csv")
foo<-bootstrap_results_countries %>% 
  filter(author=="author_first"& Dataset=="CHN & USA excluded") %>% 
  select(n,Code,replicate) %>% 
  arrange(n) %>% 
  spread(Code,n) 
foo[is.na(foo)] = 0
foo$cat<-"PW"
foo$cat<-NULL
foo$replicate<-NULL
bray = vegdist(foo, "bray")
str(bray)
bray<-as.matrix(bray)
bray[lower.tri(bray)] <- 0
bray[lower.tri(bray,diag=TRUE)] <- NA
bray<-as.vector(bray)
bray<-as.vector(na.omit(bray))
hist(bray)



# sole_NOCHNUSA
# coauthor_NOCHNUSA
# sole_ALL
# coauthor_ALL

source("./Rscript/functions_ms/DivRichCalc.R") 
OA_data<-DivRichCalc(coauthor_NOCHNUSA,'author_first','OA')
OA_countries<-as.data.frame(OA_data[3])
colnames(OA_countries)
names(OA_countries)<-c("Code","n")
OA_countries<-OA_countries %>%spread(Code,n)
OA_countries[is.na(OA_countries)] = 0
OA_countries$cat<-"OA"

data<-bind_rows(OA_countries,foo)
data[is.na(data)] = 0
# first row is oa
data$cat<-NULL
data$replicate<-NULL

bray2 = vegdist(data, "bray") 
#hist(bray2)
str(bray2)

bray2<-as.matrix(bray2)
bray2[lower.tri(bray2)] <- 0
bray2[lower.tri(bray2,diag=TRUE)] <- NA


bray2<-as_tibble(bray2)
OAvPW<-slice(bray2,1)
OAvPW<-as.vector(OAvPW)
OAvPW<-as.numeric(OAvPW)
PWvPW<-slice(bray2,2:1001)

PWvPW<-c(PWvPW)
PWvPW<-unlist(PWvPW)
PWvPW<-as.vector(PWvPW)
PWvPW<-as_tibble(PWvPW)
PWvPW<-na.omit(PWvPW)
summary(PWvPW)

OAvPW<-na.omit(OAvPW)
OAvW<-unlist(OAvPW)
OAvPW<-as_tibble(OAvPW)
summary(OAvPW)
summary(PWvPW)

names(OAvPW)<-c("bray")
names(PWvPW)<-c("bray")
hist(PWvPW$bray)
hist(OAvPW$bray)
OAvPW$cat<-"OA vs. PW"
PWvPW$cat<-"PW vs. PW"
data2<-bind_rows(OAvPW,PWvPW)

data2.4<-data2
data2.4$authors<-"First Authors"
data2.4$dataset<-"China and USA Excluded"


test4<-t.test(bray,bray2)
data<-bind_rows(data2.1,data2.2,data2.3,data2.4)

write_csv(data,"./output/BC_data.csv")


library(broom)
library(purrr)


data$authors <- ordered(data$authors,levels = c("Single Authors","First Authors"))

tab <- map_df(list(test1, test2, test3,test4), tidy)
# tab<-tab[c("estimate", "statistic", "p.value", "conf.low", "conf.high")]
tab<-tab[c("statistic", "p.value", "conf.low", "conf.high")]
tab<-tab %>% dplyr::rename("t"="statistic", "p value"="p.value", "95% CI (low)"="conf.low", "95% CI (high)"="conf.high")



tab1<-data %>% group_by(dataset,authors,cat) %>% 
  summarize(mean_BC=mean(bray)) %>% 
  spread(cat,mean_BC) %>% 
  select(authors,dataset,'OA vs. PW','PW vs. PW') 
tab1<-dplyr::rename(tab1,'mean OA vs. PW'='OA vs. PW','mean PW vs. PW'='PW vs. PW')

tab2<-data %>% group_by(dataset,authors,cat) %>% 
  summarize(sd_BC=sd(bray)) %>% 
  spread(cat,sd_BC) %>% 
  select(authors,dataset,'OA vs. PW','PW vs. PW')
tab2<-dplyr::rename(tab2,'SD OA vs. PW'='OA vs. PW','SD PW vs. PW'='PW vs. PW')


tab1<-left_join(tab1,tab2)


tab1<-bind_cols(tab1,tab)




count<-data %>% group_by(dataset,authors,cat) %>% 
  summarize(n=n()) 
















############################################################
# Table 1: PAPERS PER JOURNAL 
############################################################
source("./Rscript/functions_ms/SummaryTable.R") 
Tables<-SummaryTable(AllData)

# Table1<-as.data.frame(Table1)
# Table1[4,1]<-"Biochimie[^1]"
# Table1[26,1]<-"Microelectronic Engineering[^1]"
write.csv(Tables[1], "./tables_figs/Table1_July.csv", row.names = FALSE)
# Alternative version
Table1v2<-Tables[2]
Table1v2<-as.data.frame(Table1v2)
names(Table1v2)<-c("Journal","Articles (n)","OA Mirror Articles (n)","APC ($)")
Table1v2[4,1]<-"BiochimieSuper1"
Table1v2[27,1]<-"Microelectronic EngineeringSuper2"
Table1v2[39,4]<-""
colnames(Table1v2)
write.csv(Table1v2, "./tables_figs/Table1v2_July.csv", row.names = FALSE)

############################################################################
# FIGURE 1: For each journal category, the % of articles by 
# 1st authors from different national income classes
############################################################################
source("./Rscript/functions_figures/AppFig3.R") 
AppFig3<-AppFig3(AllData,"author_first")
AppFig3

################
# Fig 1 with sampled distributions of PW 
################
# bootstrap_results<-read_csv('./output/bootstrap_results.csv')
# bootstrap_results_countries<-read_csv("./output/bootstrap_results_countries.csv")


source("./Rscript/functions_figures/AltFig1.R") 
Fig1A<-AltFig1(bootstrap_results,bootstrap_results_countries)
Fig1A


source("./Rscript/functions_figures/AltFig1B.R") 
Fig1B<-AltFig1(bootstrap_results,bootstrap_results_countries)
Fig1B

# AuPosition: "author_first","author_last","solo"
# countries: "All",  "no_CHN_USA"
source("./Rscript/functions_figures/AltFig1_hist.R") 
Fig1<-AltFig1_hist(bootstrap_results,bootstrap_results_countries,"author_first","All")
Fig1

############################################################
# Fig. 2: For 1st authors from each national income class,
# the % of articles that were in PW vs OA journals
# Group by countries first. The idea is to compare
# where authors from each income class publish
###########################################################
#########################################################
source("./Rscript/functions_figures/Fig2.R") 
# OPTIONS: "author_first"   "author_last" "author_all"
Fig2a<-Fig2(AllData,"author_first")
Fig2a
png(file="./tables_figs/Fig2a_first_authors.png",width=1000, height=700)
Fig2a
dev.off()

# Fig2b<-Fig2(AllData,"author_last")
# Fig2b
# png(file="./tables_figs/Fig2b.png",width=1000, height=700)
# Fig2b
# dev.off()
# 


############################################################
# FIG 3: for articles in OA journals: the number of articles 
# by 1st authors in each country
# 3a: OA-First  # 3b: PW-First
# 3c: OA-Last   # 3d: PW-Last
############################################################

# ALL JOURNALS, FIRST, LAST, SOLO AUTHOR COUNTRY 
# FOR APPENDIX
source("./Rscript/functions_figures/AppFig1.R") 
Appendix1Fig<-AppFig1(AllData)
Appendix1Fig

png(file="./tables_figs/Appendix1Fig.png",width=1000, height=700)
Appendix1Fig
dev.off()

# SOLO AUTHORED, PW
source("./Rscript/functions_figures/Fig3.R") 
Fig3bSoloPW<-Fig3(sole_author_pubs_ALL_first_author,"author_first","PW")
Fig3bSoloPW
png(file="./tables_figs/Fig3bSoloPW.png",width=1000, height=700)
Fig3bSoloPW
dev.off()


# SOLO AUTHORED, OA
Fig3bSoloOA<-Fig3(sole_author_pubs_ALL_first_author,"author_first","OA")
Fig3bSoloOA
png(file="./tables_figs/Fig3bSoloOA.png",width=1000, height=700)
Fig3bSoloOA
dev.off()


# COAUTHORED, FIRST, PW
source("./Rscript/functions_figures/Fig3.R") 
Fig3bCoPW_First<-Fig3(coauthor_pubs_ALL_first_author,"author_first","PW")
Fig3bCoPW_First
png(file="./tables_figs/Fig3bCoauthoredPW_First.png",width=1000, height=700)
Fig3bCoPW_First
dev.off()

# COAUTHORED, FIRST, OA

source("./Rscript/functions_figures/Fig3.R") 
Fig3aCoOA_First<-Fig3(coauthor_pubs_ALL_first_author,"author_first","OA")
Fig3aCoOA_First
png(file="./tables_figs/Fig3aCoAuthoredOA_First.png",width=1000, height=700)
Fig3aCoOA_First
dev.off()

##################################################
# FIG4: Compare the obsered author diversity and 
# richness in OA Journals with an identically sized 
# bootstrapped sample of OA articles
# ie, where the does the observed lie relative to the bootstrap? 
##################################################
# DATA PREP
OA_papers <- coauthor_pubs %>% filter(JrnlType == "OA")
OA_papers_boot<-sample_n(OA_papers, nrow(OA_papers), replace = TRUE)
source("./Rscript/functions_ms/DivRichCalc.R") 
nboot <-1000 #number of bootstrap samples
Richness <-rep(NA, nboot)
InvSimp <-rep(NA, nboot)
bootstrap.OA.1st<-data.frame(Richness,InvSimp)
rm(Richness,InvSimp)
set.seed(10)
for(i in 1:nboot){
  bootOA<-DivRichCalc(sample_n(OA_papers, 
                               nrow(OA_papers), replace = TRUE),
                      "author_first","OA")
  bootstrap.OA.1st[i,1]<-bootOA[1]
  bootstrap.OA.1st[i,2]<-bootOA[2]
}
bootstrap.OA.1st<-arrange(bootstrap.OA.1st)

hist(bootstrap.OA.1st$Richness)
hist(bootstrap.OA.1st$InvSimp)
summary(bootstrap.OA.1st)
OADivRich<-DivRichCalc(coauthor_pubs,"author_first","OA")
OAdiv<-as.numeric((OADivRich)[2])
OARich<-as.numeric((OADivRich)[1])

########## BOOTSTRAP 1st author diversity OA

source("./Rscript/functions_figures/Fig4.R") 
fig4<-Fig4(bootstrap.OA.1st,OAdiv,OARich)
fig4a<-fig4[1]
fig4b<-fig4[2]
rm(fig4)
png(file="./tables_figs/plot4a.png",width=1000, height=700)
fig4a
dev.off()
png(file="./tables_figs/plot4b.png",width=1000, height=700)
fig4b
dev.off()


#################################################################
# calculate diversity indices of 
# FIRST AUTHORS FOR EACH INDIVIDUAL JOURNAL
# Returns in WIDE format to include diff between OA and PW mirors
#################################################################

source("./Rscript/functions/DivCalcJrnl.R") # enter as divCalc(df,JrnlType,Author)
DivMetrics<-DivCalcJrnl(AllData)
write.csv(DivMetrics, 'output/DivMetricsALL_FirstAuthors.csv', row.names = FALSE)

boxplot(DivMetrics$DeltaDiv)
median(DivMetrics$DeltaDiv,na.rm=TRUE)
# save_name<-paste('output/DivMetricsALL_', Sys.Date(), '.csv') #to add the date of output to filename
# rm(save_name)
# source("./Rscript/functions/DivRichCalc.R")
# # DivRichCalc<-function(DataSet,AuPosition,JrnlType)
# DivRichCalc_result<-DivRichCalc(sole_ALL,"author_first","OA")
# DivRichCalc_result

############################################################
############################################################
# OTHER ANALYSES
############################################################
############################################################

############################################################
# Mean No. of authors per article (& SD) for each journal
############################################################
AvgNumbAuthors <- AllData %>% # average number of authors per journal 
  # filter(Year==2019) %>% 
  group_by(JrnlType,Journal,pair_key,DOI) %>% 
  arrange(JrnlType, Journal) %>% 
  filter(AuthorNum == max(AuthorNum)) %>% 
  group_by(pair_key,JrnlType,Journal) %>% 
  summarize(avg_n=mean(AuthorNum),sd_n=sd(AuthorNum)) %>% 
  arrange(Journal)
AvgNumbAuthors

############################################################
# histogram of author number and mean/Sd number of 
# authors (all journals pooled)
############################################################
AvgNumbAuthorsAll <- coauthor_pubs_ALL %>% # average number of authors per journal 
  group_by(DOI) %>% 
  filter(AuthorNum == max(AuthorNum)) %>% 
  ungroup()
hist(AvgNumbAuthorsAll$AuthorNum, breaks=30)
table(AvgNumbAuthorsAll$AuthorNum)/25531*100
summarize(AvgNumbAuthorsAll,avg_n=mean(AuthorNum),sd_n=sd(AuthorNum))



###################################
MiddleIncome_First<-FirstLast %>% 
  filter(IncomeGroup_last=="Lower middle"| IncomeGroup_last=="Upper middle") %>% 
  group_by(IncomeGroup_last,Country_last,JrnlType) %>% 
  summarize(n=n()) %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(IncomeGroup_last,JrnlType,desc(perc))
MiddleIncome_Last

MiddleIncome_First<-FirstLast %>% 
  filter(IncomeGroup_first=="Lower middle"| IncomeGroup_first=="Upper middle") %>% 
  group_by(IncomeGroup_first,Country_first,JrnlType) %>% 
  summarize(n=n()) %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(IncomeGroup_first,JrnlType,desc(perc)) %>% 
  filter(perc>97)
MiddleIncome_First

Low_First<-FirstLast %>% 
  filter(IncomeGroup_first=="Low") %>% 
  group_by(IncomeGroup_first,Country_first,JrnlType) %>% 
  summarize(n=n()) %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(IncomeGroup_first,JrnlType,desc(perc)) 
# filter(perc>97)
Low_First
sum(Low_First$n)



Low_Last<-FirstLast %>% 
  filter(IncomeGroup_last=="Low") %>% 
  group_by(IncomeGroup_last,Country_last,JrnlType) %>% 
  summarize(n=n()) %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(IncomeGroup_last,JrnlType,desc(perc)) 
# filter(perc>97)
Low_Last
sum(Low_Last$n)


High_Last<-FirstLast %>% 
  filter(IncomeGroup_last=="High") %>% 
  group_by(IncomeGroup_last,Country_last,JrnlType) %>% 
  summarize(n=n()) %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(IncomeGroup_last,JrnlType,desc(perc)) 
# filter(perc>97)
High_Last
sum(High_Last$n)
