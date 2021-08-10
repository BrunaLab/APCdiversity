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
# 
# # NEED TO STANDARDIZE COUNTRYIES IN UK IN COUNTRY COLUMN
# MirrorPairs<-read_csv("./data_clean/MirrorPairs.csv")
# WaiverCountries<-read_csv("./data_clean/WaiverCountries.csv")
# NON_WavierCountries<-read_csv("./data_clean/NON_WavierCountries.csv")
# 
# 
AllData <- read_csv("./data_clean/all_data_analysis.csv")

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


####################################
####################################
####################################
# Permutation tests
# generate the results of permutation tests comparing mirror and oa in parent
# This will save all the results to the clean_data folder. No need to have in 
# memory as not used in any further analyses
source("./Rscript/functions_ms/permutation_tests_OA.R")
permutation_tests_OA(AllData)


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


write_csv(BootOAinPW_RichDiv, "./data_clean/BootOAinPW_RichDiv.csv")
write_csv(BootOAinPW_Countries,'./data_clean/BootOAinPW_Countries.csv')

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




write_csv(BootMirror_RichDiv, "./data_clean/BootMirror_RichDiv.csv")
write_csv(BootMirror_Countries,'./data_clean/BootMirror_Countries.csv')
# Boot_RichDiv<-read_csv("./output/Boot_RichDiv.csv")
# Boot_Countries<-read_csv("./output/Boot_Countries.csv")




## Data from permutation test results 
# source("./Rscript/functions_ms/permutation_tests_OA.R")
# permutation_tests_OA(AllData)

# summary(boot_results_countries)
# write.csv(Boot_Countries, "./data_clean/Boot_Countries.csv", row.names = FALSE)
# # Boot_Countries<-read_csv("./output/Boot_Countries.csv")
# write.csv(NoReplacePW_RichDiv,'./data_clean/Boot_RichDiv.csv',row.names = FALSE)
# # Boot_RichDiv<-read_csv("./output/Boot_RichDiv.csv")
