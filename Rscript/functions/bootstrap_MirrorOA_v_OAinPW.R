bootstrap_MirrorOA_v_OAinPW<-function(Dataset,AuPosition) {
#############################################################################################
# DIV OF PW SUBSAMPLES MATCHING OA BY NUMBER OF ARTICLES
###################################################################################################
# source("./Rscript/functions/Prep_for_samplePW.R")
# This returns a list with two elements: 
# [1] the df of PW papers from which to sample
# [2] a vector n with number of papers in each OA journal.
#     a matching number of PW papers will be sampled with 
#     'samplePW'
# Dataset<-first_ALL
  # Dataset<-AllData
# AuPosition<-"author_first"
# source("./Rscript/functions/Prep_for_sample_MirrorvOAinPW.R")
mirrorOA_papers <- Dataset %>%
    filter(JrnlType=="OA") %>% 
    group_by(pair_key,Journal) %>%
    summarize(n=n_distinct(DOI)) %>% 
    arrange(Journal) %>%
    ungroup() 
  # select(pair_key,n)
  # mirrorOA_papers$pair_key<-as.factor(mirrorOA_papers$pair_key)
  # nlevels(mirrorOA_papers$pair_key)
  # mirrorOA_papers$pair_key<-droplevels(mirrorOA_papers$pair_key)
  # nlevels(mirrorOA_papers$pair_key)
  n<-sum(mirrorOA_papers$n)
#   
OAinPW_papers<- Dataset %>%
    filter(JrnlType == "PW" & ArticleType == "OA")
# source("./Rscript/functions/sampleOAinPW.R")
# OAinPW_sampling<-samplePW(OAinPW_papers,n)
# OAinPW_sample<-as.data.frame(PW_sampling[1])

# PW_sample_check<-PW_sampling[2]
################

############################################
# now sample and iterate!
# THIS IS FOR MATCHED SUBSAMPLING - FIRST AUTHOR
# 1<-Richness  2<-InvSimpsons 3<-Countries
############################################
source("./Rscript/functions/sampleOAinPW.R")
source("./Rscript/functions/DivRichCalc.R")
# source("./Rscript/functions/DivCalcPooledFirst.R")
OAinPW_papers<-as.data.frame(OAinPW_papers)
n<-unlist(n)
n<-as.data.frame(n)
n<-n$n
library(tictoc)
tic()   
nboot <-2000 #number of bootstra p samples
InvSimp <-rep(NA, nboot)
Richness<-rep(NA, nboot)
Countries<-rep(NA, nboot)
Shannon<-rep(NA, nboot)
Even<-rep(NA, nboot)
replicate<-data.frame(replicate=as.numeric())

SubsampledOAinPW.results_First <- data.frame(Richness,InvSimp,Shannon,Even)
# replicate<-data.frame(replicate)

rm(InvSimp,Richness,Shannon,Even)
set.seed(1)
for(i in 1:nboot){


  
  source("./Rscript/functions/sampleOAinPW.R")
  OAinPW_sample<-sampleOAinPW(OAinPW_papers,n)
  # The first element of the list is the list of papers. 
  # Need to take that list and Dataset to get back all the 
  # papers with all their authors.
  OAinPW_sample<-as.data.frame(OAinPW_sample[1])
  OAinPW_sample_refID<-OAinPW_sample$refID
  OAinPW_sample<-Dataset %>% filter(refID%in%OAinPW_sample_refID)
  # PW_sample %>% summarize(n_distinct(DOI)) #confirm the number of articles
  OAinPW_sample<-as.data.frame(OAinPW_sample)
  AuPosition<-AuPosition
  ArticleType<-"OA"
  JrnlType<-"PW"
  results<-DivRichCalc(OAinPW_sample,AuPosition,JrnlType,ArticleType)
  SubsampledOAinPW.results_First[i,1]<-(results)[1]
  SubsampledOAinPW.results_First[i,2]<-(results)[2]
  SubsampledOAinPW.results_First[i,3]<-(results)[4]
  SubsampledOAinPW.results_First[i,4]<-(results)[5]
  Countries[i]<-(results)[3]
  
  # nCountries[i,1]<-i
  # nCountries[i,2]<-(nrow(Countries[[i]]))
  # count<-data.frame(replicate=rep(nCountries[i,1], each=nCountries[i,2]))
  count<-data.frame(replicate=rep(i, each=SubsampledOAinPW.results_First[i,1]))
  replicate<- bind_rows(replicate,count)
  # replicate<-bind_rows(replicate,count)
    # bind_rows(counter,counter=rep(i, each=nCountries))
  # nCountries[i]<-nrow(Countries[[i]])
  
}

SubsampledOAinPW.results_First$author<-AuPosition
SubsampledOAinPW.results_First

########################################
# CALC AND SAVE A DF OF THE COUNTRIES SAMPLED
# EACH RUN
########################################


CountryInfo<-read_csv("./data_clean/CountryData.csv")
Countries<-bind_rows(Countries)
Countries<-bind_cols(Countries,replicate)
Subsampled_Countries<-as.data.frame(Countries)
# str(Subsampled_Countries)
Subsampled_Countries<-Subsampled_Countries %>% 
  inner_join(CountryInfo,Subsampled_Countries,by="Code") 
# head(Subsampled_Countries,10)
Subsampled_Countries$IncomeGroup<-as.factor(Subsampled_Countries$IncomeGroup) 
Subsampled_Countries$Region<-as.factor(Subsampled_Countries$Region) 
Subsampled_Countries$Code<-as.factor(Subsampled_Countries$Code)
library(countrycode)
Subsampled_Countries$Country<-countrycode(Subsampled_Countries$Code,origin = 'iso3c',destination ="country.name")
Subsampled_Countries$author<-AuPosition



SubsampledOAinPW.results_First$Dataset<-deparse(substitute(Dataset)) #adds the name of the dataset (All Data or No China USA as chr)
SubsampledOAinPW.results_First$Dataset<-gsub("AllData_noUSAorCHN","Without CHN & USA",SubsampledOAinPW.results_First$Dataset)
SubsampledOAinPW.results_First$Dataset<-gsub("AllData","All Countries",SubsampledOAinPW.results_First$Dataset)
Subsampled_Countries$Dataset<-deparse(substitute(Dataset)) #adds the name of the dataset (All Data or No China USA as chr)
Subsampled_Countries$Dataset<-gsub("AllData_noUSAorCHN","Without CHN & USA",Subsampled_Countries$Dataset)
Subsampled_Countries$Dataset<-gsub("AllData","All Countries",Subsampled_Countries$Dataset)
SubsampledOAinPW.results<-list(SubsampledOAinPW.results_First,Subsampled_Countries)
toc()

return(SubsampledOAinPW.results)
}