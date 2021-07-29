bootstrap_OA_in_PW<-function(Dataset,AuPosition,OAtype) {
#############################################################################################
# DIV OF PW SUBSAMPLES MATCHING OA BY NUMBER OF ARTICLES
###################################################################################################
# source("./Rscript/functions/Prep_for_samplePW.R")
# This returns a list with two elements: 
# [1] the df of PW papers from which to sample
# [2] a vector n with number of papers in each OA journal.
#     a matching number of PW papers will be sampled with 
#     'samplePW'
# Dataset<-sole_ALL 
# AuPosition<-"author_first"
# OAtype<-"mirror"
source("./Rscript/functions_ms/Prep_for_sampleOAinPW.R")
Prep_for_samplePW<-Prep_for_sampleOAinPW(Dataset,OAtype)
PW_papers<-Prep_for_samplePW[1]
n<-Prep_for_samplePW[2]
# sum(as.data.frame(n))
# source("./Rscript/functions/samplePW.R")
# This returns a list with two elements: 
# [1] the df of sampled PW papers
# [2] df comparing n of papers in OA journals & n of PW papers sampled
source("./Rscript/functions_ms/samplePW.R")
PW_sampling<-suppressMessages(samplePW(PW_papers,n))
PW_sampling<-samplePW(PW_papers,n)
PW_sample<-PW_sampling[1]
PW_sample_check<-PW_sampling[2]
################

############################################
# now sample and iterate!
# THIS IS FOR MATCHED SUBSAMPLING - FIRST AUTHOR
# 1<-Richness  2<-InvSimpsons 3<-Countries
############################################
source("./Rscript/functions_ms/samplePW.R")
source("./Rscript/functions_ms/DivRichCalc.R")
# source("./Rscript/functions/DivCalcPooledFirst.R")
PW_papers<-as.data.frame(PW_papers)
n<-unlist(n)
n<-as.data.frame(n)
n<-n$n
library(tictoc)
tic()   
nboot <-1000 #number of bootstra p samples
InvSimp <-rep(NA, nboot)
Richness<-rep(NA, nboot)
Countries<-rep(NA, nboot)
Shannon<-rep(NA, nboot)
Even<-rep(NA, nboot)
replicate<-data.frame(replicate=as.numeric())

SubsampledPW.results_First <- data.frame(Richness,InvSimp,Shannon,Even)
# replicate<-data.frame(replicate)

rm(InvSimp,Richness,Shannon,Even)
set.seed(1)
for(i in 1:nboot){


  PW_sample<-suppressMessages(samplePW(PW_papers,n))  
  
  PW_sample<-samplePW(PW_papers,n)
  # The first element of the list is the list of papers. 
  # Need to take that list and Dataset to get back all the 
  # papers with all their authors.
  PW_sample<-as.data.frame(PW_sample[1])
  PW_sample_refID<-PW_sample$refID
  PW_sample<-Dataset %>% filter(refID%in%PW_sample_refID)
  # PW_sample %>% summarize(n_distinct(DOI)) #confirm the number of articles
  PW_sample<-as.data.frame(PW_sample)
  AuPosition<-AuPosition
  ArticleType<-"PW"
  JrnlType<-"PW"
  
  results<-DivRichCalc(PW_sample,AuPosition,JrnlType,ArticleType)
  SubsampledPW.results_First[i,1]<-(results)[1]
  SubsampledPW.results_First[i,2]<-(results)[2]
  SubsampledPW.results_First[i,3]<-(results)[4]
  SubsampledPW.results_First[i,4]<-(results)[5]
  Countries[i]<-(results)[3]
  
  # nCountries[i,1]<-i
  # nCountries[i,2]<-(nrow(Countries[[i]]))
  # count<-data.frame(replicate=rep(nCountries[i,1], each=nCountries[i,2]))
  count<-data.frame(replicate=rep(i, each=SubsampledPW.results_First[i,1]))
  replicate<- bind_rows(replicate,count)
  # replicate<-bind_rows(replicate,count)
    # bind_rows(counter,counter=rep(i, each=nCountries))
  # nCountries[i]<-nrow(Countries[[i]])
  # counter on screen
  cat("\r", i, "of", nboot) 
  flush.console()
}

SubsampledPW.results_First$author<-AuPosition
SubsampledPW.results_First

########################################
# CALC AND SAVE A DF OF THE COUNTRIES SAMPLED
# EACH RUN
########################################

# 
# # Use the AllData df to get the basic info on each country (code, region, etc)
# CountryInfo<-AllData %>% 
#   select(First_Author_Country,Region,IncomeGroup,Code) %>% 
#   group_by(Code) %>% 
#   slice(1)
# # head(CountryInfo,10)
# 
# 
# Countries<-bind_rows(Countries)
# Countries<-bind_cols(Countries,replicate)
# Subsampled_Countries<-as.data.frame(Countries)
# # str(Subsampled_Countries)
# Subsampled_Countries<-Subsampled_Countries %>% 
#   inner_join(CountryInfo,Subsampled_Countries,by="Code") 
# # head(Subsampled_Countries,10)
# Subsampled_Countries$IncomeGroup<-as.factor(Subsampled_Countries$IncomeGroup) 
# Subsampled_Countries$Region<-as.factor(Subsampled_Countries$Region) 
# Subsampled_Countries$Code<-as.factor(Subsampled_Countries$Code) 
# Subsampled_Countries$Country<-as.factor(Subsampled_Countries$First_Author_Country) 
# Subsampled_Countries$author<-AuPosition


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




SubsampledPW.results_First$Dataset<-deparse(substitute(Dataset)) #adds the name of the dataset (All Data or No China USA as chr)
SubsampledPW.results_First$Dataset<-gsub("AllData_noUSAorCHN","Without CHN & USA",SubsampledPW.results_First$Dataset)
SubsampledPW.results_First$Dataset<-gsub("AllData","All Countries",SubsampledPW.results_First$Dataset)
Subsampled_Countries$Dataset<-deparse(substitute(Dataset)) #adds the name of the dataset (All Data or No China USA as chr)
Subsampled_Countries$Dataset<-gsub("AllData_noUSAorCHN","Without CHN & USA",Subsampled_Countries$Dataset)
Subsampled_Countries$Dataset<-gsub("AllData","All Countries",Subsampled_Countries$Dataset)
SubsampledPW.results<-list(SubsampledPW.results_First,Subsampled_Countries)
toc()

return(SubsampledPW.results)
}