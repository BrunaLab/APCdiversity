SubSamplePWvsOA_comparison<-function(Dataset,AuPosition) {
#############################################################################################
# DIV OF PW SUBSAMPLES MATCHING OA BY NUMBER OF ARTICLES
###################################################################################################
# source("./Rscript/functions/Prep_for_samplePW.R")
# This returns a list with two elements: 
# [1] the df of PW papers from which to sample
# [2] a vector n with number of papers in each OA journal.
#     a matching number of PW papers will be sampled with 
#     'samplePW'
source("./Rscript/functions/Prep_for_samplePW.R")
Prep_for_samplePW<-Prep_for_samplePW(Dataset)
PW_papers<-Prep_for_samplePW[1]
n<-Prep_for_samplePW[2]

# source("./Rscript/functions/samplePW.R")
# This returns a list with two elements: 
# [1] the df of sampled PW papers
# [2] df comparing n of papers in OA journals & n of PW papers sampled
source("./Rscript/functions/samplePW.R")
PW_sampling<-samplePW(PW_papers,n)
PW_sample<-PW_sampling[1]
PW_sample_check<-PW_sampling[2]
################

############################################
# now sample and iterate!
# THIS IS FOR MATCHED SUBSAMPLING - FIRST AUTHOR
# 1<-Richness  2<-InvSimpsons 3<-Countries
############################################
source("./Rscript/functions/samplePW.R")
source("./Rscript/functions/DivRichCalc.R")
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
SubsampledPW.results_First <- data.frame(Richness,InvSimp)
rm(InvSimp,Richness)
set.seed(10)
for(i in 1:nboot){
  PW_sample<-samplePW(PW_papers,n)
  # The first element of the list is the list of papers. 
  # Need to take that list and Dataset to get back all the 
  # papers with all their authors.
  PW_sample<-as.data.frame(PW_sample[1])
  PW_sample_DOIs<-PW_sample$DOI
  PW_sample<-Dataset %>% filter(DOI%in%PW_sample_DOIs)
  # PW_sample %>% summarize(n_distinct(DOI)) #confirm the number of articles
  PW_sample<-as.data.frame(PW_sample)
  AuPosition<-AuPosition
  JrnlType<-"PW"
  results<-DivRichCalc(PW_sample,AuPosition,JrnlType)
  SubsampledPW.results_First[i,1]<-(results)[1]
  SubsampledPW.results_First[i,2]<-(results)[2]
}
SubsampledPW.results_First
toc()

return(SubsampledPW.results_First)
}