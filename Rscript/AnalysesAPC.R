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

# load the data to be used in analyses

AllData<-read_csv(file="./data_clean/AllData.csv")

MirrorPairs<-read.csv("./data_clean/MirrorPairs.csv")

WaiverCountries<-read.csv("./data_clean/WaiverCountries.csv")

AllData_noUSAorCHN<-read_csv(file="./data_clean/AllData_noUSAorCHN.csv")


############################################################
# APCs
############################################################
APC<-MirrorPairs %>% 
  filter(JrnlType=="OA") %>% 
  summarize(medianAPC=median(APC),avg_APC=mean(APC),sd_APC=sd(APC),maxAPC=max(APC),minAPC=min(APC))
APC<-round(APC,2)
APC

############################################################
# Total number of journals
############################################################
n_journals <- AllData %>% 
  group_by(JrnlType) %>% 
  summarize(n=n_distinct(Journal))
n_journals

############################################################
# Number of articles 
############################################################
# TOTAL
NumbArticles <- AllData %>% #number of papers per journal
  summarize(n=n_distinct(DOI))
NumbArticles

# BY JOURNAL TYPE
NumbArticles_JrnlType <- AllData %>% #number of papers per journal
  group_by(JrnlType) %>% 
  summarize(n=n_distinct(DOI))
NumbArticles_JrnlType

n_counrtries<-AllData %>% 
  group_by(JrnlType) %>% 
  summarize(n_distinct(Code))
n_counrtries

counrtriesOA<-AllData %>% 
  filter(AuthorNum==1&JrnlType=="OA") %>% 
  group_by(Code) %>% 
  select(Code,IncomeGroup,Region) %>% 
  slice(1)

counrtriesPW<-AllData %>% 
  filter(AuthorNum==1&JrnlType=="PW") %>% 
  group_by(Code) %>% 
  select(Code,IncomeGroup,Region) %>% 
  slice(1)


intersecting_countries<-intersect(counrtriesOA, counrtriesPW)
OAnotPW<-setdiff(counrtriesOA, counrtriesPW)
OAnotPW$Code<-as.factor(OAnotPW$Code)
OAnotPW$IncomeGroup<-as.factor(OAnotPW$IncomeGroup)
OAnotPW$Region<-as.factor(OAnotPW$Region)
OAnotPW$set<-"in OA but not PW"
OAnotPW$set<-as.factor(OAnotPW$set)
OAnotPW_summary<-summary(OAnotPW)

PWnotOA<-setdiff(counrtriesPW,counrtriesOA)
PWnotOA$set<-"in PW but not OA"
PWnotOA$Code<-as.factor(PWnotOA$Code)
PWnotOA$IncomeGroup<-as.factor(PWnotOA$IncomeGroup)
PWnotOA$Region<-as.factor(PWnotOA$Region)
PWnotOA$set<-as.factor(PWnotOA$set)
PWnotOA_summary<-summary(PWnotOA)
Table_setdiffs<-bind_rows(OAnotPW,PWnotOA)
Table_setdiffs.2<-Table_setdiffs %>% 
  select(set,IncomeGroup) %>% 
  group_by(set,IncomeGroup) %>% 
  summarize(n())


############################################################
# Summary Table: PAPERS PER JOURNAL 
############################################################
source("./Rscript/functions/SummaryTable.R") 
Table1<-SummaryTable(AllData)
Table1<-Table1[1]
write.csv(Table1, "./tables_figs/Table1.csv", row.names = FALSE)
# Alternative version
Table1v2<-Table1[2]
write.csv(Table1v2, "./tables_figs/Table1v2.csv", row.names = FALSE)



############################################################################
# FIGURE 1: For each journal category, the % of articles by 
# 1st authors from different national income classes
############################################################################
source("./Rscript/functions_figures/Fig1.R") 
Fig1a<-Fig1(AllData,"author_first")
Fig1a
png(file="./tables_figs/plot1a.png",width=1000, height=700)
Fig1a
dev.off()

Fig1b<-Fig1(AllData,"author_last")
png(file="./tables_figs/plot1b.png",width=1000, height=700)
Fig1b
dev.off()


################
# Fig 1 as a waffle plot 
################
source("./Rscript/functions_figures/Fig1waffle.R") 
Fig1wafflePlot<-Fig1waffle(AllData)
# Fig1wafflePlot



################
# Fig 1 with sampled distributions of PW 
################









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
png(file="./tables_figs/Fig2a.png",width=1000, height=700)
Fig2a
dev.off()

Fig2b<-Fig2(AllData,"author_last")
Fig2b
png(file="./tables_figs/Fig2b.png",width=1000, height=700)
Fig2b
dev.off()

############################################################
# FIG 3: for articles in OA journals: the number of articles 
# by 1st authors in each country
# 3a: OA-First  # 3b: PW-First
# 3c: OA-Last   # 3d: PW-Last
############################################################
source("./Rscript/functions_figures/Fig3.R") 
Fig3a<-Fig3(AllData,"author_first","OA")
Fig3a
png(file="./tables_figs/Fig3a.png",width=1000, height=700)
Fig3a
dev.off()

Fig3b<-Fig3(AllData,"author_first","PW")
Fig3b
png(file="./tables_figs/Fig3b.png",width=1000, height=700)
Fig3b
dev.off()

Fig3c<-Fig3(AllData,"author_last","OA")
Fig3c
png(file="./tables_figs/Fig3c.png",width=1000, height=700)
Fig3c
dev.off()

Fig3d<-Fig3(AllData,"author_last","PW")
Fig3d
png(file="./tables_figs/Fig3d.png",width=1000, height=700)
Fig3d
dev.off()


##################################################
# FIG4: Compare the obsered author diversity and 
# richness in OA Journals with an identically sized 
# bootstrapped sample of OA articles
# ie, where the does the observed lie relative to the bootstrap? 
##################################################
# DATA PREP
OA_papers <- AllData %>% filter(JrnlType == "OA")
OA_papers_boot<-sample_n(OA_papers, nrow(OA_papers), replace = TRUE)
source("./Rscript/functions/DivRichCalc.R") 
nboot <-1000 #number of bootstrap samples
Richness <-rep(NA, nboot)
InvSimp <-rep(NA, nboot)
bootstrap.OA.1st<-data.frame(Richness,InvSimp)
rm(Richness,InvSimp)
set.seed(10)
for(i in 1:nboot){
  bootOA<-DivRichCalc(sample_n(OA_papers, nrow(OA_papers), replace = TRUE),"author_first","OA")
  bootstrap.OA.1st[i,1]<-bootOA[1]
  bootstrap.OA.1st[i,2]<-bootOA[2]
}
bootstrap.OA.1st<-arrange(bootstrap.OA.1st)

hist(bootstrap.OA.1st$Richness)
hist(bootstrap.OA.1st$InvSimp)
summary(bootstrap.OA.1st)
OADivRich<-DivRichCalc(AllData,"author_first","OA")
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
write.csv(DivMetrics, 'output/DivMetricsALL.csv', row.names = FALSE)

boxplot(DivMetrics$DeltaDiv)
median(DivMetrics$DeltaDiv,na.rm=TRUE)
# save_name<-paste('output/DivMetricsALL_', Sys.Date(), '.csv') #to add the date of output to filename
# rm(save_name)
source("./Rscript/functions/DivRichCalc.R")
# DivRichCalc<-function(DataSet,AuPosition,JrnlType)
DivRichCalc_result<-DivRichCalc(AllData,"author_last","OA")
DivRichCalc_result

#############################################
# AUTHOR DIVERSITY & RICHNESS: ALL PAPERS POOLED
# Returns results for first authors, last authors, 
# and all authors in a df 
#################################################
source("./Rscript/functions/DivRichCalcSummaryTable_sampled.R")
Table2<-DivRichCalcSummaryTable_sampled(AllData,
                                          AllData_noUSAorCHN,
                                          SubsampledPW.results_First,
                                          SubsampledPW.results_Last,
                                          SubsampledPW.results_All,
                                          SubsampledPW.results_First_NOUSACHN,
                                          SubsampledPW.results_Last_NOUSACHN,
                                          SubsampledPW.results_All_NOUSACHN)
Table2
write.csv(Table2, "./tables_figs/Table2.csv", row.names = FALSE)

  
# source("./Rscript/functions/DivRichCalcSummaryTable.R") # enter as divCalc(df,JrnlType,Author)
# Table2<-DivRichCalcSummaryTable(AllData)
# Table2
# write.csv(Table2, "./tables_figs/Table2.csv", row.names = FALSE)

###############################################################################
# COMPARE OBSERVED OA DIV/RICH with SUBSAMPLE PW ARTICLES
# PW ARTICLES WERE SAMPLED IN FREQUENCY BY JOURNAL MATCHING
# OA JOURNALS

# RETURNS 2 DF: 
# [1]: a value of div and rich from each bootstrp run
# [2]: df of each country slected in each run (with freq)

###############################################################################

########################################
# ALL COUNTRIES (WITH CHN AND USA)
########################################

########################################
# SAMPLED DIV/RICH: FIRST AUTHORS
source("./Rscript/functions/SubSamplePWvsOA_comparison.R")
SubsampledPW.results_First<-SubSamplePWvsOA_comparison(AllData,"author_first")
# Save df of Div and Rich results 
write.csv(SubsampledPW.results_First[1], 
          'output/SubsampledPW.results_RichDiv_FIRST_AUTHOR.csv', 
          row.names = FALSE)
write.csv(SubsampledPW.results_First[2], 
          'output/SubsampledPW.results_Countries_FIRST_AUTHOR.csv', 
          row.names = FALSE)


########################################
# SAMPLED DIV/RICH: LAST AUTHORS
source("./Rscript/functions/SubSamplePWvsOA_comparison.R")
SubsampledPW.results_Last<-SubSamplePWvsOA_comparison(AllData,"author_last")

# Save df of Div and Rich results
write.csv(SubsampledPW.results_Last[1], 
          'output/SubsampledPW.results_RichDiv_LAST_AUTHOR.csv', 
          row.names = FALSE)
write.csv(SubsampledPW.results_Last[2], 
          'output/SubsampledPW.results_Countries_LAST_AUTHOR.csv', 
          row.names = FALSE)


########################################
# SAMPLED DIV/RICH: ALL AUTHORS
source("./Rscript/functions/SubSamplePWvsOA_comparison.R")
SubsampledPW.results_All<-SubSamplePWvsOA_comparison(AllData,"author_all")

# Save df of Div and Rich results
write.csv(SubsampledPW.results_All[1], 
          'output/SubsampledPW.results_RichDiv_ALL_AUTHOR.csv', 
          row.names = FALSE)

write.csv(SubsampledPW.results_All[2], 
          'output/SubsampledPW.results_Countries_ALL_AUTHOR.csv', 
          row.names = FALSE)



########################################
# NO CHN AND USA
# REQUIRES SAMPLING WITH REPLACEMENT DUE TO SMALLER SAMPLE SIZES
########################################

########################################
# SAMPLED DIV/RICH: FIRST AUTHORS
source("./Rscript/functions/SubSamplePWvsOA_comparison.R")
SubsampledPW.results_First_NOUSACHN<-SubSamplePWvsOA_comparison(AllData_noUSAorCHN,"author_first")
# Save df of Div and Rich results 
write.csv(SubsampledPW.results_First_NOUSACHN[1], 
          'output/SubsampledPW.results_RichDiv_FIRST_AUTHOR_NOUSACHN.csv', 
          row.names = FALSE)
write.csv(SubsampledPW.results_First_NOUSACHN[2], 
          'output/SubsampledPW.results_Countries_FIRST_AUTHOR_NOUSACHN.csv', 
          row.names = FALSE)


########################################
# SAMPLED DIV/RICH: LAST AUTHORS
source("./Rscript/functions/SubSamplePWvsOA_comparison.R")
SubsampledPW.results_Last_NOUSACHN<-SubSamplePWvsOA_comparison(AllData_noUSAorCHN,"author_last")

# Save df of Div and Rich results
write.csv(SubsampledPW.results_Last_NOUSACHN[1], 
          'output/SubsampledPW.results_RichDiv_LAST_AUTHOR_NOUSACHN.csv', 
          row.names = FALSE)
write.csv(SubsampledPW.results_Last_NOUSACHN[2], 
          'output/SubsampledPW.results_Countries_LAST_AUTHOR_NOUSACHN.csv', 
          row.names = FALSE)


########################################
# SAMPLED DIV/RICH: ALL AUTHORS
source("./Rscript/functions/SubSamplePWvsOA_comparison.R")
SubsampledPW.results_All_NOUSACHN<-SubSamplePWvsOA_comparison(AllData_noUSAorCHN,"author_all")

# Save df of Div and Rich results
write.csv(SubsampledPW.results_All_NOUSACHN[1], 
          'output/SubsampledPW.results_RichDiv_ALL_AUTHOR_NOUSACHN.csv', 
          row.names = FALSE)

write.csv(SubsampledPW.results_All_NOUSACHN[2], 
          'output/SubsampledPW.results_Countries_ALL_AUTHOR_NOUSACHN.csv', 
          row.names = FALSE)

###############################################

# 
# Subsampled_Income_summary<-Subsampled_Countries %>% 
#   group_by(replicate,IncomeGroup) %>% 
#   summarize(n=n()) %>% 
#   mutate(perc=n/sum(n)*100) %>% 
#   group_by(IncomeGroup) %>% 
#   summarize(avg_n=mean(n),
#             sd_n=sd(n),
#             avg_perc=mean(perc),
#             sd_perc=sd(perc)) %>% 
#   ungroup()
# 
# Subsampled_Income_summary$JrnlType<-"PW_sampled"
# Subsampled_Income_summary$AuthorNum<-1
# # Subsampled_Income_summary$var<-"Region"
# 
# Subsampled_Income_withoutRep<-Subsampled_Income_summary
# Subsampled_Region_summary<-Subsampled_Countries %>% 
#   group_by(replicate,Region) %>% 
#   summarize(n=n()) %>% 
#   mutate(perc=n/sum(n)*100) %>% 
#   group_by(Region) %>% 
#   summarize(avg=mean(perc),sd=sd(perc)) %>% 
#   ungroup()
# Subsampled_Region_summary$AuthorNum<-1
# # Subsampled_Region_summary$var<-"Region"
# Subsampled_Region_summary
# 
# source("./Rscript/functions_figures/Fig2_AvgofSampled.R") 
# # OPTIONS: "author_first"   "author_last" "author_all"
# Fig2a_Avg<-Fig2_AvgofSampled(Subsampled_Income_summary,"author_first")
# Fig2a_Avg
# 
# source("./Rscript/functions_figures/Fig2b_AvgofSampled.R") 
# # OPTIONS: "author_first"   "author_last" "author_all"
# Fig2b_Avg<-Fig2b_AvgofSampled(Subsampled_Region_summary,"author_first")
# Fig2b_Avg


# FIG DIV FIRST AUTHOR
# SubsampledPW.results_First<-read_csv('output/SubsampledPW.results_FIRST_AUTHOR.csv')
# hist(SubsampledPW.results_First$InvSimp)
# summary(SubsampledPW.results_First)

#

########################################
# FIGURES
########################################

# FIG DIVERSITY FIRST AUTHOR
source("./Rscript/functions_figures/Fig5a.R")
Fig5a<-Fig5a(SubsampledPW.results_First,AllData)
Fig5a
png(file="./tables_figs/plot5a.png",width=1000, height=700)
Fig5a
dev.off()

# FIG RICHNESS FIRST AUTHOR
source("./Rscript/functions_figures/Fig5d.R")
Fig5d<-Fig5d(SubsampledPW.results_First,AllData)
Fig5d
png(file="./tables_figs/plot5d.png",width=1000, height=700)
Fig5d
dev.off()

# FIG DIV LAST AUTHOR
source("./Rscript/functions_figures/Fig5b.R")
Fig5b<-Fig5b(SubsampledPW.results_Last,AllData)
Fig5b
png(file="./tables_figs/plot5b.png",width=1000, height=700)
Fig5b
dev.off()

# FIG RICHNESS LAST AUTHOR
source("./Rscript/functions_figures/Fig5e.R")
Fig5e<-Fig5e(SubsampledPW.results_First,AllData)
Fig5e
png(file="./tables_figs/plot5e.png",width=1000, height=700)
Fig5e
dev.off()

########################################
# FIG DIV ALL AUTHOR
source("./Rscript/functions_figures/Fig5c.R")
Plot5c<-Fig5c(SubsampledPW.results_All,AllData)
Plot5c
png(file="./tables_figs/plot5c.png",width=1000, height=700)
Plot5c
dev.off()

# FIG RICHNESS AL AUTHOR
source("./Rscript/functions_figures/Fig5f.R")
Fig5f<-Fig5f(SubsampledPW.results_First,AllData)
Fig5f
png(file="./tables_figs/plot5f.png",width=1000, height=700)
Fig5f
dev.off()


######################################
# FIGS NO CHINA
######################################
# FIG DIVERSITY FIRST AUTHOR
source("./Rscript/functions_figures/Fig5a_noCHNorUSA.R")
Fig5a_NOUSACHN<-Fig5a_noCHNorUSA(SubsampledPW.results_First_NOUSACHN,AllData_noUSAorCHN)
Fig5a_NOUSACHN
png(file="./tables_figs/plot5a_NOUSACHN.png",width=1000, height=700)
Fig5a_NOUSACHN
dev.off()

# FIG RICHNESS FIRST AUTHOR
source("./Rscript/functions_figures/Fig5d_noCHNorUSA.R")
Fig5d_noCHNorUSA<-Fig5d_noCHNorUSA(SubsampledPW.results_First_NOUSACHN,AllData_noUSAorCHN)
Fig5d_noCHNorUSA
png(file="./tables_figs/plot5d_NOUSACHN.png",width=1000, height=700)
Fig5d_NOUSACHN
dev.off()

# FIG DIV LAST AUTHOR
source("./Rscript/functions_figures/Fig5b_noCHNorUSA.R")
Fig5b_NOUSACHN<-Fig5b_noCHNorUSA(SubsampledPW.results_Last_NOUSACHN,AllData_noUSAorCHN)
Fig5b_NOUSACHN
png(file="./tables_figs/plot5b_NOUSACHN.png",width=1000, height=700)
Fig5b_NOUSACHN
dev.off()

# FIG RICHNESS LAST AUTHOR
source("./Rscript/functions_figures/Fig5e_noCHNorUSA.R")
Fig5e_NOUSACHN<-Fig5e_noCHNorUSA(SubsampledPW.results_First_NOUSACHN,AllData_noUSAorCHN)
Fig5e_NOUSACHN
png(file="./tables_figs/plot5e_NOUSACHN.png",width=1000, height=700)
Fig5e_NOUSACHN
dev.off()

########################################
# FIG DIV ALL AUTHOR
source("./Rscript/functions_figures/Fig5c_noCHNorUSA.R")
Plot5c_NOUSACHN<-Fig5c_noCHNorUSA(SubsampledPW.results_All_NOUSACHN,AllData_noUSAorCHN)
Plot5c_NOUSACHN
png(file="./tables_figs/plot5c_NOUSACHN.png",width=1000, height=700)
Plot5c_NOUSACHN
dev.off()

# FIG RICHNESS AL AUTHOR
source("./Rscript/functions_figures/Fig5f_noCHNorUSA.R")
Fig5f_NOUSACHN<-Fig5f_noCHNorUSA(SubsampledPW.results_First_NOUSACHN,AllData_noUSAorCHN)
Fig5f_NOUSACHN
png(file="./tables_figs/plot5f_NOUSACHN.png",width=1000, height=700)
Fig5f_NOUSACHN
dev.off()














# TODO: NOT YET IN FUNCTION FORM, NEEDS CLEAN UP AND ANNOTATION

###############################################
###############################################
# how often are FIRST author and LAST author 
# from same country or region?
LastAuthors <- AllData %>%
  group_by(DOI) %>% 
  filter(AuthorNum == max(AuthorNum)) %>%
  filter(AuthorNum>1) %>% 
  distinct(DOI, .keep_all = TRUE) %>% 
  ungroup()
colnames(LastAuthors)
names(LastAuthors)<-c("Code_last","DOI","Journal","Year","AuthorNum_last","Country_last","JrnlType",
                       "pair_key","Region_last","IncomeGroup_last")

summarize(LastAuthors,n_distinct(DOI))
nrow(LastAuthors)

dupes<-duplicated(LastAuthors$DOI)
summary(dupes)

# eliminate the duplicates
LastAuthors<-LastAuthors[!duplicated(LastAuthors$DOI), ]
dupes<-duplicated(LastAuthors$DOI)
summary(dupes)


FirstAuthors <- AllData %>%
  group_by(DOI) %>% 
  filter(AuthorNum == 1) %>% 
  distinct(DOI, .keep_all = TRUE) %>% 
  ungroup()

dupes<-duplicated(FirstAuthors$DOI)
summary(dupes)

# eliminate the duplicates
FirstAuthors<-FirstAuthors[!duplicated(FirstAuthors$DOI), ]
dupes<-duplicated(FirstAuthors$DOI)
summary(dupes)


colnames(FirstAuthors)
names(FirstAuthors)<-c("Code_first","DOI","Journal","Year","AuthorNum_first","Country_first","JrnlType",
                       "pair_key","Region_first","IncomeGroup_first")


# Do a left join by last author, because only papers that have 
# an author greater than 1, ie, two or more auth9ors, should be considered
FirstLast<-left_join(LastAuthors,FirstAuthors,by=c("DOI","Journal", "Year", "JrnlType","pair_key"))

summarize(FirstLast,n_distinct(DOI))
nrow(FirstLast)
dupes<-FirstLast %>% 
  group_by(DOI) %>% 
  filter(n()>1) %>% 
  arrange(DOI)
dupes

dupes<-duplicated(FirstLast$DOI)
summary(dupes)

# eliminate the duplicates
FirstLast<-FirstLast[!duplicated(FirstLast$DOI), ]
dupes<-duplicated(FirstLast$DOI)
summary(dupes)




FirstLast$FirstLastRegion<-paste(FirstLast$Region_first,FirstLast$Region_last,sep="+")
FirstLast$FirstLastIncome<-paste(FirstLast$IncomeGroup_first,FirstLast$IncomeGroup_last,sep="+")
FirstLast$FirstLastCountry<-paste(FirstLast$Country_first,FirstLast$Country_last,sep="+")
FirstLast$FirstLastCode<-paste(FirstLast$Code_first,FirstLast$Code_last,sep="+")

colnames(FirstLast)

FirstLast_Region<-summary(FirstLast$Region_first==FirstLast$Region_last)
19301/(19301+6240)
# 75%
summary(FirstLast$IncomeGroup_first==FirstLast$IncomeGroup_last)
20300/(20300+5241)
# 80%

summary(FirstLast$Country_first==FirstLast$Country_last)
16477/(16477+9064)
# 65%
FirstLast$Country_check<-(FirstLast$Country_first==FirstLast$Country_last)
FirstLast_diff<-FirstLast %>% 
  filter(Country_check=="FALSE") %>% 
  arrange(FirstLastCode) %>% 
  group_by(FirstLastCountry) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))
FirstLast_diff

#####################
# Region

FirstLast$FirstLastRegion<-as.factor(FirstLast$FirstLastRegion)
FirstLast_RegionSummary<-FirstLast %>%
  ungroup() %>% 
  select(JrnlType,FirstLastRegion) %>% 
  group_by(JrnlType,FirstLastRegion) %>%
  summarize(N=n()) %>% 
  group_by(JrnlType) %>%
  arrange(desc(N)) %>% 
  mutate(Pcnt=(N/sum(N)*100)) %>%
  mutate(cumPcnt=cumsum(Pcnt)) %>% 
  arrange(JrnlType,cumPcnt)
FirstLast_RegionSummary
  
write.csv(FirstLast_RegionSummary, 
          'tables_figs/Table4_FirstLast_RegionSummary.csv', 
          row.names = FALSE)
#####################
# Country 

FirstLast$FirstLastCountry<-as.factor(FirstLast$FirstLastCountry)
FirstLast_CountrySummary<-FirstLast %>%
  ungroup() %>% 
  select(JrnlType,FirstLastCountry) %>% 
  group_by(JrnlType,FirstLastCountry) %>%
  summarize(N=n()) %>% 
  group_by(JrnlType) %>%
  arrange(desc(N)) %>% 
  mutate(Pcnt=(N/sum(N)*100)) %>%
  mutate(cumPcnt=cumsum(Pcnt)) %>% 
  arrange(JrnlType,cumPcnt)
FirstLast_CountrySummary

write.csv(FirstLast_CountrySummary, 
          'tables_figs/Table5_FirstLast_CountrySummary.csv', 
          row.names = FALSE)




#####################
# Income 

FirstLast$FirstLastIncome<-as.factor(FirstLast$FirstLastIncome)
FirstLast_IncomeSummary<-FirstLast %>%
  ungroup() %>% 
  select(JrnlType,FirstLastIncome) %>% 
  group_by(JrnlType,FirstLastIncome) %>%
  summarize(N=n()) %>% 
  group_by(JrnlType) %>%
  arrange(desc(N)) %>% 
  mutate(Pcnt=(N/sum(N)*100)) %>%
  mutate(cumPcnt=cumsum(Pcnt)) %>% 
  arrange(JrnlType,cumPcnt)
FirstLast_IncomeSummary

write.csv(FirstLast_IncomeSummary, 
          'tables_figs/Table6_FirstLast_IncomeSummary.csv', 
          row.names = FALSE)


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
AvgNumbAuthorsAll <- AllData %>% # average number of authors per journal 
  group_by(DOI) %>% 
  filter(AuthorNum == max(AuthorNum)) %>% 
  ungroup()
hist(AvgNumbAuthorsAll$AuthorNum, breaks=150)

summarize(AvgNumbAuthorsAll,avg_n=mean(AuthorNum),sd_n=sd(AuthorNum))



