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
NO_USA_CHN_FL<-read_csv(file="./data_clean/NO_USA_CHN_FL.csv")

one_author_pubs_ALL<-read_csv(file="./data_clean/one_author_pubs_ALL.csv")
coauthor_pubs_ALL<-read_csv(file="./data_clean/coauthor_pubs_ALL.csv")
one_author_pubsNOCHNUSA<-read_csv(file="./data_clean/one_author_pubsNOCHNUSA.csv")
coauthor_pubsNOCHNUSA<-read_csv(file="./data_clean/coauthor_pubsNOCHNUSA.csv")


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

# Solo Author by Journal Type
NumbSoloArticles_JrnlType<-one_author_pubs %>% 
group_by(JrnlType) %>% 
  summarize(n=n_distinct(DOI))
NumbSoloArticles_JrnlType

# Coauthored by journal type
NumbCoAuthoredArticles_JrnlType<-coauthor_pubs %>% 
group_by(JrnlType) %>% 
  summarize(n=n_distinct(DOI))
NumbCoAuthoredArticles_JrnlType

# NUMBER OF COUNTRIES REPRESENTED IN DATASET

# Total by Journal Type
n_countries<-AllData %>% 
  group_by(JrnlType) %>% 
  summarize(n_distinct(Code))
n_countries
# Countries: First Authors by Jrnl Type (including solo authored pubs)
countries_first<-AllData %>% 
  filter(AuthorNum==1) %>% 
  group_by(Code,JrnlType) %>% 
  select(Code,IncomeGroup,Region,JrnlType) %>% 
  slice(1)
ncountries_first<-ungroup(countries_first) %>% 
  group_by(JrnlType) %>% 
  tally(n_distinct(Code))
ncountries_first

# countries Last Authors by jrnl type
countries_last<-AllData %>% 
  group_by(DOI) %>% 
  filter(AuthorNum == max(AuthorNum)) %>%
  group_by(Code,JrnlType) %>% 
  select(Code,IncomeGroup,Region,JrnlType) %>% 
  slice(1)
ncountries_last<-ungroup(countries_last) %>% 
  group_by(JrnlType) %>% 
  tally(n_distinct(Code))
ncountries_last



intersecting_countries<-intersect(countriesOA1, countriesPW1)
OAnotPW<-setdiff(countriesOA1, countriesPW1)
OAnotPW$Code<-as.factor(OAnotPW$Code)
OAnotPW$IncomeGroup<-as.factor(OAnotPW$IncomeGroup)
OAnotPW$Region<-as.factor(OAnotPW$Region)
OAnotPW$set<-"in OA but not PW"
OAnotPW$set<-as.factor(OAnotPW$set)
OAnotPW_summary<-summary(OAnotPW)

PWnotOA<-setdiff(countriesPW1,countriesOA1)
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

# Last authors 
LastAuthors <- AllData %>%
  group_by(DOI) %>% 
  filter(AuthorNum == max(AuthorNum)) %>%
  filter(AuthorNum>1) %>% # This filters outr last authors
  distinct(DOI, .keep_all = TRUE) %>% 
  ungroup()
colnames(LastAuthors)





############################################################
# Summary Table: PAPERS PER JOURNAL 
############################################################
source("./Rscript/functions/SummaryTable.R") 
Tables<-SummaryTable(AllData)
Table1<-Tables[1]
write.csv(Table1, "./tables_figs/Table1.csv", row.names = FALSE)
# Alternative version
Table1v2<-Tables[2]
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
bootstrap_results<-read.csv('./output/bootstrap_results.csv')
bootstrap_results
source("./Rscript/functions_figures/AltFig1.R") 
Fig1_alt<-AltFig1(,"author_first")
Fig1_alt






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

# SOLO AUTHORED, PW
source("./Rscript/functions_figures/Fig3.R") 
Fig3bSoloPW<-Fig3(one_author_pubs,"author_first","PW")
Fig3bSoloPW
png(file="./tables_figs/Fig3bSoloPW.png",width=1000, height=700)
Fig3bSoloPW
dev.off()

# SOLO AUTHORED, OA
Fig3bSoloOA<-Fig3(one_author_pubs,"author_first","OA")
Fig3bSoloOA
png(file="./tables_figs/Fig3bSoloOA.png",width=1000, height=700)
Fig3bSoloOA
dev.off()

# COAUTHORED, FIRST, PW
source("./Rscript/functions_figures/Fig3.R") 
Fig3bCoPW_First<-Fig3(coauthor_pubs,"author_first","PW")
Fig3bCoPW_First
png(file="./tables_figs/Fig3bCoauthoredPW_First.png",width=1000, height=700)
Fig3bCoPW_First
dev.off()

# COAUTHORED, FIRST, OA
source("./Rscript/functions_figures/Fig3.R") 
Fig3aCoOA_First<-Fig3(coauthor_pubs,"author_first","OA")
Fig3aCoOA_First
png(file="./tables_figs/Fig3aCoAuthoredOA_First.png",width=1000, height=700)
Fig3aCoOA_First
dev.off()

# COAUTHORED, LAST, PW
Fig3dCoPW_Last<-Fig3(coauthor_pubs,"author_last","PW")
Fig3dCoPW_Last
png(file="./tables_figs/Fig3dCoAuthoredPW_Last.png",width=1000, height=700)
Fig3dCoPW_Last
dev.off()

# COAUTHORED, LAST, OA
Fig3cCoOA_Last<-Fig3(coauthor_pubs,"author_last","OA")
Fig3cCoOA_Last
png(file="./tables_figs/Fig3cCoAuthoredOA_Last.png",width=1000, height=700)
Fig3cCoOA_Last
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
OADivRich<-DivRichCalc(coauthor_pubs,"author_last","OA")
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
DivMetrics<-DivCalcJrnl(coauthor_pubs)
write.csv(DivMetrics, 'output/DivMetricsALL.csv', row.names = FALSE)

boxplot(DivMetrics$DeltaDiv)
median(DivMetrics$DeltaDiv,na.rm=TRUE)
# save_name<-paste('output/DivMetricsALL_', Sys.Date(), '.csv') #to add the date of output to filename
# rm(save_name)
source("./Rscript/functions/DivRichCalc.R")
# DivRichCalc<-function(DataSet,AuPosition,JrnlType)
DivRichCalc_result<-DivRichCalc(coauthor_pubs,"author_last","OA")
DivRichCalc_result

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
########################################
# SAMPLED DIV/RICH: SINGLE AUTHOR PUBS 
source("./Rscript/functions/SubSamplePWvsOA_comparison.R")
SubsampledPW.results_Solo<-SubSamplePWvsOA_comparison(one_author_pubs,"author_first")

# Save df of Div and Rich results 
write.csv(SubsampledPW.results_Solo[1], 
          'output/SubsampledPW.results_RichDiv_SOLO_ALL.csv', 
          row.names = FALSE)
write.csv(SubsampledPW.results_Solo[2], 
          'output/SubsampledPW.results_Countries_SOLO_ALL.csv', 
          row.names = FALSE)


# SAMPLED DIV/RICH: SINGLE AUTHOR PUBS 
source("./Rscript/functions/SubSamplePWvsOA_comparison.R")
SubsampledPW.results_Solo_NoUSACHN<-SubSamplePWvsOA_comparison(one_author_pubsNOCHNUSA,"author_first")

# Save df of Div and Rich results 
write.csv(SubsampledPW.results_Solo_NoUSACHN[1], 
          'output/SubsampledPW.results_RichDiv_NoUSACHN.csv', 
          row.names = FALSE)
write.csv(SubsampledPW.results_Solo_NoUSACHN[2], 
          'output/SubsampledPW.results_Countries_SOLO_NoUSACHN.csv', 
          row.names = FALSE)


########################################
########################################
# SAMPLED DIV/RICH: FIRST AUTHORS - COAUTHORED PUBS
source("./Rscript/functions/SubSamplePWvsOA_comparison.R")
SubsampledPW.results_First<-SubSamplePWvsOA_comparison(coauthor_pubs,"author_first")

# Save df of Div and Rich results 
write.csv(SubsampledPW.results_First[1], 
          'output/SubsampledPW.results_RichDiv_FIRST_AUTHOR.csv', 
          row.names = FALSE)
write.csv(SubsampledPW.results_First[2], 
          'output/SubsampledPW.results_Countries_FIRST_AUTHOR.csv', 
          row.names = FALSE)
# SubsampledPW.results_First<-read.csv('output/SubsampledPW.results_RichDiv_FIRST_AUTHOR.csv')

########################################
# SAMPLED DIV/RICH: LAST AUTHORS
source("./Rscript/functions/SubSamplePWvsOA_comparison.R")
SubsampledPW.results_Last<-SubSamplePWvsOA_comparison(coauthor_pubs,"author_last")

# Save df of Div and Rich results
write.csv(SubsampledPW.results_Last[1], 
          'output/SubsampledPW.results_RichDiv_LAST_AUTHOR.csv', 
          row.names = FALSE)
write.csv(SubsampledPW.results_Last[2], 
          'output/SubsampledPW.results_Countries_LAST_AUTHOR.csv', 
          row.names = FALSE)

# SubsampledPW.results_Last<-read.csv('output/SubsampledPW.results_RichDiv_LAST_AUTHOR.csv')

########################################
# SAMPLED DIV/RICH: ALL AUTHORS
source("./Rscript/functions/SubSamplePWvsOA_comparison.R")
SubsampledPW.results_All<-SubSamplePWvsOA_comparison(coauthor_pubs,"author_all")

# Save df of Div and Rich results
write.csv(SubsampledPW.results_All[1], 
          'output/SubsampledPW.results_RichDiv_ALL_AUTHOR.csv', 
          row.names = FALSE)

write.csv(SubsampledPW.results_All[2], 
          'output/SubsampledPW.results_Countries_ALL_AUTHOR.csv', 
          row.names = FALSE)

# SubsampledPW.results_All<-read.csv('output/SubsampledPW.results_RichDiv_ALL_AUTHOR.csv')


########################################
# NO CHN AND USA
# REQUIRES SAMPLING WITH REPLACEMENT DUE TO SMALLER SAMPLE SIZES
########################################

########################################
# SAMPLED DIV/RICH: FIRST AUTHORS
source("./Rscript/functions/SubSamplePWvsOA_comparison.R")
SubsampledPW.results_First_NOUSACHN<-SubSamplePWvsOA_comparison(coauthor_pubsNOCHNUSA,"author_first")

# Save df of Div and Rich results 
write.csv(SubsampledPW.results_First_NOUSACHN[1], 
          'output/SubsampledPW.results_RichDiv_FIRST_AUTHOR_NOUSACHN.csv', 
          row.names = FALSE)
write.csv(SubsampledPW.results_First_NOUSACHN[2], 
          'output/SubsampledPW.results_Countries_FIRST_AUTHOR_NOUSACHN.csv', 
          row.names = FALSE)

# SubsampledPW.results_First_NOUSACHN<-read.csv('output/SubsampledPW.results_RichDiv_FIRST_AUTHOR_NOUSACHN.csv')
########################################
# SAMPLED DIV/RICH: LAST AUTHORS
source("./Rscript/functions/SubSamplePWvsOA_comparison.R")
SubsampledPW.results_Last_NOUSACHN<-SubSamplePWvsOA_comparison(coauthor_pubsNOCHNUSA,"author_last")

# Save df of Div and Rich results
write.csv(SubsampledPW.results_Last_NOUSACHN[1], 
          'output/SubsampledPW.results_RichDiv_LAST_AUTHOR_NOUSACHN.csv', 
          row.names = FALSE)
write.csv(SubsampledPW.results_Last_NOUSACHN[2], 
          'output/SubsampledPW.results_Countries_LAST_AUTHOR_NOUSACHN.csv', 
          row.names = FALSE)
# SubsampledPW.results_Last_NOUSACHN<-read.csv('output/SubsampledPW.results_RichDiv_LAST_AUTHOR_NOUSACHN.csv')

########################################
# SAMPLED DIV/RICH: ALL AUTHORS
source("./Rscript/functions/SubSamplePWvsOA_comparison.R")
SubsampledPW.results_All_NOUSACHN<-SubSamplePWvsOA_comparison(coauthor_pubsNOCHNUSA,"author_all")

# Save df of Div and Rich results
write.csv(SubsampledPW.results_All_NOUSACHN[1], 
          'output/SubsampledPW.results_RichDiv_ALL_AUTHOR_NOUSACHN.csv', 
          row.names = FALSE)

write.csv(SubsampledPW.results_All_NOUSACHN[2], 
          'output/SubsampledPW.results_Countries_ALL_AUTHOR_NOUSACHN.csv', 
          row.names = FALSE)
# SubsampledPW.results_All_NOUSACHN<-read.csv( 'output/SubsampledPW.results_RichDiv_ALL_AUTHOR_NOUSACHN.csv')

##########################

bootstrap_results_countries<-bind_rows(SubsampledPW.results_Solo[2],
            SubsampledPW.results_Solo_NoUSACHN[2],
            SubsampledPW.results_First[2],
            SubsampledPW.results_Last[2],
            SubsampledPW.results_All[2],
            SubsampledPW.results_First_NOUSACHN[2],
            SubsampledPW.results_Last_NOUSACHN[2],
            SubsampledPW.results_All_NOUSACHN[2])

bootstrap_results_countries<-bootstrap_results_countries %>% 
  mutate(author = ifelse(Dataset == "one_author_pubs", "solo", author)) %>% 
  mutate(author = ifelse(Dataset == "one_author_pubsNOCHNUSA", "solo", author)) %>% 
  mutate(Dataset = ifelse(Dataset == "one_author_pubsNOCHNUSA", "CHN & USA excluded", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "coauthor_pubsNOCHNUSA", "CHN & USA excluded", Dataset)) %>% 
  mutate(Dataset = ifelse(Dataset == "coauthor_pubs", "All Countries", Dataset)) %>% 
  mutate(Dataset = ifelse(Dataset == "one_author_pubs", "All Countries", Dataset))

write.csv(bootstrap_results_countries, "./output/bootstrap_results_countries.csv", row.names = FALSE)



##############################





#############################################
# AUTHOR DIVERSITY & RICHNESS: ALL PAPERS POOLED
# Returns results for first authors, last authors, 
# and all authors in a df 
#################################################
source("./Rscript/functions/DivRichCalcTable_Solo.R")
Table2_Solo<-DivRichCalcTable_Solo(one_author_pubs,
                             one_author_pubsNOCHNUSA,
                             SubsampledPW.results_Solo,
                             SubsampledPW.results_Solo_NoUSACHN)
Table2_Solo


source("./Rscript/functions/DivRichCalcSummaryTable_sampled.R")
Table2_CoAuthored<-DivRichCalcSummaryTable_sampled(coauthor_pubs,
                                        coauthor_pubsNOCHNUSA,
                                        SubsampledPW.results_First,
                                        SubsampledPW.results_Last,
                                        SubsampledPW.results_All,
                                        SubsampledPW.results_First_NOUSACHN,
                                        SubsampledPW.results_Last_NOUSACHN,
                                        SubsampledPW.results_All_NOUSACHN)
Table2_CoAuthored


Table2_Joint<-bind_rows(Table2_CoAuthored,Table2_Solo)

write.csv(Table2_Joint, "./tables_figs/Table2.csv", row.names = FALSE)



# ##################################################
# # OA Bootstrapped results
# # TODO: CONVERT THIS TO A FUNCTIONS!
# ##################################################
# # DATA PREP
# OA_papers <- coauthor_pubs %>% filter(JrnlType == "OA")
# # OA_papers <- coauthor_pubsNOCHNUSA %>% filter(JrnlType == "OA")
# OA_papers_boot<-sample_n(OA_papers, nrow(OA_papers), replace = TRUE)
# source("./Rscript/functions/DivRichCalc.R") 
# nboot <-1000 #number of bootstrap samples
# Richness <-rep(NA, nboot)
# InvSimp <-rep(NA, nboot)
# bootstrap.OA.1st<-data.frame(Richness,InvSimp)
# rm(Richness,InvSimp)
# set.seed(10)
# for(i in 1:nboot){
#   bootOA<-DivRichCalc(sample_n(OA_papers, nrow(OA_papers), replace = TRUE),"author_first","OA")
#   # bootOA<-DivRichCalc(sample_n(OA_papers, nrow(OA_papers), replace = TRUE),"author_last","OA")
#   # bootOA<-DivRichCalc(sample_n(OA_papers, nrow(OA_papers), replace = TRUE),"author_all","OA")
#   bootstrap.OA.1st[i,1]<-bootOA[1]
#   bootstrap.OA.1st[i,2]<-bootOA[2]
# }
# bootstrap.OA.1st<-arrange(bootstrap.OA.1st)
# bootstrap.OA.1st$Dataset<-"All Countries"
# bootstrap.OA.1st$author<-"author_first"
# bootstrapOA_First<-bootstrap.OA.1st
# hist(bootstrap.OA.1st$Richness)
# # bootstrap.OA.1st$Dataset<-"All Countries"
# # bootstrap.OA.1st$author<-"author_last"
# # bootstrapOA_Last<-bootstrap.OA.1st
# 
# # bootstrap.OA.1st$Dataset<-"All Countries"
# # bootstrap.OA.1st$author<-"author_all"
# # bootstrapOA_All<-bootstrap.OA.1st
# # 
# # bootstrap.OA.1st$Dataset<-"Without CHN & USA"
# # bootstrap.OA.1st$author<-"author_first"
# # bootstrapOA_noUSACHN_First<-bootstrap.OA.1st
# 
# # bootstrap.OA.1st$Dataset<-"Without CHN & USA"
# # bootstrap.OA.1st$author<-"author_last"
# # bootstrapOA_noUSACHN_Last<-bootstrap.OA.1st
# 
# # bootstrap.OA.1st$Dataset<-"Without CHN & USA"
# # bootstrap.OA.1st$author<-"author_all"
# # bootstrapOA_noUSACHN_All<-bootstrap.OA.1st
# # 
# # bootstrapped_OA_combined<-bind_rows(bootstrapOA_noUSACHN_All,
# #                                     bootstrapOA_noUSACHN_First,
# #                                     bootstrapOA_noUSACHN_Last,
# #                                     bootstrapOA_All,
# #                                     bootstrapOA_First,
# #                                     bootstrapOA_Last)
# # 
# # bootstrapped_OA_combined$JrnlType<-"OA"
# # bootstrapped_OA_combined<-as.data.frame(bootstrapped_OA_combined)
# # hist(bootstrapOA_noUSACHN_Last$Richness)
# =
# # OADivRich<-DivRichCalc(AllData,"author_first","OA")
# # OAdiv<-as.numeric((OADivRich)[2])
# # OARich<-as.numeric((OADivRich)[1])
# 



bootstrapped_PW_combined<-bind_rows(SubsampledPW.results_Solo[1],
                                    SubsampledPW.results_Solo_NoUSACHN[1],
                                    SubsampledPW.results_First[1],
                                    SubsampledPW.results_Last[1],
                                    SubsampledPW.results_All[1],
                                    SubsampledPW.results_First_NOUSACHN[1],
                                    SubsampledPW.results_Last_NOUSACHN[1],
                                    SubsampledPW.results_All_NOUSACHN[1])

bootstrapped_PW_combined$JrnlType<-"PW"
str(bootstrapped_PW_combined)
# bootstrapped_PW_combined$author<-as.factor(bootstrapped_PW_combined$author)
# bootstrapped_PW_combined$Dataset<-as.factor(bootstrapped_PW_combined$Dataset)
# bootstrapped_PW_combined$JrnlType<-as.factor(bootstrapped_PW_combined$JrnlType)
summary(bootstrapped_PW_combined)

bootstrapped_PW_combined<-bootstrapped_PW_combined %>% 
  mutate(author = ifelse(Dataset == "one_author_pubs", "solo", author)) %>% 
    mutate(author = ifelse(Dataset == "one_author_pubsNOCHNUSA", "solo", author)) %>% 
    mutate(Dataset = ifelse(Dataset == "one_author_pubsNOCHNUSA", "CHN & USA excluded", Dataset)) %>%
  mutate(Dataset = ifelse(Dataset == "coauthor_pubsNOCHNUSA", "CHN & USA excluded", Dataset)) %>% 
  mutate(Dataset = ifelse(Dataset == "coauthor_pubs", "All Countries", Dataset)) %>% 
  mutate(Dataset = ifelse(Dataset == "one_author_pubs", "All Countries", Dataset))
  
bootstrapped_PW_combined<-as.data.frame(bootstrapped_PW_combined)

bootstrap_results<-bootstrapped_PW_combined
str(bootstrapped_PW_combined)
levels(as.factor(bootstrapped_PW_combined$author))
levels(as.factor(bootstrapped_PW_combined$Dataset))
# bootstrap_results<-bind_rows(bootstrapped_PW_combined,
#                              bootstrapped_OA_combined)




################
OADivRichSolo<-DivRichCalc(one_author_pubs,"author_first","OA")
OAdivFA<-as.numeric((OADivRichSolo)[2])
OARichFA<-as.numeric((OADivRichSolo)[1])

OADivRichSoloNo<-DivRichCalc(one_author_pubsNOCHNUSA,"author_first","OA")
OAdivFANo<-as.numeric((OADivRichSoloNo)[2])
OARichFANo<-as.numeric((OADivRichSoloNo)[1])

OADivRichFA<-DivRichCalc(coauthor_pubs,"author_first","OA")
OAdivFA<-as.numeric((OADivRichFA)[2])
OARichFA<-as.numeric((OADivRichFA)[1])
OADivRichLA<-DivRichCalc(coauthor_pubs,"author_last","OA")
OAdivLA<-as.numeric((OADivRichLA)[2])
OARichLA<-as.numeric((OADivRichLA)[1])

OADivRichFN<-DivRichCalc(coauthor_pubsNOCHNUSA,"author_first","OA")
OAdivFN<-as.numeric((OADivRichFN)[2])
OARichFN<-as.numeric((OADivRichFN)[1])
OADivRichLN<-DivRichCalc(coauthor_pubsNOCHNUSA,"author_last","OA")
OAdivLN<-as.numeric((OADivRichLN)[2])
OARichLN<-as.numeric((OADivRichLN)[1])

summary(bootstrap_results)


write.csv(bootstrap_results,'./output/bootstrap_results.csv',row.names = FALSE)

#################

# New facet label names for country variable
# not needed 
Table2<-ungroup(Table2_Joint)
Table2<-Table2 %>% arrange(desc(metric),author)
colnames(Table2)
colnames(bootstrap_results_fig_data)

author<-rep(Table2$author,1,strings.as.factors=TRUE)
# metric<-rep(c("Richness","Richness","Richness","Diversity","Diversity","Diversity"),4)

CIlow_Rich<-c(rep(NA,8),Table2$CIlow[1:4],Table2$CIlow1[1:4])
CIlow_Div<-c(rep(NA,8),Table2$CIlow[5:8],Table2$CIlow1[5:8])
CIhigh_Rich<-c(rep(NA,8),Table2$CIhigh[1:4],Table2$CIhigh1[1:4])
CIhigh_Div<-c(rep(NA,8),Table2$CIhigh[5:8],Table2$CIhigh1[5:8])

RichnessOA<-c(as.numeric(Table2$OA_AllCountries[1:4]),as.numeric(Table2$OA_noCHNorUSA[1:4]),
              rep(NA,8))
InvSimpOA<-c(as.numeric(Table2$OA_AllCountries[5:8]),as.numeric(Table2$OA_noCHNorUSA[5:8]),
             rep(NA,8))

JrnlType<-c(rep("OA",8),rep("PW",8))
labeltext <- rep(c("OA['obs']","PW['boot. mean']"), each=8)
# labeltext <- paste("labeltext"," == ","InvSimp",sep="")
dataset<-rep((rep(c("All Countries","CHN & USA excluded"),each=4)),2)
Table2.2<-as.data.frame(cbind(author=as.character(author),Dataset=as.character(dataset),
                              JrnlType,InvSimpOA,RichnessOA,CIlow_Rich,CIhigh_Rich,CIlow_Div,CIhigh_Div,labeltext),stringsAsFactors = TRUE)
Table2.2$author<-gsub("first","author_first",Table2.2$author)
Table2.2$author<-gsub("last","author_last",Table2.2$author)
Table2.2$author<-gsub("all","author_all",Table2.2$author)
# Table2.2$author<-gsub("solo","Single Authors",Table2.2$author)
colnames(bootstrap_results_fig_data)
str(Table2.2)
Table2.2$RichnessOA<-as.integer(as.character(Table2.2$RichnessOA))
Table2.2$InvSimpOA<-as.integer(as.character(Table2.2$InvSimpOA))
# bootstrap_results_fig_data<-left_join(bootstrap_results_fig_data,Table2.2,by=c("author","Dataset","JrnlType"))

# bootstrap_results_fig_data$CIhigh_Div<-as.integer(bootstrap_results_fig_data$CIhigh_Div)

# brewer.pal(3, "Set1") gets the hex codes from palette
# [1] "#E41A1C" "#377EB8" "#4DAF4A"

str(Table2.2)

Table2.2$CIlow_Div<-as.integer(as.character(Table2.2$CIlow_Div))
Table2.2$CIlow_Rich<-as.integer(as.character(Table2.2$CIlow_Rich))
Table2.2$CIhigh_Div<-as.integer(as.character(Table2.2$CIhigh_Div))
Table2.2$CIhigh_Rich<-as.integer(as.character(Table2.2$CIhigh_Rich))
Table2.2$facet_title<-paste(Table2.2$author,Table2.2$Dataset,sep=' , ')
# Table2.2$facet_title<-gsub("author_first", "First Authors", Table2.2$facet_title)
# Table2.2$facet_title<-gsub("author_last", "Last Authors", Table2.2$facet_title)
# Table2.2$facet_title<-gsub("author_all", "All Authors", Table2.2$facet_title)
# Table2.2$facet_title<-gsub("solo", "Single Authors", Table2.2$facet_title)

write.csv(Table2.2,'./output/Table2.2.csv',row.names = FALSE)

###################
# Figure - Diversity
source("./Rscript/functions_figures/DivBootFig.R")
DivBootFig(bootstrap_results,Table2.2)

  ####################################
  # Figure - Richness
source("./Rscript/functions_figures/RichBootFig.R")
RichBootFig(bootstrap_results,Table2.2)

  ############################
  
  
  
  
  
  
  
  
  # brewer.pal(3, "Set1") gets the hex codes from palette
  # [1] "#E41A1C" "#377EB8" "#4DAF4A"
  author.labels <- c(author_first = "First Authors", author_last = "Last Authors")
  pRich<-
    ggplot(bootstrap_results_fig_data, aes(x=Richness,fill=JrnlType)) + 
    geom_histogram(bins=40,color="black", alpha=0.8, position = 'identity') +
    scale_fill_brewer(palette = "Set1")+
    facet_grid(cols = vars(author), rows=vars(Dataset),labeller=labeller(author = author.labels))+
    # geom_vline(aes(xintercept=OADiv_First),
    #            color="darkblue", linetype="dashed", size=1)+
    geom_vline(data = subset(bootstrap_results_fig_data, 
                             Dataset == "CHN & USA excluded" & author=="author_first"),
               aes(xintercept = OARichFN),
               color="#E41A1C", linetype="dashed", size=1)+
    geom_vline(data = subset(bootstrap_results_fig_data, 
                             Dataset == "CHN & USA excluded" & author=="author_last"),
               aes(xintercept = OARichLN),
               color="#E41A1C", linetype="dashed", size=1)+
    geom_vline(data = subset(bootstrap_results_fig_data, 
                             Dataset == "All Countries" & author=="author_first"),
               aes(xintercept = OARichFA),
               color="#E41A1C", linetype="dashed", size=1)+
    geom_vline(data = subset(bootstrap_results_fig_data, 
                             Dataset == "All Countries" & author=="author_last"),
               aes(xintercept = OARichLA),
               color="#E41A1C", linetype="dashed", size=1)+
    # annotate("text", x = 14.2, y = 105,label =(paste(probFirst,"%",sep="")))+
    # # geom_label(label="Observed OA Diversity (0%)", x=13,y=275,
    # #            label.padding = unit(0.55, "lines"), # Rectangle size around label
    # #            label.size = 0.5,color = "darkblue", fill="white")+
    xlab("Author Geographic Richness")+
    ylab("Frequency")+
    guides(fill=guide_legend("Journal\nCategory"))+
    # scale_x_continuous(expand = c(0,0),limits = c(0,35))+
    scale_y_continuous(expand = c(0,0),limits = c(0,200))
  
  pRich<-pRich+
    theme_classic()+ 
    theme(
      axis.text.x = element_text(size=18),
      axis.text.y = element_text(size=18),
      axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 24, hjust=0.5,),
      strip.text.x = element_text(size = 18,face="bold"),
      strip.text.y = element_text(size = 18,face="bold"),
      legend.title = element_text(colour="black", size=18, 
                                  face="bold"),
      legend.text = element_text(colour="black", size=14),
      panel.spacing.x =unit(.5, "lines"), 
      panel.spacing.y=unit(1,"lines"),
      plot.margin =unit(c(1,1,1,1.5), "lines")  
    )
  pRich
  
  
  
  
  
  




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
Fig5c<-Fig5c(SubsampledPW.results_All,AllData)
Fig5c
png(file="./tables_figs/Fig5c.png",width=1000, height=700)
Fig5c
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
Fig5a_NOUSACHN<-Fig5a_noCHNorUSA(SubsampledPW.results_First_NOUSACHN,NO_USA_CHN_FL)
Fig5a_NOUSACHN
png(file="./tables_figs/plot5a_NOUSACHN.png",width=1000, height=700)
Fig5a_NOUSACHN
dev.off()

# FIG RICHNESS FIRST AUTHOR
source("./Rscript/functions_figures/Fig5d_noCHNorUSA.R")
Fig5d_noCHNorUSA<-Fig5d_noCHNorUSA(SubsampledPW.results_First_NOUSACHN,NO_USA_CHN_FL)
Fig5d_noCHNorUSA
png(file="./tables_figs/plot5d_NOUSACHN.png",width=1000, height=700)
Fig5d_NOUSACHN
dev.off()

# FIG DIV LAST AUTHOR
source("./Rscript/functions_figures/Fig5b_noCHNorUSA.R")
Fig5b_NOUSACHN<-Fig5b_noCHNorUSA(SubsampledPW.results_Last_NOUSACHN,NO_USA_CHN_FL)
Fig5b_NOUSACHN
png(file="./tables_figs/plot5b_NOUSACHN.png",width=1000, height=700)
Fig5b_NOUSACHN
dev.off()

# FIG RICHNESS LAST AUTHOR
source("./Rscript/functions_figures/Fig5e_noCHNorUSA.R")
Fig5e_NOUSACHN<-Fig5e_noCHNorUSA(SubsampledPW.results_First_NOUSACHN,NO_USA_CHN_FL)
Fig5e_NOUSACHN
png(file="./tables_figs/plot5e_NOUSACHN.png",width=1000, height=700)
Fig5e_NOUSACHN
dev.off()

########################################
# FIG DIV ALL AUTHOR
source("./Rscript/functions_figures/Fig5c_noCHNorUSA.R")
Fig5c_NOUSACHN<-Fig5c_noCHNorUSA(SubsampledPW.results_All_NOUSACHN,NO_USA_CHN_FL)
Fig5c_NOUSACHN
png(file="./tables_figs/Fig5c_NOUSACHN.png",width=1000, height=700)
Fig5c_NOUSACHN
dev.off()

# FIG RICHNESS AL AUTHOR
source("./Rscript/functions_figures/Fig5f_noCHNorUSA.R")
Fig5f_NOUSACHN<-Fig5f_noCHNorUSA(SubsampledPW.results_First_NOUSACHN,NO_USA_CHN_FL)
Fig5f_NOUSACHN
png(file="./tables_figs/plot5f_NOUSACHN.png",width=1000, height=700)
Fig5f_NOUSACHN
dev.off()


# PUTTING PANELS TOGETHER IN A SINGLE FIGURE
library(gridExtra)

RICHNESS.FIG<-grid.arrange(Fig5d,Fig5e,Fig5f, Fig5d_noCHNorUSA,Fig5e_NOUSACHN,Fig5f_NOUSACHN, nrow = 2)
DIV.FIG<-grid.arrange(Fig5a,Fig5b,Fig5c, Fig5a_NOUSACHN,Fig5b_NOUSACHN,Fig5c_NOUSACHN, nrow = 2)











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


foo<-FirstLast %>% filter(JrnlType=="OA") 
summary(foo$Country_first==foo$Country_last)

foo2<-FirstLast %>% filter(JrnlType=="PW") 
summary(foo2$Country_first==foo2$Country_last)


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


summary(FirstLast$Country_check)

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
