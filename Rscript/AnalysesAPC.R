#=============================================================================================================#
# Script created by Jesse Borden and Emilio M. Bruna (embruna@ufl.edu) analyze
# data and generate figures/tables for : PAPER CITATION TO BE ADDED 
# Script created in  R version 3.6.3 (2020-02-29)
# Uses packages tidyverse_1.3.0 , vegan_2.5-6, reshape0.8.8
#=============================================================================================================#

# libraries
library(tidyverse)
# library(sampling)
library(reshape)
library(vegan)
# library(purrr)
# library(stringr)

# load the data to be used in analyses
AllData<-read_csv(file="./data_clean/AllData.csv")

############################################################
# Remove any journal pairs for whihc data re incomplete
############################################################

# TODO: 
# no articles for "Clinics and Research in Hepatology and
# Gastroenterology: X (9)" so exclude it and mirror
# Diabetes and Metabolism: X (13) is missing (no info on page) 
# Europ. J Obsterics, Gynecology: X (16) none published) 
missing_jrnls<-c(9,13,16)
AllData<-AllData %>% filter(!pair_key%in% missing_jrnls)
rm(missing_jrnls)

############################################################
# Total number of journals
############################################################
n_journals <- AllData %>% 
  group_by(JrnlType) %>% 
  summarize(n=n_distinct(Journal))
n_journals
sum(n_journals$n)

############################################################
# Total number of articles in study
############################################################
NumbArticles <- AllData %>% #number of papers per journal
  group_by(pair_key,JrnlType,Journal) %>% 
  summarize(n=n_distinct(DOI))%>% 
  arrange(Journal)
NumbArticles
Total_No_Articles<-sum(NumbArticles$n)
arrange(NumbArticles,desc(n))
############################################################
# Number of articles in each OA Journal
# and
# total number of OA articles (all journals pooled)
############################################################
NumbArtOA <- NumbArticles %>% #number of articles that are Open Access
  filter(JrnlType == "OA")
NumbArtOA

TOTAL_NumbArtOA<-sum(NumbArtOA$n) #total number summed accross all OA journals
TOTAL_NumbArtOA
 
############################################################
# Number of articles in each PW Journal
# and
# total number of PW articles (all journals pooled)
############################################################

NumbArtPW <- NumbArticles %>% #number of articles that are paywall
  filter(JrnlType == "PW")
NumbArtPW

TOTAL_NumbArtPW<-sum(NumbArtPW$n) #total number summed accross all OA journals
TOTAL_NumbArtPW



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

############################################################################
# Make a table of the results 
############################################################################
Table1<-ungroup(NumbArticles) %>%
  # select(-pair_key) %>% 
  select(JrnlType,Journal,pair_key) %>% 
  spread(JrnlType,Journal)
Table1

Table1.1<-ungroup(NumbArticles) %>%
  # select(-pair_key) %>% 
  select(JrnlType,n,pair_key) %>% 
  spread(JrnlType,n)
Table1.1
Table1<-bind_cols(Table1,Table1.1) %>% 
  select(PW,PW1,OA,OA1)
Table1$PW<-as.character(Table1$PW)
Table1$PW<-str_to_title(Table1$PW)
Table1$OA<-as.character(Table1$OA)
Table1$OA<-str_to_title(Table1$OA)
Table1$OA<-str_replace(Table1$OA, " And ", " and ")
Table1$OA<-str_replace(Table1$OA, " Of ", " of ")
Table1$PW<-str_replace(Table1$PW, " And ", " and ")
Table1$PW<-str_replace(Table1$PW, " Of ", " of ")
# Table1$PW<-str_replace(Table1$PW, "Journal ", "J. ")
# Table1$OA<-str_replace(Table1$PW, "Journal ", "J. ")
names(Table1)<-c("Journal","Articles (n)","Open Access Mirror","Articles (n)")
rm(Table1.1)
pub_totals<-c("Total: ",TOTAL_NumbArtPW,"Total:",TOTAL_NumbArtOA)
Table1<-rbind(Table1,pub_totals)
write.csv(Table1, "./tables_figs/Table1.csv", row.names = FALSE)

####ALTERNATIVE, MORE EFFICIENT TABLE 1
Table1v2<-Table1
names(Table1v2)<- names(Table1v2)<-c("Journal","No. Articles","Open Access Mirror","No. Articles - OA Mirror")
Table1v2<-Table1v2 %>% select(-'Open Access Mirror')
write.csv(Table1v2, "./tables_figs/Table1v2.csv", row.names = FALSE)



############################################################################
# For each journal category: 
# the % of articles by 1st authors from different national income classes
############################################################################
################
# PREP THE DATA 
################
first_author_income_cats<-AllData %>% 
  filter(AuthorNum==1) %>% 
  group_by(JrnlType,IncomeGroup) %>% 
  tally() %>% 
  mutate(percentage=n/sum(n)*100)





################
# FIGURE 
################

png(file="./tables_figs/plot1.png",width=1000, height=700)
labels <- c(OA = "Open Access Articles", PW = "Paywalled Articles")
plot1<-ggplot(first_author_income_cats, aes(x=IncomeGroup,y = percentage))+
  geom_bar(stat = "identity")+
  xlab("Author National Income Category") + ylab("Articles Published (%)")+
  # scale_x_discrete(limits = bar_order)+
  facet_grid(cols = vars(JrnlType),labeller=labeller(JrnlType = labels))+
  ggtitle("Figure 1")
plot1<-plot1+
  theme_classic()+
  theme(
    axis.text.x = element_text(size=18,angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size=18),
    axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
    axis.title.y=element_text(colour="black", size = 24, vjust=2),
    strip.text.x = element_text(size = 18),
    plot.margin =unit(c(1,1,1,1.5), "lines")   #plot margin - top, right, bottom, left
    )
plot1
dev.off()
################
# As a waffle 
################
library(waffle)
waffle_df<-filter(first_author_income_cats,JrnlType=="OA")
percOA<-waffle_df$percentage
names(percOA)<-as.character(waffle_df$IncomeGroup)
waffle_df2<-filter(first_author_income_cats,JrnlType=="PW")
percPW<-waffle_df2$percentage
names(percPW)<-as.character(waffle_df2$IncomeGroup)

waffle(percPW,rows=14, size=1,legend_pos = "top", title = "Paywalled Articles")
waffle(percOA,rows=6, size=1,legend_pos = "top", title = "Open Access Articles")

gridExtra::grid.arrange(
  waffle(percOA,rows=14, size=1,legend_pos = "right", xlab = "Open Access Articles",flip=TRUE,title="Fig. 1: 1st Author National Income Category (%)"),
  waffle(percPW,rows=14, size=1,legend_pos = "right", xlab = "Paywalled Articles", flip=TRUE, title =" ") #to get the spacing right include a blank title
)

rm(waffle_df,waffle_df2)


############################################################
# For 1st authors from each national income class: 
# The % of articles that were in PW vs OA journals
############################################################

################
# PREP THE DATA 
################

# Here group by countries first. The idea is to compare
# where authors from each income class publish

# ISOLATE FIRST AUTHORS
first_jrnlcat_by_income_cats<-AllData %>% 
  filter(AuthorNum==1) %>% 
  group_by(IncomeGroup,JrnlType) %>% 
  tally() %>% 
  mutate(percentage=n/sum(n)*100)


# ISOLATE LAST AUTHORS
last_jrnlcat_by_income_cats<-AllData %>% 
  group_by(DOI) %>% 
  filter(AuthorNum == max(AuthorNum)) %>% 
  group_by(IncomeGroup,JrnlType) %>% 
  tally() %>% 
  mutate(percentage=n/sum(n)*100) 
  

# SELECT 1st or last Author
plot2data<-first_jrnlcat_by_income_cats
# OR
# plot2data<-last_jrnlcat_by_income_cats
################
# FIGURE 2
################
png(file="./tables_figs/plot2.png",width=1000, height=700)
plot2<-ggplot(plot2data, aes(x=JrnlType,y = percentage, fill=JrnlType))+
  geom_bar(stat = "identity")+
  facet_grid(cols = vars(IncomeGroup))+
  xlab("Author National Income Category") + ylab("Articles published in\nPW vs. OA Journals (%)")+
  # scale_fill_manual(values=c("midnightblue","dimgray"), name="Journal Category",
  scale_fill_manual(values=c("dimgray","dimgray"), name="Journal Category",
                         breaks=c("OA", "PW"),
                         labels=c("Open Access", "Paywalled"))+
  scale_y_continuous(breaks = seq(0, 100, 10))+
  # coord_flip()+
  ggtitle("Fig. 2")
plot2<-plot2+
  theme_classic()+
  theme(
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
    axis.title.y=element_text(colour="black", size = 24, vjust=2),
    legend.position="none",
    strip.text.x = element_text(size = 20),
    plot.margin =unit(c(1,1,1,1.5), "lines")   #plot margin - top, right, bottom, left
    # legend.text =element_text(colour="black", size = 16),
    # legend.title =element_text(colour="black", size = 18),
  )
plot2
dev.off()

############################################################
# for articles in OA journals: the number of articles by
# 1st authors in each country
############################################################

#################
# SELECT OA OR PW, FIRST OR LAST AUTHOR
#################

JournalCategory<-"OA"
# OR 
# JournalCategory<-"PW"

# FIRST AUTHOR
AllGeo <-AllData %>%
  group_by(DOI) %>% 
  filter(AuthorNum == 1) %>%
  filter(JrnlType==JournalCategory) %>% 
  filter(Country != "NA" & Code != "NA") %>%
  group_by(Country, Code)%>%
  tally() %>% 
  arrange(desc(n))
sum(AllGeo$n)

# LAST AUTHOR
# AllGeo <-AllData %>%
#   group_by(DOI) %>%
#   filter(AuthorNum == max(AuthorNum)) %>%
#   filter(JrnlType==JournalCategory) %>%
#   filter(Country != "NA" & Code != "NA") %>%
#   group_by(Country, Code)%>%
#   tally() %>%
#   arrange(desc(n))
# sum(AllGeo$n)

head(AllData,10)


AllGeo <-left_join(AllGeo, CountryData, by = "Code") %>%
  filter(IncomeGroup != "NA") %>% 
  ungroup() %>%
  mutate(perc=n/sum(n)*100)
AllGeo$perc<-round(AllGeo$perc,2)
AllGeo<-arrange(AllGeo,desc(AllGeo$perc))
AllGeo<- transform(AllGeo,Code = reorder(Code,perc))
AllGeo$IncomeGroup<-droplevels(AllGeo$IncomeGroup)
AllGeo$IncomeGroup <- ordered(AllGeo$IncomeGroup, levels = c("High income", "Upper middle income","Lower middle income","Low income"))
levels(AllGeo$IncomeGroup)

cutoff = 25 # This is how many countries you want on the chart, all the rest will be in "OTHER"
AllGeo<-arrange(AllGeo, desc(perc)) %>% select(Code,n,perc, IncomeGroup)
most.common.authors<-slice(AllGeo, 1:cutoff)
lst.common.authors<-slice(AllGeo, (cutoff+1):nrow(AllGeo)) 
# lst.common.authors$Code<-"all others"
# lst.common.authors$IncomeGroup<-"all others"
lst.common.authors<-lst.common.authors %>% 
  group_by(IncomeGroup) %>% 
  summarize(n=sum(n),perc=sum(perc),n_countries=n_distinct(Code)) 
lst.common.authors$Code<-paste(lst.common.authors$n_countries,lst.common.authors$IncomeGroup,"countries", sep = " ", collapse = NULL)
lst.common.authors$Code<-gsub("income countries","",lst.common.authors$Code)
most.common.authors<-bind_rows(most.common.authors, lst.common.authors)
most.common.authors$Code<-as.factor(most.common.authors$Code)
most.common.authors

# This is needed to put them in order in the plot with OTHER at the end of the graph
order<-rev(seq(1:nrow(most.common.authors))) #REV is what makes it go tyop to bottom if flipped coordinates
most.common.authors$Code <- factor(most.common.authors$Code,most.common.authors$Code[levels = order])
# rm(order,AllGeo,lst.common.authors)
most.common.authors

# png(file="./tables_figs/plot3a_OA.png",width=1000, height=700)
# png(file="./tables_figs/plot3b_PW.png",width=1000, height=700)
png(file="./tables_figs/plot3c_OA.png",width=1000, height=700)
# png(file="./tables_figs/plot3d_PW.png",width=1000, height=700)
plot3<-ggplot(most.common.authors, aes(x=Code,y=perc, fill=IncomeGroup))+
  geom_bar(stat = "identity")+
  # geom_text(size = 3, position = position_stack(vjust = 0.5))+
  xlab("Country in which\nfirst author\nis based") + ylab("Percentage of Articles")+
  ylim(0, 35)+
  coord_flip()+
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"), 
                    name="National Income Category",
                    breaks=c("High income", "Upper middle income","Lower middle income","Low income"))+
  ggtitle("Fig. 3a: OA Journals")
  
plot3<-plot3+theme_light()+
  theme(
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
    axis.title.y=element_text(colour="black", size = 24, hjust=0.5,vjust=0.5,angle = 0),
    legend.position="right",
    plot.margin =unit(c(1,1,1,1.5), "lines")  
  )
plot3  
dev.off()



##################################################
# SUBSET and bootstrap Paywall Journals 
# by the number found in Open Access Journals
##################################################

OA_papers <- AllData %>% filter(JrnlType == "OA")
OA_papers_boot<-sample_n(OA_papers, nrow(OA_papers), replace = TRUE)
source("./Rscript/functions/DivCalcPooledFirst.R") 
nboot <-1000 #number of bootstrap samples
Richness <-rep(NA, nboot)
InvSimp <-rep(NA, nboot)
bootstrap.OA.1st<-data.frame(Richness,InvSimp)
rm(Richness,InvSimp)
set.seed(10)
for(i in 1:nboot){
  bootOA<-DivCalcPooledFirst(sample_n(OA_papers, nrow(OA_papers), replace = TRUE))
  bootstrap.OA.1st[i,1]<-bootOA[1]
  bootstrap.OA.1st[i,2]<-bootOA[2]
}

OAdiv<-as.numeric(DivCalcPooledFirst(OA_papers)[2])
OARich<-as.numeric(DivCalcPooledFirst(OA_papers)[1])

########## BOOTSTRAP 1st author diversity OA

png(file="./tables_figs/plot4a.png",width=1000, height=700)
OAbootPlot<-ggplot(bootstrap.OA.1st, aes(x=InvSimp)) + 
  geom_histogram(bins=25, colour="black", fill="white")+
  geom_vline(aes(xintercept=OAdiv),
             color="darkblue", linetype="dashed", size=1)+
    geom_label(label="Observed OA Diversity", x=16,y=115,
             label.padding = unit(0.55, "lines"), # Rectangle size around label
             label.size = 0.5,color = "darkblue", fill="white")+
  xlab("National Diversity (Inv. Simpson's) of First Authors") + ylab("Frequency")+
  # xlab("Richness (No. of Countries)") + ylab("Frequency")+
  scale_y_continuous(expand = c(0,0),limits = c(0,120))
OAbootPlot<-OAbootPlot+
  theme_classic()+
  theme(
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
    axis.title.y=element_text(colour="black", size = 24, hjust=0.5,vjust=0.5,angle = 0),
   plot.margin =unit(c(1,1,1,1.5), "lines")  
  )
OAbootPlot
dev.off()



png(file="./tables_figs/plot4b.png",width=1000, height=700)
OAbootPlot2<-ggplot(bootstrap.OA.1st, aes(x=Richness)) + 
  geom_histogram(bins=25, colour="black", fill="white")+
  geom_vline(aes(xintercept=OARich),
             color="darkblue", linetype="dashed", size=1)+
  geom_label(label="Observed\nOA Richness", x=68,y=160,
             label.padding = unit(0.55, "lines"), # Rectangle size around label
             label.size = 0.5,color = "darkblue", fill="white")+
  xlab("No. of Countries - First Authors") + ylab("Frequency")+
    scale_y_continuous(expand = c(0,0),limits = c(0,170))
OAbootPlot2<-OAbootPlot2+
  theme_classic()+
  theme(
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
    axis.title.y=element_text(colour="black", size = 24, hjust=0.5,vjust=0.5,angle = 0),
    plot.margin =unit(c(1,1,1,1.5), "lines")  
  )
OAbootPlot2
dev.off()

#################################################################
# calculate diversity indices BY CAT AND JOURNAL
#################################################################

source("./Rscript/functions/DivCalcJrnl.R") # enter as divCalc(df,JrnlType,Author)
# OA JOURNALS by 1st AUTHORS and journal
OA_papers <- AllData %>% filter(JrnlType == "OA")
DivMetricsOA<-DivCalcJrnl(OA_papers)

# PW JOURNALS by 1st AUTHORS and jourmal
PW_papers <- AllData %>% filter(JrnlType == "PW")
DivMetricsPW<-DivCalcJrnl(PW_papers)

###################################
# Put them together in wide format
###################################
# DivMetrics<-bind_rows(DivMetricsOA,DivMetricsPW) %>% 
#   arrange(pair_key)
# colnames(DivMetrics)
DivMetricsPW_wide<-DivMetricsPW %>% 
  select(-Journal) %>% 
  spread(JrnlType,DivSimpson) %>% 
  dplyr::rename(PW_DivSimpson=PW,PW_rich=rich,PW_abund=abund,PW_EffectSpecNum=EffectSpecNum)
DivMetricsOA_wide<-DivMetricsOA %>% 
  select(-Journal) %>% 
  spread(JrnlType,DivSimpson) %>% 
  dplyr::rename(OA_DivSimpson=OA,OA_rich=rich,OA_abund=abund,OA_EffectSpecNum=EffectSpecNum)
DivMetricsALL<-left_join(DivMetricsPW_wide,DivMetricsOA_wide,by="pair_key")
# Add Difference in diversity score (DeltaDiv)
DivMetricsALL$DeltaDiv <- DivMetricsALL$PW_DivSimpson - DivMetricsALL$OA_DivSimpson
DivMetricsALL$DeltaRich <- DivMetricsALL$PW_rich - DivMetricsALL$OA_rich
str(DivMetricsALL)
boxplot(DivMetricsALL$DeltaDiv)
median(DivMetricsALL$DeltaDiv,na.rm=TRUE)
save_name<-paste('output/DivMetricsALL_', Sys.Date(), '.csv') #to add the date of output to filename
write.csv(DivMetricsALL, save_name, row.names = FALSE)
rm(save_name)
#############################################
# DIVERSITY & RICHNESS of ENTIRE POOL 
# By FIRST AUTHOR, LAST AUTHOR, or ALL AUTHORS
#############################################

#################
# By First Author
#################
source("./Rscript/functions/DivCalcPooledFirst.R") 
Div_OA_pool_first<-DivCalcPooledFirst(OA_papers)
#DIversity for the entire Community of PW papers
Div_PW_pool_first<-DivCalcPooledFirst(PW_papers)

# Binding together
DivMetricsPooled_first <- as.data.frame(cbind(Div_OA_pool_first[1], Div_PW_pool_first[1],Div_OA_pool_first[2], Div_PW_pool_first[2]))
DivMetricsPooled_first <-DivMetricsPooled_first %>% dplyr::rename(OA_richness=V1,PW_richness=V2,OA_invSimp=V3,PW_invSimp=V4)
rownames(DivMetricsPooled_first)<-nrow(DivMetricsPooled_first)
# write.csv(DivMetricsPooled_first, "CleanData/DivMetricsPooled_first.csv", row.names = FALSE)

#################
# By Last Author
#################
source("./Rscript/functions/DivCalcPooledLast.R") 
Div_OA_pool_last<-DivCalcPooledLast(OA_papers)
#DIversity for the entire Community of PW papers
Div_PW_pool_last<-DivCalcPooledLast(PW_papers)

# Binding together
DivMetricsPooled_last <- as.data.frame(cbind(Div_OA_pool_last[1], Div_PW_pool_last[1],Div_OA_pool_last[2], Div_PW_pool_last[2]))
DivMetricsPooled_last <-DivMetricsPooled_last %>% dplyr::rename(OA_richness=V1,PW_richness=V2,OA_invSimp=V3,PW_invSimp=V4)
rownames(DivMetricsPooled_last)<-nrow(DivMetricsPooled_last)
# write.csv(DivMetricsPooled_last, "CleanData/DivMetricsPooled_last.csv", row.names = FALSE)

#################
# By All Author
#################
source("./Rscript/functions/DivCalcPooledAll.R") 
Div_OA_pool_all<-DivCalcPooledAll(OA_papers)
#DIversity for the entire Community of PW papers
Div_PW_pool_all<-DivCalcPooledAll(PW_papers)

# Binding together
DivMetricsPooled_all <- as.data.frame(cbind(Div_OA_pool_all[1], Div_PW_pool_all[1],Div_OA_pool_all[2], Div_PW_pool_all[2]))
DivMetricsPooled_all <-DivMetricsPooled_all %>% dplyr::rename(OA_richness=V1,PW_richness=V2,OA_invSimp=V3,PW_invSimp=V4)
rownames(DivMetricsPooled_all)<-nrow(DivMetricsPooled_all)
# write.csv(DivMetricsPooled_all, "CleanData/DivMetricsPooled_all.csv", row.names = FALSE)

#######################################
# Table Div and Richness Pooled
#######################################



Table2<-bind_rows(unlist(DivMetricsPooled_first),
                  unlist(DivMetricsPooled_last),
                         unlist(DivMetricsPooled_all))
Table2$Authors<-c("First","Last","All")
# Table2$OA_invSimp<-round(as.numeric(Table2$OA_invSimp))
# Table2$PW_invSimp<-round(as.numeric(Table2$PW_invSimp))
Table2<- Table2 %>% select("Authors","OA_richness.Richness","PW_richness.Richness","OA_invSimp.Richness","PW_invSimp.Richness")  
names(Table2)<-c("Authors","OA Richness","PW Richness","OA Diversity","PW Diversity")
write.csv(Table2, "./tables_figs/Table2.csv", row.names = FALSE)

#############################################################################################
# DIV OF PW SUBSAMPLES MATCHING OA BY NUMBER OF ARTICLES
###################################################################################################
# df of all articles in PW journals
FirstAuthPW<-AllData %>% 
  filter(Country != "NA" & Code != "NA") %>%
  filter(JrnlType=="PW") %>% 
  filter(AuthorNum==1)
FirstAuthPW$Journal<-droplevels(FirstAuthPW$Journal)

# df of all articles in OA journals
FirstAuthOA<-AllData %>% 
  filter(Country != "NA" & Code != "NA") %>%
  filter(JrnlType=="OA") %>% 
  filter(AuthorNum==1)
FirstAuthOA$Journal<-droplevels(FirstAuthOA$Journal)

# number of journals, make sure they are the same!
nlevels(FirstAuthPW$Journal)==nlevels(FirstAuthOA$Journal)

# How to sample different amounts from different groups is very nicely laid out in this post:
# https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html

# create a df telling you how many articles published in each OA journal
# will sample in this amount from the df of PW articles
OA_sample<-NumbArtOA %>% 
  select(pair_key,n)
OA_sample<-ungroup(OA_sample) 
OA_sample<-select(OA_sample,-JrnlType)
nlevels(OA_sample$pair_key)
OA_sample<-droplevels(OA_sample)
nlevels(OA_sample$pair_key)

PW_papers<-AllData %>%
  group_by(pair_key) %>% 
  filter(JrnlType=="PW") 
nlevels(PW_papers$pair_key)
levels(PW_papers$pair_key)
PW_papers<-droplevels(PW_papers)
nlevels(PW_papers$pair_key)

# STEP 1
# Are you sure all the journals in PW df are represented 
# in the OA df and vice versa?

# FALSES would indicate "no"
# levels(PW_papers$pair_key)==levels(OA_sample$pair_key)

# This will ensure the journals are in both (using intersection
# of pair_key in OA and PW)
OA_pk<-OA_sample$pair_key
PW_papers_pk<-PW_papers$pair_key
Ipk<-intersect(PW_papers_pk,OA_pk)
Ipk<-as_factor(Ipk)
OA_sample$pair_key
OA_sample<-OA_sample %>% filter(pair_key%in%Ipk)
OA_sample<-droplevels(OA_sample)

PW_papers<-semi_join(PW_papers,OA_sample,buy="pair_key") #this is what makes sure you are only sampling from PW journals for which there is OA
PW_papers$pair_key<-as.factor(PW_papers$pair_key)
nlevels(PW_papers$pair_key)
PW_papers<-droplevels(PW_papers)
nlevels(PW_papers$pair_key)
PW_papers<-arrange(PW_papers,pair_key)

# This is how many you are going to be sampling from each PW Journal.
# It is the number of articles n in each OA journal 
n = as.data.frame(OA_sample$n)
source("./Rscript/functions/samplePW.R")
PW_sample<-samplePW(PW_papers,n)

# To double check and see which might have been in OA sample but no PW papers
PW_sample_check<-PW_sample %>% 
  group_by(pair_key) %>% 
  summarize(sample_n=n())
PW_sample_check<-full_join(PW_sample_check,OA_sample, by="pair_key")
################

# STEP 1
# Are you sure all the journals in PW df are represented 
# in the OA df and vice versa?

# FALSES would indicate "no"
# levels(PW_papers$pair_key)==levels(OA_sample$pair_key)

# This will ensure the journals are in both (using intersection
# of pair_key in OA and PW)
OA_pk<-OA_sample$pair_key
PW_papers_pk<-PW_papers$pair_key
Ipk<-intersect(PW_papers_pk,OA_pk)
Ipk<-as_factor(Ipk)
OA_sample$pair_key
OA_sample<-OA_sample %>% filter(pair_key%in%Ipk)
OA_sample<-droplevels(OA_sample)

PW_papers<-semi_join(PW_papers,OA_sample,buy="pair_key") #this is what makes sure you are only sampling from PW journals for which there is OA
PW_papers$pair_key<-as.factor(PW_papers$pair_key)
nlevels(PW_papers$pair_key)
PW_papers<-droplevels(PW_papers)
nlevels(PW_papers$pair_key)
PW_papers<-arrange(PW_papers,pair_key)

# This is how many you are going to be sampling from each PW Journal.
# It is the number of articles n in each OA journal 
n = as.data.frame(OA_sample$n)
str(n)

############################################
# now sample and iterate!
# THIS IS FOR MATCHED SUBSAMPLING - FIRST AUTHOR
# 1<-Richness  2<-InvSimpsons 3<-Countries
############################################
source("./Rscript/functions/samplePW.R")
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
  results<-DivCalcPooledFirst(PW_sample)
  SubsampledPW.results_First[i,1]<-(results)[1]
  SubsampledPW.results_First[i,2]<-(results)[2]
  }
SubsampledPW.results_First
toc()

write.csv(SubsampledPW.results_First, 
          'output/SubsampledPW.results_FIRST_AUTHOR.csv', 
          row.names = FALSE)



probFirst<-sum(SubsampledPW.results_First$InvSimp>OAdiv)/1000
probFirst

png(file="./tables_figs/plot5a.png",width=1000, height=700)
OAdiv_First<-as.numeric(DivCalcPooledFirst(OA_papers)[2])
pDiv_first<-ggplot(SubsampledPW.results_First, aes(x=InvSimp)) + 
  geom_histogram(bins=25, colour="black", fill="white")+
  geom_vline(aes(xintercept=OAdiv_First),
               color="darkblue", linetype="dashed", size=1)+
  geom_label(label="0% bootstrap PW values >\nObserved OA Diversity", x=13,y=175,
             label.padding = unit(0.55, "lines"), # Rectangle size around label
             label.size = 0.5,color = "darkblue", fill="white")+
  xlab("Bootstrapped PW Diversity - First Authors") + ylab("Frequency")+
  scale_y_continuous(expand = c(0,0),limits = c(0,200))+
    scale_x_continuous(breaks = c(1:16),limits=c(5,16))
  pDiv_first<-pDiv_first+
  theme_classic()+ 
  theme(
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
    axis.title.y=element_text(colour="black", size = 24, hjust=0.5,),
    plot.margin =unit(c(1,1,1,1.5), "lines")  
  )
pDiv_first
dev.off()

############################################
# now sample and iterate!
# THIS IS FOR MATCHED SUBSAMPLING - LAST AUTHOR
# 1<-Richness  2<-InvSimpsons 3<-Countries
############################################
source("./Rscript/functions/samplePW.R")
library(tictoc)
tic()
nboot <-1000 #number of bootstrap samples
InvSimp <-rep(NA, nboot)
Richness<-rep(NA, nboot)
SubsampledPW.results_Last <- data.frame(Richness,InvSimp)
rm(InvSimp,Richness)
set.seed(10)
for(i in 1:nboot){
  PW_sample<-samplePW(PW_papers,n)
  results<-DivCalcPooledLast(PW_sample)
  SubsampledPW.results_Last[i,1]<-(results)[1]
  SubsampledPW.results_Last[i,2]<-(results)[2]
}
SubsampledPW.results_Last
toc()

write.csv(SubsampledPW.results_Last, 
          'output/SubsampledPW.results_LAST_AUTHOR.csv', 
          row.names = FALSE)



probLast<-sum(SubsampledPW.results_Last$InvSimp>OAdiv_Last)/1000
probLast


png(file="./tables_figs/plot5b.png",width=1000, height=700)
OAdiv_Last<-as.numeric(DivCalcPooledLast(OA_papers)[2])
pDiv_last<-ggplot(SubsampledPW.results_Last, aes(x=InvSimp)) + 
  geom_histogram(bins=25, colour="black", fill="white")+
  geom_vline(aes(xintercept=OAdiv_Last),
             color="darkblue", linetype="dashed", size=1)+
geom_label(label="96% bootstrap PW values >\nObserved OA Diversity",
    x=8.5,y=135,label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.5,color = "darkblue", fill="white")+
  xlab("Bootstrapped PW Diversity - Last Authors") + ylab("Frequency")+
  # scale_x_continuous(breaks = c(3:14),limits=c(3,14))+
  scale_y_continuous(expand = c(0,0),limits = c(0,150))
pDiv_last<-pDiv_last+
  theme_classic()+ 
  theme(
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
    axis.title.y=element_text(colour="black", size = 24, hjust=0.5),
    plot.margin =unit(c(1,1,1,1.5), "lines")  
  )
pDiv_last
dev.off()





############################################
# now sample and iterate!
# THIS IS FOR MATCHED SUBSAMPLING - LAST AUTHOR
# 1<-Richness  2<-InvSimpsons 3<-Countries
############################################
source("./Rscript/functions/samplePW.R")
library(tictoc)
tic()
nboot <-1000 #number of bootstrap samples
InvSimp <-rep(NA, nboot)
Richness<-rep(NA, nboot)
SubsampledPW.results_ALL <- data.frame(Richness,InvSimp)
rm(InvSimp,Richness)
set.seed(10)
for(i in 1:nboot){
  PW_sample<-samplePW(PW_papers,n)
  results<-DivCalcPooledAll(PW_sample)
  SubsampledPW.results_ALL[i,1]<-(results)[1]
  SubsampledPW.results_ALL[i,2]<-(results)[2]
}
SubsampledPW.results_ALL
toc()

write.csv(SubsampledPW.results_ALL, 
          'output/SubsampledPW.results_ALL_AUTHOR.csv', 
          row.names = FALSE)



probAll<-sum(SubsampledPW.results_ALL$InvSimp>OAdiv_All)/1000
probAll

png(file="./tables_figs/plot5c.png",width=1000, height=700)
OAdiv_All<-as.numeric(DivCalcPooledAll(OA_papers)[2])
pDiv_all<-ggplot(SubsampledPW.results_ALL, aes(x=InvSimp)) + 
  geom_histogram(bins=25, colour="black", fill="white")+
  geom_vline(aes(xintercept=OAdiv_All),
             color="darkblue", linetype="dashed", size=1)+
  geom_label(label="0% bootstrap PW values >\nObserved OA Diversity",
             x=11.5,y=135,label.padding = unit(0.55, "lines"), # Rectangle size around label
             label.size = 0.5,color = "darkblue", fill="white")+
  xlab("Bootstrapped PW Diversity - All Authors") + ylab("Frequency")+
  # scale_x_continuous(breaks = c(3:14),limits=c(3,14))+
  scale_y_continuous(expand = c(0,0),limits = c(0,150))
  pDiv_all<-pDiv_all+
  theme_classic()+ 
  theme(
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
    axis.title.y=element_text(colour="black", size = 24, hjust=0.5),
    plot.margin =unit(c(1,1,1,1.5), "lines")  
  )
pDiv_all
dev.off()




############################################

############################################
# Richness OA vs Boot PW
# FIRST Authors
############################################
# SubsampledPW.results_First<-read_csv('./output/SubsampledPW.results_FIRST_AUTHOR.csv')

probFirst<-sum(SubsampledPW.results_First$Richness>OArich_First)/1000
probFirst

png(file="./tables_figs/plot6a.png",width=1000, height=700)
OArich_First<-as.numeric(DivCalcPooledFirst(OA_papers)[1])
prich_first<-ggplot(SubsampledPW.results_First, aes(x=Richness)) +
  geom_histogram(bins=25, colour="black", fill="white")+
  geom_vline(aes(xintercept=OArich_First),
             color="darkblue", linetype="dashed", size=1)+
  geom_label(label="0% bootstrap PW values >\nObserved OA Richness", x=65,y=240,
             label.padding = unit(0.55, "lines"), # Rectangle size around label
             label.size = 0.5,color = "darkblue", fill="white")+
  xlab("Bootstrapped PW Richness - First Authors") + ylab("Frequency")+
  scale_y_continuous(expand = c(0,0),limits = c(0,250))+
  scale_x_continuous(breaks = seq(30,70, by=10),limits=c(30,70))
prich_first<-prich_first+
  theme_classic()+ 
  theme(
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
    axis.title.y=element_text(colour="black", size = 24, hjust=0.5,),
    plot.margin =unit(c(1,1,1,1.5), "lines")  
  )
prich_first
dev.off()




############################################
# Richness OA vs Boot PW
# LAST Authors
############################################
# SubsampledPW.results_Last<-read_csv('./output/SubsampledPW.results_LAST_AUTHOR.csv')

problast<-sum(SubsampledPW.results_Last$Richness>OArich_last)/1000
problast


png(file="./tables_figs/plot6b.png",width=1000, height=700)
OArich_last<-as.numeric(DivCalcPooledLast(OA_papers)[1])
prich_last<-ggplot(SubsampledPW.results_Last, aes(x=Richness)) +
  geom_histogram(bins=25, colour="black", fill="white")+
  geom_vline(aes(xintercept=OArich_last),
             color="darkblue", linetype="dashed", size=1)+
  geom_label(label="99.9% bootstrap PW values >\nObserved OA Richness", x=58,y=230,
             label.padding = unit(0.55, "lines"), # Rectangle size around label
             label.size = 0.5,color = "darkblue", fill="white")+
  xlab("Bootstrapped PW Richness - Last Authors") + ylab("Frequency")+
  scale_y_continuous(expand = c(0,0),limits = c(0,250))+
  scale_x_continuous(breaks = seq(50,80, by=10),limits=c(50,80))
prich_last<-prich_last+
  theme_classic()+ 
  theme(
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
    axis.title.y=element_text(colour="black", size = 24, hjust=0.5,),
    plot.margin =unit(c(1,1,1,1.5), "lines")  
  )
prich_last
dev.off()




############################################
# Richness OA vs Boot PW
# ALL Authors
############################################
# SubsampledPW.results_All<-read_csv('./output/SubsampledPW.results_ALL_AUTHOR.csv')
probAll<-sum(SubsampledPW.results_All$Richness>OArich_All)/1000
probAll


png(file="./tables_figs/plot6c.png",width=1000, height=700)
OArich_All<-as.numeric(DivCalcPooledAll(OA_papers)[1])
prich_All<-ggplot(SubsampledPW.results_All, aes(x=Richness)) +
  geom_histogram(bins=25, colour="black", fill="white")+
  geom_vline(aes(xintercept=OArich_All),
             color="darkblue", linetype="dashed", size=1)+
  geom_label(label="0% bootstrap PW values >\nObserved OA Richness", x=85,y=230,
             label.padding = unit(0.55, "lines"), # Rectangle size around label
             label.size = 0.5,color = "darkblue", fill="white")+
  xlab("Bootstrapped PW Richness - All Authors") + ylab("Frequency")+
  scale_y_continuous(expand = c(0,0),limits = c(0,250))+
  scale_x_continuous(breaks = seq(50,90, by=10),limits=c(50,90))
prich_All<-prich_All+
  theme_classic()+ 
  theme(
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
    axis.title.y=element_text(colour="black", size = 24, hjust=0.5,),
    plot.margin =unit(c(1,1,1,1.5), "lines")  
  )
prich_All
dev.off()



###############################################
# how often is first author and last author from same country?  region?


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

summary(FirstLast$Region_first==FirstLast$Region_last)
19298/(19298+6244)

summary(FirstLast$IncomeGroup_first==FirstLast$IncomeGroup_last)
20298/(20298+5244)

summary(FirstLast$Country_first==FirstLast$Country_last)
16473/(16473+9069)


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







# now sample and iterate!
# THIS IS FOR NO SUBSAMPLING - FIRST AUTHOR, ALL ARTICLES POOLED
# 1<-Richness  2<-InvSimpsons 3<-Countries
############################################
# 
# library(tictoc)
# tic()
# nboot <-1000 #number of bootstrap samples
# InvSimp <-rep(NA, nboot)
# Richness<-rep(NA, nboot)
# SubsampledPW.pooled.results <- data.frame(Richness,InvSimp)
# rm(InvSimp,Richness)
# set.seed(10)
# for(i in 1:nboot){
#   PW_sample<-samplePW(PW_papers,nrow(OA_papers))
#   results<-DivCalcPooledFirst(PW_sample)
#   SubsampledPW.pooled.results[i,1]<-(results)[1]
#   SubsampledPW.pooled.results[i,2]<-(results)[2]
# }
# SubsampledPW.pooled.results
# toc()
# head(SubsampledPW.pooled.results) 
# save_name<-paste('output/SubsampledPW.pooled.results_', Sys.Date(), '.csv') #to add the date of output to filename
# write.csv(SubsampledPW.pooled.results, save_name, row.names = FALSE)
# rm(save_name)
# # SubsampledPW.pooled.results<-read_csv("CleanData/SubsampledPW.pooled.results.csv")
# source("./Rscript/functions/DivCalcPooledFirst.R") 
# OAdiv<-as.numeric(DivCalcPooledFirst(OA_papers)[2])
# PWdiv<-as.numeric(DivCalcPooledFirst(PW_papers)[2])
# # SubsampledPW.pooled.results<-read.csv("CleanData/SubsampledPW.pooled.results.csv")
# 
# pDiv<-ggplot(SubsampledPW.pooled.results, aes(x=InvSimp)) + 
#   geom_histogram(bins=25, colour="black", fill="white")+
#   geom_vline(aes(xintercept=OAdiv),
#              color="blue", linetype="dashed", size=1)+
#   geom_label(label="OA InvSimp", x=13.5,y=170,
#              # label.padding = unit(0.55, "lines"), # Rectangle size around label
#     label.size = 0.15,color = "blue")+
#   geom_vline(aes(xintercept=PWdiv),
#            color="red", linetype="dashed", size=1)+
#   geom_label(label="PW InvSimp", x=6,y=150,
#              # label.padding = unit(0.55, "lines"), # Rectangle size around label
#              label.size = 0.15,color = "red")
# pDiv<-pDiv+theme_classic() 
# pDiv
# 
# dev.off()
















# #############
# #THE FOR LOOP TO MAKE FIRST AUTHOR RICHNESS AND DIVERSITY OF COUNTRY 
# #ITTERATIONS
# #
# #BEWARE... THIS LOOP TOOK 21 MINS TO RUN ON MY COMPUTER!!!!
# #
# #############
# 
# #For Loop to calculate over all simpsons diveristy (true diversity)
# #using similar randomly sampled chunks of the over all pool
# # USING FIRST AUTHOR
# SimpsDiversitySubs <- matrix(nrow = 1, ncol = 1000) #empty matrix for diversity
# 
# for (i in 1:1000){
#   NumbArtOA2 <- NumbArtOA[-1]  
#   SamplePW3 <- FirstAuthPW %>% 
#     filter(Country != "NA" & Journal != "NA" & JrnlType != "NA") %>% #remove any article that has no country listed
#     nest(data = c(Code, DOI, Year, AuthorNum, Country, JrnlType, Region, IncomeGroup)) %>% 
#     left_join(NumbArtOA2, by = "Journal") %>%
#     mutate(Sample = map2(data, n, sample_n)) %>% 
#     unnest(Sample)%>%
#     select(Code, DOI, Journal, Year, AuthorNum, Country, JrnlType, Region,
#            IncomeGroup)
#   
#   SiteBySpecPW2 <- SamplePW3 %>%
#     group_by(Country)%>%
#     tally()
#   
#   SiteBySpecPW2 <- SiteBySpecPW2 %>%
#     spread(Country, n)
#   
#   PWDiversitySub <- diversity(SiteBySpecPW2, index = "invsimpson")
#   SimpsDiversitySubs[,i] = PWDiversitySub
#   
# }
# 
# SimpsDivSubsPW <- as.data.frame(SimpsDiversitySubs)
# write.csv(SimpsDivSubsPW, "CleanData/ProporSampsSimpsDivPW.csv", row.names = FALSE)
# 
# 
# #For Loop to calculate over all simpsons diveristy (true diversity)
# #using similar randomly sampled chunks of the over all pool
# # USING LAST AUTHOR
# SimpsDivSubsLast <- matrix(nrow = 1, ncol = 1000) #empty matrix for diversity
# 
# for (i in 1:1000){
#   NumbArtOA2 <- NumbArtOA[-1]  
#   SamplePW3 <- LastAuthPW %>% 
#     filter(Country != "NA" & Journal != "NA" & JrnlType != "NA") %>% #remove any article that has no country listed
#     nest(data = c(Code, DOI, Year, AuthorNum, Country, JrnlType, Region, IncomeGroup)) %>% 
#     left_join(NumbArtOA2, by = "Journal") %>%
#     mutate(Sample = map2(data, n, sample_n)) %>% 
#     unnest(Sample)%>%
#     select(Code, DOI, Journal, Year, AuthorNum, Country, JrnlType, Region,
#            IncomeGroup)
#   
#   SiteBySpecPW2 <- SamplePW3 %>%
#     group_by(Country)%>%
#     tally()
#   
#   SiteBySpecPW2 <- SiteBySpecPW2 %>%
#     spread(Country, n)
#   
#   PWDiversitySub <- diversity(SiteBySpecPW2, index = "invsimpson")
#   SimpsDivSubsLast[,i] = PWDiversitySub
#   
# }
# 
# SimpsDivSubsPW <- as.data.frame(SimpsDivSubsLast)
# write.csv(SimpsDivSubsPW, "CleanData/ProporSampsSimpsDivPWLast.csv", row.names = FALSE)
# 
# #For Loop to calculate over all simpsons diveristy (true diversity)
# #using similar randomly sampled chunks of the over all pool
# # USING ALL AUTHORs
# SimpsDivSubsAllAuths <- matrix(nrow = 1, ncol = 1000) #empty matrix for diversity
# NumbAuthsOA <- OpenAccessAll %>%
#   group_by(Journal) %>%
#   tally()
# 
# for (i in 1:1000){
#   SamplePW3 <- PayWallAll %>% 
#     filter(Country != "NA" & Journal != "NA" & JrnlType != "NA") %>% #remove any article that has no country listed
#     nest(data = c(Code, DOI, Year, AuthorNum, Country, JrnlType, Region, IncomeGroup)) %>% 
#     left_join(NumbAuthsOA, by = "Journal") %>%
#     mutate(Sample = map2(data, n, sample_n)) %>% 
#     unnest(Sample)%>%
#     select(Code, DOI, Journal, Year, AuthorNum, Country, JrnlType, Region,
#            IncomeGroup)
#   
#   SiteBySpecPW2 <- SamplePW3 %>%
#     group_by(Country)%>%
#     tally()
#   
#   SiteBySpecPW2 <- SiteBySpecPW2 %>%
#     spread(Country, n)
#   
#   PWDiversitySub <- diversity(SiteBySpecPW2, index = "invsimpson")
#   SimpsDivSubsAllAuths[,i] = PWDiversitySub
#   
# }
# 
# SimpsDivSubsPW <- as.data.frame(SimpsDivSubsAllAuths)
# write.csv(SimpsDivSubsPW, "CleanData/SimpsDivSubsAllAuthsPW.csv", row.names = FALSE)
# 
# 
# 
# 
# 
# 
# 
# 
# #FOR LOOP FOR making ricness and diversity matrices by journal with sampling
# ?diversity
# # MAKE EMPTY MATRICES FOR THE FOR LOOP TO FILL
# richness <- matrix(nrow = 62, ncol = 1000) #empty matrix for richness
# SimpsonDiversity <- matrix(nrow = 62, ncol = 1000) #empty matrix for diversity
# 
# for (i in 1:1000){ #do loop 100 times
#   NumbArtOA2 <- NumbArtOA[-1]   # remove "JnrlType" column from the NumbartOA dataframe
#   
#   #SUBSET
#   #this subsets PW journals (FirstauthPW df) by the number of Aricles per journal in OA sources (the numbartoa df). can change this to lastauth as well
#   SamplePW3 <- FirstAuthPW %>% 
#     filter(Country != "NA" & Journal != "NA" & JrnlType != "NA") %>% #remove any article that has no country listed
#     nest(data = c(Code, DOI, Year, AuthorNum, Country, JrnlType, Region, IncomeGroup)) %>% 
#     left_join(NumbArtOA2, by = "Journal") %>%
#     mutate(Sample = map2(data, n, sample_n)) %>% 
#     unnest(Sample)%>%
#     select(Code, DOI, Journal, Year, AuthorNum, Country, JrnlType, Region,
#            IncomeGroup)
#   #ADD OA DATA
#   FirstAuthAll <- rbind(SamplePW3, FirstAuthOA) #put randomly sampled PW articles into 
#   #the same data frame wiht our OA articles
#   FirstAuthAll$JournalAndType <- paste(FirstAuthAll$Journal, FirstAuthAll$JrnlType)
#   #TURN IT INTO SITE BY SPECIES (JRNL BY COUNTRY)
#   SiteBySpec <- FirstAuthAll %>%
#     group_by(JournalAndType, Country)%>%
#     tally()
#   
#   SiteBySpec <- cast(SiteBySpec, JournalAndType ~ Country, value = 'n')
#   SiteBySpec[is.na(SiteBySpec)] <- 0
#   Countries <- names(SiteBySpec[,2:(ncol(SiteBySpec)-1)]) #check this to be sure this 
#   
#   ####
#   #Add diversity metrics
#   country_counts <- SiteBySpec[,Countries] #final site by species matrix
#   row.names(country_counts) <- SiteBySpec$JournalAndType #add rownames for journals
#   
#   #simpson diversity index
#   DivSimpson <-diversity(country_counts, index = "simpson")
#   
#   #richness
#   rich <- rowSums(country_counts > 0)
#   
#   richness[,i] = rich
#   SimpsonDiversity[,i] = DivSimpson
# }
# 
# ###################################################
# 
# #now manipulate the 62 x 1000 matrix of itterations
# #richness
# 
# richness <- as.data.frame(richness) #make it a data frame
# richness$MeanRich <- rowMeans(richness) #add a mean column from the 1000 itterations
# write.csv(richness, "CleanData/FirstRich1000itters.csv", row.names = TRUE)
# 
# FirstAuthRich <- richness %>% #just grab the mean column
#   select(MeanRich)
# FirstAuthRich$JournalAndType <- SiteBySpec$JournalAndType #add journals in
# #now for diversity
# FirstAuthSimpDiv <- as.data.frame(SimpsonDiversity) 
#   FirstAuthSimpDiv$MeanDiveristy <- rowMeans(FirstAuthSimpDiv)
# write.csv(FirstAuthDiv, "CleanData/FirstDiv1000itter.csv", row.names = TRUE)
#   FirstAuthSimpDiv <- FirstAuthSimpDiv %>%
#   select(MeanDiveristy)
# FirstAuthSimpDiv$JournalAndType <- SiteBySpec$JournalAndType
# 
# #make a richness and diversity data frame for each journal
# FirstAuthDiv <- merge(FirstAuthSimpDiv, FirstAuthRich, by = "JournalAndType") 
# #re-grab journal types to paste onto the diersity dataframe
# AllData$JournalAndType <- paste(AllData$Journal, AllData$JrnlType) 
# JournalTypes <- AllData %>%
#   select(JournalAndType, JrnlType)%>%
#   distinct()
# 
# FirstAuthDiv <- merge(FirstAuthDiv, JournalTypes, by = "JournalAndType") #merge for final first author dataframe
# write.csv(FirstAuthDiv, "CleanData/FirstAuthDiv.csv", row.names = FALSE)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ################
# #CountryRichness OVERALL
# ################
# 
# # total country richness between the two journal types for FIRST authors
# # this does not control for sample size
# TotalRichFirst <- FirstAuth %>% 
#   group_by(JrnlType) %>%
#   summarise(Rich = n_distinct(Country))
# # same as above for last authors
# TotalRichLast <- LastAuth %>% 
#   group_by(JrnlType) %>%
#   summarise(Rich = n_distinct(Country))
# 
# 
# RichnessOA <- FirstAuthOA %>%
#   summarise(Rich = n_distinct(Country))
# 
# RichnessPW <- SamplePW2 %>%
#   group_by(JrnlType) %>%
#   summarise(Rich = n_distinct(Country))
# 
# 
# 
# 
# 
# 
# ##################################################################################################
# #############################################################################################
# #OLD CODE FOR OTHER SAMPLING TECHNIQUES> NO LONGER NEEDED!
# 
# #this is a way to randomly sample from each Paywall journal to 
# #match the number of articles in its OA mirror journal --CKG
# 
# FirstAuthPWAlphab<-FirstAuthPW[order(FirstAuthPW$Journal),]#sort the paywall article df alphabetically by journal title
# 
# SamplePW1 <- strata(FirstAuthPWAlphab, "Journal", 
#                     size = c(14,6,47,9,29,21,9,5,10,10,9,31,16,32,6,8,6,30,28,32,9,17,32,2,8,1,14,33,14,18,36),
#                     method = "srswor")
# sum(NumbArtOA$n) #using this to double check the sample size
# 
# summary(c(14,6,47,9,29,21,9,5,10,10,9,31,16,32,6,8,6,30,28,32,9,17,32,2,8,1,14,33,14,18,36))
# 
# # the code above!
# # for each strata of journal, I took the number of articles available in the OA mirror 
# # journals, and randomly sampled articles in PW journals based on those numbers. 
# # I got the number of articles per OA journal from the NumbArtOA dataframe and just typed theminto the 'size' argument.
# # Two issues: 1.) the sum of articles in NumbArtOA is 542, but the length of the FirstAuthOA dataframe is 553
# # There are 11 articles in NumbArtOA that are 2018 articles, not 2019. I don't know why they weren't excluded in lines 47 and 52.
# # specifically, they are all water research articles in theFirstAuthOA df. (lines 37, 57, 81, 116, 242, 293, 383, 392, 497, 510, 515)
# # 2.) sleep medicine and research policy have 1 and 2 articles in those journals, respectively, according to NumbArtOA. I feel like we should delete them, 
# # as we probably can't get a good diversity estimate from such a small number of articles.
# 
# SamplePW2 <- FirstAuthPW %>% #subset the paywall journals First Author Data 
#   #filter(DOI != "NA") %>%
#   group_by(Journal)%>%
#   sample_n(c(14,6,47,9,29,21,9,5,10,10,9,31,16,32,6,8,6,30,28,32,9,17,32,2,8,1,14,33,14,18,36)) # this code only grabs 30 from each journal, which is a random number SIMILAR to the 
# #numbers of articles in the open access journals
# # Number of articles to pull should be from NumbArtOA dataframe
# SamplePW4 <- FirstAuthOA %>% #subset the paywall journals First Author Data 
#   #filter(DOI != "NA") %>%
#   group_by(Journal)%>%
#   sample_n(17, replace = TRUE) # this code only grabs 30 from each journal, which is a random number SIMILAR to the 





