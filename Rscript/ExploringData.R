####################################################
# This script is to start to clean and explore the data 
# ultimately I think we need to get to a site by species type matrix in which 
# each site is a journal and the sampling effort (number of articles) is the 
# same for each mirror/sister journals in a site by species matrix, country 
# is the species, income or region is like taxon or functional group maybe?
####################################################

# libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sampling)
library(reshape)
library(vegan)
library(purrr)

# load journal list and pairs
MirrorPairs<-read_csv(file="./data/MirrorPairs.csv")
MirrorPairs
MirrorPairs <- MirrorPairs%>%
  select(Journal = SO, JrnlType = journal_cat, pair_key)

# data
# load(file="./output/ALLDATA.RData")
ALLDATA<-read_csv(file="./output/all_Journal_author_countries.csv")
ALLDATA$X1<-NULL
head(ALLDATA,20)
str(ALLDATA)
CountryData <- read.csv("data/CLASS.csv", header = TRUE)
CountryData <- CountryData[-1,]
summary(CountryData)
head(ALLDATA)

AllData <- ALLDATA %>%
  select(DOI = DI, Journal = SO, Year = PY, AuthorNum = author,
         Country = country, Code = country_code, JrnlType = journal_cat, pair_key)

head(AllData)
head(MirrorPairs)
CountryData <- CountryData %>%
  select(Code,Region, IncomeGroup = Income.group)

#this code merges the country data to include income category and geo region
AllData <- merge(AllData, CountryData, by="Code", all.x=TRUE) # merge 

head(AllData)

AllData$IncomeGroup<- ordered(AllData$IncomeGroup, levels = c("Low income", "Lower middle income", "Upper middle income","High income"))
levels(AllData$IncomeGroup)[levels(AllData$IncomeGroup)=="High income"] <- "High"
levels(AllData$IncomeGroup)[levels(AllData$IncomeGroup)=="Low income"] <- "Low"
levels(AllData$IncomeGroup)[levels(AllData$IncomeGroup)=="Lower middle income"] <- "Lower middle"
levels(AllData$IncomeGroup)[levels(AllData$IncomeGroup)=="Upper middle income"] <- "Upper middle"
#
# #scrap the "x" in the journal names, as this data is included in the JrnlType column
# AllData <- AllData %>%
#   separate(Journal, c("Journal", NA), ":") %>%
#   separate(Journal, c("Journal", NA), " x") %>%
#   separate(Journal, c("Journal", NA), " open")%>%
#   filter(Journal != "biochimie") %>%
#   filter(Year != 2018)
# deleted this because not all mirror journals have
# the same name as the original journal
# In addition, the data harvesting was only done for 
# the years of publishing of the mror journal so no 
# need to filter by yer any more

#small revison. replace '&' with 'and'
# AllData[AllData$Journal=="biosensors & bioelectronics", "Journal"] <- "biosensors and bioelectronics"


list(AllData$Journal)

# ADD IN THE JOURNAL PAIR_KEY


AllData<-left_join(AllData,MirrorPairs,by="Journal")
colnames(AllData)
summary(AllData$JrnlType.x==AllData$JrnlType.y)
summary(AllData$pair_key.x==AllData$pair_key.y)
AllData<-AllData %>%
  dplyr::select(-JrnlType.y,-pair_key.y) %>% 
  dplyr::rename(JrnlType=JrnlType.x,pair_key=pair_key.x)

AllData$JrnlType<-as.factor(AllData$JrnlType)
AllData$Code<-as.factor(AllData$Code)
AllData$DOI<-as.factor(AllData$DOI)
AllData$Journal<-as.factor(AllData$Journal)
AllData$Country<-as.factor(AllData$Country)
AllData$pair_key<-as.factor(AllData$pair_key)
AllData$pair_key<-droplevels(AllData$pair_key)
str(AllData)

levels(AllData$pair_key)

# There are some with missing DOI values, if you don't replace these
# They will be excluded from the grouping
AllData$DOI<- as.character(AllData$DOI)
AllData$DOI<- AllData$DOI %>% replace_na("missing_DOI")
AllData$DOI<- as.factor(AllData$DOI)


############################################################
############################################################
first_author_income_cats<-AllData %>% 
  filter(AuthorNum==1) %>% 
  group_by(JrnlType,IncomeGroup) %>% 
  tally() %>% 
  mutate(percentage=n/sum(n)*100)

# IncomeCats_allJournals<-bind_rows(OAAllInc,PayWallAllGeoInc)
# bar_order <- c("High", "Upper-middle", "Lower-middle","Low")
# levels(IncomeCats_allJournals$IncomeGroup)
plot1<-ggplot(first_author_income_cats, aes(x=IncomeGroup,y = percentage))+
  geom_bar(stat = "identity")+
  # scale_x_discrete(limits = bar_order)+
  facet_grid(cols = vars(JrnlType))+
  ggtitle("% of articles in each journal category by 1st authors from diff. income cats.")
plot1<-plot1+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
plot1
############################################################
############################################################
# Here group by countries first. The idea is to compare
# where authors from each income class publish
first_jrnlcat_by_income_cats<-AllData %>% 
  filter(AuthorNum==1) %>% 
  group_by(IncomeGroup,JrnlType) %>% 
  tally() %>% 
  mutate(percentage=n/sum(n)*100)

plot2<-ggplot(first_jrnlcat_by_income_cats, aes(x=IncomeGroup,y = percentage, fill=JrnlType))+
  geom_bar(position="fill",stat = "identity")+
  scale_fill_manual(values=c("midnightblue","dimgray"))+
  coord_flip()+
  ggtitle("% of articles by authors from different income classes in OA and PW journals")
plot2<-plot2+
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 1, 0.10))
plot2

############################################################
############################################################


############################################################
############################################################
OAAllGeo <-AllData %>%
  filter(AuthorNum == 1) %>%
  filter(JrnlType=="OA") %>% 
  filter(Country != "NA" & Code != "NA") %>%
  group_by(Country, Code)%>%
  tally() %>% 
  arrange(desc(n))
OAAllGeo <-left_join(OAAllGeo, CountryData, by = "Code") %>%
  filter(IncomeGroup != "NA") %>% 
  ungroup() %>%
  mutate(perc=n/sum(n)*100)
OAAllGeo$perc<-round(OAAllGeo$perc,2)
OAAllGeo<-arrange(OAAllGeo,desc(OAAllGeo$perc))
OAAllGeo<- transform(OAAllGeo,Code = reorder(Code,perc))

plot3<-ggplot(OAAllGeo, aes(Code, perc, fill=IncomeGroup))+
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("Country Representation by Income Group OA")
plot3<-plot3+theme_light()+
  scale_y_continuous(breaks = seq(0, 100, 10))
plot3  
############################################################
############################################################

############################################################
############################################################
PWAllGeo <-AllData %>%
  filter(AuthorNum == 1) %>%
  filter(JrnlType=="PW") %>% 
  filter(Country != "NA" & Code != "NA") %>%
  group_by(Country, Code)%>%
  tally() %>% 
  arrange(desc(n))
PWAllGeo <-left_join(PWAllGeo, CountryData, by = "Code") %>%
  filter(IncomeGroup != "NA") %>% 
  ungroup() %>%
  mutate(perc=n/sum(n)*100)
PWAllGeo$perc<-round(PWAllGeo$perc,2)
PWAllGeo<-arrange(PWAllGeo,desc(PWAllGeo$perc))
PWAllGeo<- transform(PWAllGeo,Code = reorder(Code,perc))

plot4<-ggplot(PWAllGeo, aes(Code, perc, fill=IncomeGroup))+
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("Country Representation by Income Group OA")
plot4<-plot4+theme_light()+
  scale_y_continuous(breaks = seq(0, 100, 10))
plot4
############################################################
############################################################


NumbArticles <- AllData %>% #number of papers per journal
  group_by(pair_key,JrnlType,Journal) %>% 
  summarize(n=n_distinct(DOI))%>% 
  arrange(Journal)
NumbArticles

############################################################
############################################################


NumbArtOA <- NumbArticles %>% #number of articles that are Open Access
  filter(JrnlType == "OA")
NumbArtOA

TOTAL_NumbArtOA<-sum(NumbArtOA$n) #total number summed accross all OA journals
TOTAL_NumbArtOA

############################################################
############################################################

NumbArtPW <- NumbArticles %>% #number of articles that are paywall
  filter(JrnlType == "PW")
NumbArtPW

TOTAL_NumbArtPW<-sum(NumbArtPW$n) #total number summed accross all OA journals
TOTAL_NumbArtPW

############################################################
############################################################


NumbAuthors <- AllData %>% # average number of authors per journal 
  # filter(Year==2019) %>% 
  group_by(JrnlType,Journal,pair_key,DOI) %>% 
  arrange(JrnlType, Journal) %>% 
  filter(AuthorNum == max(AuthorNum)) %>% 
  group_by(pair_key,JrnlType,Journal) %>% 
  summarize(avg_n=mean(AuthorNum),sd_n=sd(AuthorNum)) %>% 
  arrange(Journal)
NumbAuthors

############################################################
############################################################


##################################################
#SUBSET and bootstrap Paywall Journals by the number found in Open Access Journals
##################################################
#TODO: convert this to a function, no need to do the same thing for both of them

#################################################################
# diversity indices for OA JOURNALS by each JOURNAL 1st AUTHORS
#################################################################
SiteBySpec1<-AllData %>% 
  filter(Country != "NA" & Code != "NA") %>%
  filter(JrnlType=="OA") %>% 
  filter(AuthorNum==1) %>%
  group_by(Journal, Country)%>%
  tally()

SiteBySpec1 <- cast(SiteBySpec1, Journal ~ Country, value = 'n')
SiteBySpec1[is.na(SiteBySpec1)] <- 0
ncols_SiteBySpec1<-ncol(SiteBySpec1)
Countries <- names(SiteBySpec1[,2:ncols_SiteBySpec1]) #check this to be sure this 

####
#Add diversity metrics
country_counts <- SiteBySpec1[,Countries] #final site by species matrix
row.names(country_counts) <- SiteBySpec1$Journal #add rownames for journals

#simpson diversity index
DivSimpson <-diversity(country_counts, index = "simpson")

#abundance
abund <-rowSums(country_counts)

#richness
rich <- rowSums(country_counts > 0)

DivMetricsOA <- as.data.frame(cbind(rich, abund, DivSimpson)) 
DivMetricsOA$Journal <- SiteBySpec1$Journal
DivMetricsOA$EffectSpecNum <- 1/(1-DivMetricsOA$DivSimpson)

#################################################################
# diversity indices for PW JOURNALS by each JOURNAL 1st AUTHORS
#################################################################
SiteBySpec2<-AllData %>% 
  filter(Country != "NA" & Code != "NA") %>%
  filter(JrnlType=="PW") %>% 
  filter(AuthorNum==1) %>%
  group_by(Journal, Country)%>%
  tally()

SiteBySpec2 <- cast(SiteBySpec2, Journal ~ Country, value = 'n')
SiteBySpec2[is.na(SiteBySpec2)] <- 0
ncols_SiteBySpec2<-ncol(SiteBySpec2)
Countries<- names(SiteBySpec2[,2:ncols_SiteBySpec2]) #check this to be sure this 

####
#Add diversity metrics
country_counts <- SiteBySpec2[,Countries] #final site by species matrix
row.names(country_counts) <- SiteBySpec2$Journal #add rownames for journals

#simpson diversity index
DivSimpson <-diversity(country_counts, index = "simpson")

#abundance
abund <-rowSums(country_counts)

#richness
rich <- rowSums(country_counts > 0)

DivMetricsPW <- as.data.frame(cbind(rich, abund, DivSimpson))
DivMetricsPW$Journal <- SiteBySpec2$Journal
DivMetricsPW$EffectSpecNum <- 1/(1-DivMetricsPW$DivSimpson)

#TODO: "journal was the key back when the X was removed from the name. 
# now need to do by the "pair key" that will ID pairs of journals (since not)
# all of them are simply name/name x

MirrorPairs

DivMetricsOA<-inner_join(DivMetricsOA,MirrorPairs,by="Journal")
DivMetricsPW<-inner_join(DivMetricsPW,MirrorPairs,by="Journal")
DivMetrics<-bind_rows(DivMetricsOA,DivMetricsPW) %>% 
  arrange(pair_key)



###########
# TODO: need to reshape to calcl the diffs and do the box plot
DivMetricsALL$DeltaDiv <- DivMetricsALL$EffectSpecNumPW - DivMetricsALL$EffectSpecNumOA

# DivMetrics$DeltaDiv <- DivMetricsPW$EffectSpecNumPW - DivMetricsOA$EffectSpecNumOA

boxplot(DivMetricsALL$DeltaDiv)
median(DivMetricsALL$DeltaDiv)

write.csv(DivMetrics, "CleanData/FirstDivMetricsByJrnl.csv", row.names = FALSE)

###############
#DIVERSITY AND RICHNESS USING ENTIRE POOL FROM EACH JRNL TYPE
#################
#Calculate the diversity indices for ENTIRE OA COMMUNITY OF PAPERS
# First Authors
SiteBySpecOA <- FirstAuthOA %>%
  group_by(Country)%>%
  tally()
SiteBySpecOA <- SiteBySpecOA %>%
  spread(Country, n)
OADiversity <- diversity(SiteBySpecOA, index = "invsimpson")
OARichness <- length(SiteBySpecOA)

#DIversity for the entire Community of PW papers
SiteBySpecPW <- FirstAuthPW %>%
  filter(Country != "NA") %>%
  group_by(Country) %>%
  tally()

SiteBySpecPW <- SiteBySpecPW %>%
  spread(Country, n)
PWDiversity <- diversity(SiteBySpecPW, index = "invsimpson")
PWRichness <- length(SiteBySpecPW)

DivMetricsFullPools <- as.data.frame(cbind(OADiversity, OARichness, PWDiversity, PWRichness))
write.csv(DivMetricsFullPools, "CleanData/DivMetricsFullPools.csv", row.names = FALSE)
# Calculate the diversity indices for ENTIRE OA COMMUNITY OF PAPERS 
# LAST AUTHOR

SiteBySpecOA <- LastAuthOA %>%
  group_by(Country)%>%
  tally()
SiteBySpecOA <- SiteBySpecOA %>%
  spread(Country, n)
OADiversity <- diversity(SiteBySpecOA, index = "invsimpson")
OARichness <- length(SiteBySpecOA)

#DIversity for the entire Community of PW papers
SiteBySpecPW <- LastAuthPW %>%
  filter(Country != "NA")
  
SiteBySpecPW <- SiteBySpecPW %>%
  group_by(Country) %>%
  tally()

SiteBySpecPW <- SiteBySpecPW %>%
  spread(Country, n)
PWDiversity <- diversity(SiteBySpecPW, index = "invsimpson")
PWRichness <- length(SiteBySpecPW)

DivMetricsFullPoolsLast <- as.data.frame(cbind(OADiversity, OARichness, PWDiversity, PWRichness))
write.csv(DivMetricsFullPoolsLast, "CleanData/DivMetricsFullPoolLast.csv", row.names = FALSE)

#Calculate the diversity indices for ENTIRE OA COMMUNITY OF PAPERS
# ALL AUTHORS
SiteBySpecOA <- OpenAccessAll %>%
  filter(Country != "NA") %>%
  group_by(Country)%>%
  tally()
SiteBySpecOA <- SiteBySpecOA %>%
  spread(Country, n)
OADiversity <- diversity(SiteBySpecOA, index = "invsimpson")
OARichness <- length(SiteBySpecOA)

#DIversity for the entire Community of PW papers
SiteBySpecPW <- PayWallAll %>%
  filter(Country != "NA") %>%
  group_by(Country) %>%
  tally()

SiteBySpecPW <- SiteBySpecPW %>%
  spread(Country, n)
PWDiversity <- diversity(SiteBySpecPW, index = "invsimpson")
PWRichness <- length(SiteBySpecPW)

DivMetricsAllAuthors <- as.data.frame(cbind(OADiversity, OARichness, PWDiversity, PWRichness))
write.csv(DivMetricsAllAuthors, "CleanData/DivMetricsAllAuthors.csv", row.names = FALSE)
#sum(abund)# double check we have the same number of articles still
#sum(SiteBySpec) #check against this


#############################################################################################
# FOR LOOP
###################################################################################################
n_distinct(FirstAuthPW$Journal) #number of journals in PW
n_distinct(FirstAuthOA$Journal) #number of jourals in OA, they are the same!


#############
#THE FOR LOOP TO MAKE FIRST AUTHOR RICHNESS AND DIVERSITY OF COUNTRY 
#ITTERATIONS
#
#BEWARE... THIS LOOP TOOK 21 MINS TO RUN ON MY COMPUTER!!!!
#
#############

#For Loop to calculate over all simpsons diveristy (true diversity)
#using similar randomly sampled chunks of the over all pool
# USING FIRST AUTHOR
SimpsDiversitySubs <- matrix(nrow = 1, ncol = 1000) #empty matrix for diversity

for (i in 1:1000){
  NumbArtOA2 <- NumbArtOA[-1]  
  SamplePW3 <- FirstAuthPW %>% 
    filter(Country != "NA" & Journal != "NA" & JrnlType != "NA") %>% #remove any article that has no country listed
    nest(data = c(Code, DOI, Year, AuthorNum, Country, JrnlType, Region, IncomeGroup)) %>% 
    left_join(NumbArtOA2, by = "Journal") %>%
    mutate(Sample = map2(data, n, sample_n)) %>% 
    unnest(Sample)%>%
    select(Code, DOI, Journal, Year, AuthorNum, Country, JrnlType, Region,
           IncomeGroup)
  
  SiteBySpecPW2 <- SamplePW3 %>%
    group_by(Country)%>%
    tally()
  
  SiteBySpecPW2 <- SiteBySpecPW2 %>%
    spread(Country, n)
  
  PWDiversitySub <- diversity(SiteBySpecPW2, index = "invsimpson")
  SimpsDiversitySubs[,i] = PWDiversitySub
  
}

SimpsDivSubsPW <- as.data.frame(SimpsDiversitySubs)
write.csv(SimpsDivSubsPW, "CleanData/ProporSampsSimpsDivPW.csv", row.names = FALSE)


#For Loop to calculate over all simpsons diveristy (true diversity)
#using similar randomly sampled chunks of the over all pool
# USING LAST AUTHOR
SimpsDivSubsLast <- matrix(nrow = 1, ncol = 1000) #empty matrix for diversity

for (i in 1:1000){
  NumbArtOA2 <- NumbArtOA[-1]  
  SamplePW3 <- LastAuthPW %>% 
    filter(Country != "NA" & Journal != "NA" & JrnlType != "NA") %>% #remove any article that has no country listed
    nest(data = c(Code, DOI, Year, AuthorNum, Country, JrnlType, Region, IncomeGroup)) %>% 
    left_join(NumbArtOA2, by = "Journal") %>%
    mutate(Sample = map2(data, n, sample_n)) %>% 
    unnest(Sample)%>%
    select(Code, DOI, Journal, Year, AuthorNum, Country, JrnlType, Region,
           IncomeGroup)
  
  SiteBySpecPW2 <- SamplePW3 %>%
    group_by(Country)%>%
    tally()
  
  SiteBySpecPW2 <- SiteBySpecPW2 %>%
    spread(Country, n)
  
  PWDiversitySub <- diversity(SiteBySpecPW2, index = "invsimpson")
  SimpsDivSubsLast[,i] = PWDiversitySub
  
}

SimpsDivSubsPW <- as.data.frame(SimpsDivSubsLast)
write.csv(SimpsDivSubsPW, "CleanData/ProporSampsSimpsDivPWLast.csv", row.names = FALSE)

#For Loop to calculate over all simpsons diveristy (true diversity)
#using similar randomly sampled chunks of the over all pool
# USING ALL AUTHORs
SimpsDivSubsAllAuths <- matrix(nrow = 1, ncol = 1000) #empty matrix for diversity
NumbAuthsOA <- OpenAccessAll %>%
  group_by(Journal) %>%
  tally()

for (i in 1:1000){
  SamplePW3 <- PayWallAll %>% 
    filter(Country != "NA" & Journal != "NA" & JrnlType != "NA") %>% #remove any article that has no country listed
    nest(data = c(Code, DOI, Year, AuthorNum, Country, JrnlType, Region, IncomeGroup)) %>% 
    left_join(NumbAuthsOA, by = "Journal") %>%
    mutate(Sample = map2(data, n, sample_n)) %>% 
    unnest(Sample)%>%
    select(Code, DOI, Journal, Year, AuthorNum, Country, JrnlType, Region,
           IncomeGroup)
  
  SiteBySpecPW2 <- SamplePW3 %>%
    group_by(Country)%>%
    tally()
  
  SiteBySpecPW2 <- SiteBySpecPW2 %>%
    spread(Country, n)
  
  PWDiversitySub <- diversity(SiteBySpecPW2, index = "invsimpson")
  SimpsDivSubsAllAuths[,i] = PWDiversitySub
  
}

SimpsDivSubsPW <- as.data.frame(SimpsDivSubsAllAuths)
write.csv(SimpsDivSubsPW, "CleanData/SimpsDivSubsAllAuthsPW.csv", row.names = FALSE)








#FOR LOOP FOR making ricness and diversity matrices by journal with sampling
?diversity
# MAKE EMPTY MATRICES FOR THE FOR LOOP TO FILL
richness <- matrix(nrow = 62, ncol = 1000) #empty matrix for richness
SimpsonDiversity <- matrix(nrow = 62, ncol = 1000) #empty matrix for diversity

for (i in 1:1000){ #do loop 100 times
  NumbArtOA2 <- NumbArtOA[-1]   # remove "JnrlType" column from the NumbartOA dataframe
  
  #SUBSET
  #this subsets PW journals (FirstauthPW df) by the number of Aricles per journal in OA sources (the numbartoa df). can change this to lastauth as well
  SamplePW3 <- FirstAuthPW %>% 
    filter(Country != "NA" & Journal != "NA" & JrnlType != "NA") %>% #remove any article that has no country listed
    nest(data = c(Code, DOI, Year, AuthorNum, Country, JrnlType, Region, IncomeGroup)) %>% 
    left_join(NumbArtOA2, by = "Journal") %>%
    mutate(Sample = map2(data, n, sample_n)) %>% 
    unnest(Sample)%>%
    select(Code, DOI, Journal, Year, AuthorNum, Country, JrnlType, Region,
           IncomeGroup)
  #ADD OA DATA
  FirstAuthAll <- rbind(SamplePW3, FirstAuthOA) #put randomly sampled PW articles into 
  #the same data frame wiht our OA articles
  FirstAuthAll$JournalAndType <- paste(FirstAuthAll$Journal, FirstAuthAll$JrnlType)
  #TURN IT INTO SITE BY SPECIES (JRNL BY COUNTRY)
  SiteBySpec <- FirstAuthAll %>%
    group_by(JournalAndType, Country)%>%
    tally()
  
  SiteBySpec <- cast(SiteBySpec, JournalAndType ~ Country, value = 'n')
  SiteBySpec[is.na(SiteBySpec)] <- 0
  Countries <- names(SiteBySpec[,2:(ncol(SiteBySpec)-1)]) #check this to be sure this 
  
  ####
  #Add diversity metrics
  country_counts <- SiteBySpec[,Countries] #final site by species matrix
  row.names(country_counts) <- SiteBySpec$JournalAndType #add rownames for journals
  
  #simpson diversity index
  DivSimpson <-diversity(country_counts, index = "simpson")
  
  #richness
  rich <- rowSums(country_counts > 0)
  
  richness[,i] = rich
  SimpsonDiversity[,i] = DivSimpson
}

###################################################

#now manipulate the 62 x 1000 matrix of itterations
#richness

richness <- as.data.frame(richness) #make it a data frame
richness$MeanRich <- rowMeans(richness) #add a mean column from the 1000 itterations
write.csv(richness, "CleanData/FirstRich1000itters.csv", row.names = TRUE)

FirstAuthRich <- richness %>% #just grab the mean column
  select(MeanRich)
FirstAuthRich$JournalAndType <- SiteBySpec$JournalAndType #add journals in
#now for diversity
FirstAuthSimpDiv <- as.data.frame(SimpsonDiversity) 
  FirstAuthSimpDiv$MeanDiveristy <- rowMeans(FirstAuthSimpDiv)
write.csv(FirstAuthDiv, "CleanData/FirstDiv1000itter.csv", row.names = TRUE)
  FirstAuthSimpDiv <- FirstAuthSimpDiv %>%
  select(MeanDiveristy)
FirstAuthSimpDiv$JournalAndType <- SiteBySpec$JournalAndType

#make a richness and diversity data frame for each journal
FirstAuthDiv <- merge(FirstAuthSimpDiv, FirstAuthRich, by = "JournalAndType") 
#re-grab journal types to paste onto the diersity dataframe
AllData$JournalAndType <- paste(AllData$Journal, AllData$JrnlType) 
JournalTypes <- AllData %>%
  select(JournalAndType, JrnlType)%>%
  distinct()

FirstAuthDiv <- merge(FirstAuthDiv, JournalTypes, by = "JournalAndType") #merge for final first author dataframe
write.csv(FirstAuthDiv, "CleanData/FirstAuthDiv.csv", row.names = FALSE)











################
#CountryRichness OVERALL
################

# total country richness between the two journal types for FIRST authors
# this does not control for sample size
TotalRichFirst <- FirstAuth %>% 
  group_by(JrnlType) %>%
  summarise(Rich = n_distinct(Country))
# same as above for last authors
TotalRichLast <- LastAuth %>% 
  group_by(JrnlType) %>%
  summarise(Rich = n_distinct(Country))


RichnessOA <- FirstAuthOA %>%
  summarise(Rich = n_distinct(Country))

RichnessPW <- SamplePW2 %>%
  group_by(JrnlType) %>%
  summarise(Rich = n_distinct(Country))






##################################################################################################
#############################################################################################
#OLD CODE FOR OTHER SAMPLING TECHNIQUES> NO LONGER NEEDED!

#this is a way to randomly sample from each Paywall journal to 
#match the number of articles in its OA mirror journal --CKG

FirstAuthPWAlphab<-FirstAuthPW[order(FirstAuthPW$Journal),]#sort the paywall article df alphabetically by journal title

SamplePW1 <- strata(FirstAuthPWAlphab, "Journal", 
                    size = c(14,6,47,9,29,21,9,5,10,10,9,31,16,32,6,8,6,30,28,32,9,17,32,2,8,1,14,33,14,18,36),
                    method = "srswor")
sum(NumbArtOA$n) #using this to double check the sample size

summary(c(14,6,47,9,29,21,9,5,10,10,9,31,16,32,6,8,6,30,28,32,9,17,32,2,8,1,14,33,14,18,36))

# the code above!
# for each strata of journal, I took the number of articles available in the OA mirror 
# journals, and randomly sampled articles in PW journals based on those numbers. 
# I got the number of articles per OA journal from the NumbArtOA dataframe and just typed theminto the 'size' argument.
# Two issues: 1.) the sum of articles in NumbArtOA is 542, but the length of the FirstAuthOA dataframe is 553
# There are 11 articles in NumbArtOA that are 2018 articles, not 2019. I don't know why they weren't excluded in lines 47 and 52.
# specifically, they are all water research articles in theFirstAuthOA df. (lines 37, 57, 81, 116, 242, 293, 383, 392, 497, 510, 515)
# 2.) sleep medicine and research policy have 1 and 2 articles in those journals, respectively, according to NumbArtOA. I feel like we should delete them, 
# as we probably can't get a good diversity estimate from such a small number of articles.

SamplePW2 <- FirstAuthPW %>% #subset the paywall journals First Author Data 
  #filter(DOI != "NA") %>%
  group_by(Journal)%>%
  sample_n(c(14,6,47,9,29,21,9,5,10,10,9,31,16,32,6,8,6,30,28,32,9,17,32,2,8,1,14,33,14,18,36)) # this code only grabs 30 from each journal, which is a random number SIMILAR to the 
#numbers of articles in the open access journals
# Number of articles to pull should be from NumbArtOA dataframe
SamplePW4 <- FirstAuthOA %>% #subset the paywall journals First Author Data 
  #filter(DOI != "NA") %>%
  group_by(Journal)%>%
  sample_n(17, replace = TRUE) # this code only grabs 30 from each journal, which is a random number SIMILAR to the 





