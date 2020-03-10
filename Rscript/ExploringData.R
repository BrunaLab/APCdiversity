####################################################
# This script is to start to clean and explore the data 
# ultimately I think we need to get to a site by species type matrix in which each site is a journal and 
# the sampling effort (number of articles) is the same for each mirror/sister journals
# in a site by species matrix, country is the species, income or region is like taxon or functional group
# maybe?
####################################################

# libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sampling)
library(reshape)
library(vegan)
library(purrr)

# data
load(file="./output/ALLDATA.RData")
head(ALLDATA,10)
str(ALLDATA)
CountryData <- read.csv("data/CLASS.csv", header = TRUE)
CountryData <- CountryData[-1,]

head(ALLDATA)

AllData <- ALLDATA %>%
  select(DOI = DI, Journal = SO, Year = PY, AuthorNum = author,
         Country = country, Code = country_code, JrnlType = jrnl_type)

CountryData <- CountryData %>%
  select(Code,Region, IncomeGroup = Income.group)

#this code merges the country data to include income category and geo region
AllData <- merge(AllData, CountryData, by="Code", all.x=TRUE) # merge 

head(AllData)

#scrap the "x" in the journal names, as this data is included in the JrnlType column
AllData <- AllData %>%
  separate(Journal, c("Journal", NA), ":") %>%
  separate(Journal, c("Journal", NA), " x") %>%
  separate(Journal, c("Journal", NA), " open")%>%
  filter(Journal != "biochimie") %>%
  filter(Year != 2018)
#small revison. replace '&' with 'and'
AllData[AllData$Journal=="biosensors & bioelectronics", "Journal"] <- "biosensors and bioelectronics"
list(AllData$Journal)

###################
# group and Subsets
####################

# There are some with missing DOI values, if you don't replace these
# They will be excluded from the grouping
AllData$DOI<- as.character(AllData$DOI)
AllData$DOI<- AllData$DOI %>% replace_na("missing_DOI")
AllData$DOI<- as.factor(AllData$DOI)

# NumbAuthors <- AllData %>% # number of authors per journal 
#   filter(Year==2019) %>% 
#   group_by(JrnlType, Journal,Year) %>% 
#   summarize(n=n_distinct(DOI))

NumbAuthors <- AllData %>% # average number of authors per journal 
  filter(Year==2019) %>% 
  group_by(JrnlType,Journal,DOI) %>% 
  arrange(JrnlType, Journal) %>% 
  filter(AuthorNum == max(AuthorNum)) %>% 
  group_by(JrnlType, Journal) %>% 
  summarize(avg_n=mean(AuthorNum),sd_n=sd(AuthorNum))
NumbAuthors

NumbArticles <- AllData %>% #number of papers per journal
  filter(Year==2019) %>% 
  group_by(JrnlType, Journal) %>% 
  summarize(n=n_distinct(DOI))
NumbArticles

NumbArtOA <- NumbArticles %>% #number of articles that are Open Access
  filter(JrnlType == "OA")
NumbArtOA

TOTAL_NumbArtOA<-sum(NumbArtOA$n) #total number summed accross all OA journals
TOTAL_NumbArtOA


NumbArtPW <- NumbArticles %>% #number of articles that are paywall
  filter(JrnlType == "paywall")
NumbArtPW

TOTAL_NumbArtPW<-sum(NumbArtPW$n) #total number summed accross all OA journals
TOTAL_NumbArtPW

OpenAccessAll <- AllData %>% #use the numbers of articles here to select those from paywal journals
  filter(JrnlType == "OA")

PayWallAll <- AllData %>% 
  filter(JrnlType == "paywall")


test <- cbind(NumbArtOA, NumbArtPW)

#Data subsets by author
# first author subsets
FirstAuth <- AllData %>%
  filter(AuthorNum == 1)

FirstAuthOA <- FirstAuth %>%
  filter(JrnlType == "OA")
FirstAuthPW <- FirstAuth %>%
  filter(JrnlType == "paywall")

#last author subsets
LastAuth <- AllData %>%
  group_by(DOI) %>%
  arrange(AuthorNum) %>%
  slice(n()) %>%
  ungroup

LastAuthOA <- LastAuth %>%
  filter(JrnlType == "OA")

LastAuthPW <- LastAuth %>%
  filter(JrnlType == "paywall")



#########
LastAuthOA
FirstAuthOA

first_and_last_OA<-inner_join(FirstAuthOA,LastAuthOA,by="DOI")
first_and_last_OA$country_same<-first_and_last_OA$Country.x==first_and_last_OA$Country.y
summary(first_and_last_OA$country_same)  
first_and_last_OA$Income_same<-first_and_last_OA$IncomeGroup.x==first_and_last_OA$IncomeGroup.y
summary(first_and_last_OA$Income_same)  
first_and_last_OA$Region_same<-first_and_last_OA$Region.x==first_and_last_OA$Region.y
summary(first_and_last_OA$Region_same)  


LastAuthPW
FirstAuthPW

############################################
# shaping data to do barcharts of country representation
# income representation
# and region representation
#################################################
PayWallAllGeo <- PayWallAll %>%
  filter(Country != "NA" & Code != "NA") %>%
  group_by(Country, Code)%>%
  tally()

PayWallAllGeo <- left_join(PayWallAllGeo, CountryData, by = "Code")

PayWallAllGeoInc <- PayWallAll %>%
  filter(IncomeGroup != "NA")%>%
  group_by(IncomeGroup)%>%
  tally()

OAAllGeo <- OpenAccessAll %>%
  filter(Country != "NA" & Code != "NA") %>%
  group_by(Country, Code)%>%
  tally()

OAAllGeo <- left_join(OAAllGeo, CountryData, by = "Code")

OAAllInc <- OpenAccessAll %>%
  filter(IncomeGroup != "NA") %>%
  group_by(IncomeGroup) %>%
  tally()

#Country representation all authors PW
ggplot(PayWallAllGeo, aes(Country, n, fill = IncomeGroup))+
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("Country Representation by IncomeGroup PW")

# Country Representation All authors OA
ggplot(OAAllGeo, aes(Country, n, fill = IncomeGroup))+
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("Country Representation by Income Group OA")

#income representation of all authors PW
ggplot(PayWallAllGeoInc, aes(IncomeGroup,
                             y = n, fill = IncomeGroup))+
  geom_bar(stat = "identity")+
  ggtitle("Author Country Income Group For all Authors
          in PW Journals")
# income representation of all authors OA
ggplot(OAAllInc, aes(IncomeGroup,
                     y = n, fill = IncomeGroup))+
  geom_bar(stat = "identity")+
  ggtitle("Author Country Income Group for All Authors
          in OA Journals")

##################################################
#SUBSET and bootstrap Paywall Journals by the number found in Open Access Journals
##################################################

#Calculate the diversity indices for OA JOURNALS by each JOURNAL!
SiteBySpec1 <- FirstAuthOA %>%
  group_by(Journal, Country)%>%
  tally()

SiteBySpec1 <- cast(SiteBySpec1, Journal ~ Country, value = 'n')
SiteBySpec1[is.na(SiteBySpec1)] <- 0
Countries <- names(SiteBySpec1[,2:60]) #check this to be sure this 

####
#Add diversity metrics
country_countsOA <- SiteBySpec1[,Countries] #final site by species matrix
row.names(country_countsOA) <- SiteBySpec1$Journal #add rownames for journals

#simpson diversity index
DivSimpsonOA <-diversity(country_countsOA, index = "simpson")

#abundance
abundOA <-rowSums(country_countsOA)

#richness
richOA <- rowSums(country_counts > 0)

DivMetrics <- as.data.frame(cbind(richOA, abundOA, DivSimpsonOA))
DivMetrics$Journal <- SiteBySpec1$Journal
DivMetrics$EffectSpecNumOA <- 1/(1-DivMetricsOA$DivSimpsonOA)

#Calculate the diversity indices for PW JOURNALS by each JOURNAL!
SiteBySpec2 <- FirstAuthPW %>%
  filter(Country != "NA") %>%
  group_by(Journal, Country)%>%
  tally()

SiteBySpec2 <- cast(SiteBySpec2, Journal ~ Country, value = 'n')
SiteBySpec2[is.na(SiteBySpec2)] <- 0
Countries <- names(SiteBySpec2[,2:126]) #check this to be sure this 

####
#Add diversity metrics
country_countsPW <- SiteBySpec2[,Countries] #final site by species matrix
row.names(country_countsPW) <- SiteBySpec2$Journal #add rownames for journals

#simpson diversity index
DivSimpsonPW <-diversity(country_countsPW, index = "simpson")

#abundance
abundPW <-rowSums(country_countsPW)

#richness
richPW <- rowSums(country_countsPW > 0)

DivMetricsPW <- as.data.frame(cbind(richPW, abundPW, DivSimpsonPW))
DivMetricsPW$Journal <- SiteBySpec2$Journal
DivMetricsPW$EffectSpecNumPW <- 1/(1-DivMetricsPW$DivSimpsonPW)

DivMetrics <- merge(DivMetrics, DivMetricsPW, by = "Journal")
DivMetrics$DeltaDiv <- DivMetrics$EffectSpecNumOA - DivMetrics$EffectSpecNumPW

boxplot(DivMetrics$DeltaDiv)
median(DivMetrics$DeltaDiv)

write.csv(DivMetrics, "CleanData/FirstDivMetricsByJrnl.csv", row.names = FALSE)


?diversity

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
n_distinct(SamplePW3$Journal) #number of journals in PW
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





