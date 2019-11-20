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

NumbAuthors <- AllData %>% # number of authors per journal 
  filter(Year==2019) %>% 
  group_by(JrnlType, Journal,Year) %>% 
  summarize(n=n_distinct(DOI))

NumbArticles <- AllData %>% #number of papers per journal
  filter(Year==2019) %>% 
  group_by(JrnlType, Journal) %>% 
  summarize(n=n_distinct(DOI))

NumbArtOA <- NumbArticles %>% #number of articles that are Open Access
  filter(JrnlType == "OA")

NumbArtPW <- NumbArticles %>% #number of articles that are paywall
  filter(JrnlType == "paywall")

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
  filter(JrnlType == "PW")



##################################################
#SUBSET and bootstrap Paywall Journals by the number found in Open Access Journals
##################################################

###################################################################################################
#############################################################################################
# I think we can delete this chunk of code

#this is a way to randomly sample from each Paywall journal to 
#match the number of articles in its OA mirror journal --CKG

FirstAuthPWAlphab<-FirstAuthPW[order(FirstAuthPW$Journal),]#sort the paywall article df alphabetically by journal title

SamplePW1 <- strata(FirstAuthPWAlphab, "Journal", 
            size = c(14,6,47,9,29,21,9,5,10,10,9,31,16,32,6,8,6,30,28,32,9,17,32,2,8,1,14,33,14,18,36),
            method = "srswor")
sum(NumbArtOA$n) #using this to double check the sample size

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




#NEXT STEPS:
# once we figure out the correct smapling from paywall hournals we next
# need to make TWO site (journal) by species () matrices. One with Paywall
# One for Open Access



head(SamplePW2)

#############################################################################################
###################################################################################################

NumbArtOA2 <- NumbArtOA[-1]   # remove "JnrlType" column from the NumbartOA dataframe

#this subsets PW journals (FirstauthPW df) by the number of Aricles per journal in OA sources (the numbartoa df). can change this to lastauth as well
SamplePW3<-FirstAuthPW %>% 
  filter(Country != "NA") %>% #remove any article that has no country listed
  group_by(Journal)
  nest() %>% 
  ungroup()
  mutate(n = c(14,6,47,9,29,21,9,5,10,10,9,31,16,32,6,8,6,30,28,32,9,17,32,2,8,1,14,33,14,18,36)) 
  mutate(samp = map2(data, n, sample_n)) %>%
    select(-data) %>%
    unnest(samp)
  
?mutate

FirstAuthAll <- rbind(SamplePW3, FirstAuthOA) #put randomly sampled PW articles into 
#the same data frame wiht our OA articles
FirstAuthAll$JournalAndType <- paste(FirstAuthAll$Journal, FirstAuthAll$JrnlType)

SiteBySpec <- FirstAuthAll %>%
  group_by(JournalAndType, Country)%>%
  tally()

SiteBySpec <- cast(SiteBySpec, JournalAndType ~ Country, value = 'n')
SiteBySpec[is.na(SiteBySpec)] <- 0
Countries <- names(SiteBySpec[,2:73]) #check this to be sure this 

####
#Add diversity metrics
country_counts <- SiteBySpec[,Countries] #final site by species matrix
row.names(country_counts) <- SiteBySpec$JournalAndType #add rownames for journals

#simpson diversity index
DivSimpson <-diversity(country_counts, index = "simpson")

#abundance
abund <-rowSums(country_counts)

#richness
rich <- rowSums(country_counts > 0)

DivMetrics <- cbind(rich, abund, DivSimpson)

#sum(abund)# double check we have the same number of articles still
#sum(SiteBySpec) #check against this


#############################################################################################
# FOR LOOP
###################################################################################################
n_distinct(SamplePW3$Journal) #number of journals in PW
n_distinct(FirstAuthOA$Journal) #number of jourals in OA, they are the same!


richness <- matrix(nrow = 62, ncol = 1000) #empty matrix for richness
SimpsonDiversity <- matrix(nrow = 62, ncol = 1000) #empty matrix for diversity

for (i in 1:1000){ #do loop 100 times
  NumbArtOA2 <- NumbArtOA[-1]   # remove "JnrlType" column from the NumbartOA dataframe
  
  #this subsets PW journals (FirstauthPW df) by the number of Aricles per journal in OA sources (the numbartoa df). can change this to lastauth as well
  SamplePW3 <- FirstAuthPW %>% 
    filter(Country != "NA" & Journal != "NA" & JrnlType != "NA") %>% #remove any article that has no country listed
    nest(data = c(Code, DOI, Year, AuthorNum, Country, JrnlType, Region, IncomeGroup)) %>% 
    left_join(NumbArtOA2, by = "Journal") %>%
    mutate(Sample = map2(data, n, sample_n)) %>% 
    unnest(Sample)%>%
    select(Code, DOI, Journal, Year, AuthorNum, Country, JrnlType, Region,
           IncomeGroup)
  
  FirstAuthAll <- rbind(SamplePW3, FirstAuthOA) #put randomly sampled PW articles into 
  #the same data frame wiht our OA articles
  FirstAuthAll$JournalAndType <- paste(FirstAuthAll$Journal, FirstAuthAll$JrnlType)
  
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





#>>>>>>> 7bf2df0ce9cc2af010084e7a76a2dd4c45904029

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

