#
#This script is to start to clean and explore the data 
#

# libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

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
  filter(Journal != "biochimie")

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
#SUBSET Paywall Journals by the number found in Open Access Journals
##################################################
SamplePW <- FirstAuthPW %>% #subset the paywall journals First Author Data 
  filter(DOI != "NA") %>%
  group_by(Journal)%>%
  sample_n(30) # this code only grabs 30 from each journal, which is a random number SIMILAR to the 
#numbers of articles in the open access journals

##
#CountryRichness
##
# total country richness between the two journal types for FIRST authors
# this does not control for sample size
TotalRichFirst <- FirstAuthors %>% 
  group_by(JrnlType) %>%
  summarise(Rich = n_distinct(Country))
# same as above for last authors
TotalRichLast <- LastAuthors %>% 
  group_by(JrnlType) %>%
  summarise(Rich = n_distinct(Country))

