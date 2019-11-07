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
  separate(Journal, c("Journal", "X"), ":")


# group 
NumbAuthors <- AllData %>% # number of authors per journal 
  filter(Year==2019) %>% 
  group_by(Journal,Year) %>% 
  summarize(n=n_distinct(DOI))

NumbArticles <-AllData %>% #number of papers per journal
  filter(Year==2019) %>% 
  group_by(JrnlType, Journal) %>% 
  summarize(n=n_distinct(DOI))

NumbArtOA <- NumbArticles %>%
  filter(JrnlType == "OA")
NumbArtPW <- NumbArticles %>%
  filter(JrnlType == "paywall")

#Data subsets

# first author
FirstAuthors <- AllData %>%
  filter(AuthorNum ==1)
#last author
LastAuthors <- AllData %>%
  group_by(DOI) %>%
  arrange(AuthorNum) %>%
  slice(n()) %>%
  ungroup


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

