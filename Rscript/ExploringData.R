#
#This script is to start to clean and explore the data 
#

# data
load(file="./output/ALLDATA.RData")
head(ALLDATA,10)
str(ALLDATA)

# libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

head(ALLDATA)
AllData <- ALLDATA %>%
  select(DOI = DI, Journal = SO, Year = PY, AuthorNum = author,
         Country = country, CountryCode = country_code, JrnlType = jrnl_type)


# group 
summary_table <- AllData %>% 
  filter(Year==2019) %>% 
  group_by(Journal,Year) %>% 
  summarize(n=n_distinct(DOI))

summary_table2<-AllData %>% 
  filter(Year==2019) %>% 
  group_by(JrnlType, Journal) %>% 
  summarize(n=n_distinct(DOI))
 

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

