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


