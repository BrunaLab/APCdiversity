


# data
load(file="./output/ALLDATA.RData")
head(ALLDATA,10)
str(ALLDATA)
library(tidyverse)
summary_table<-ALLDATA %>% filter(PY==2019) %>% group_by(SO,PY) %>% summarize(n=n_distinct(DI))
summary_table2<-ALLDATA %>% filter(PY==2019) %>% group_by(jrnl_type,SO) %>% summarize(n=n_distinct(DI))
