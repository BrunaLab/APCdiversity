# This code will calclulate the Geographic Diversity of 1st authors 
# for non-OA articles in subscription journals when removing 
# countries one at a time from the dataset. It then bootstraps these 
# results to calclulate the mean and 95% confidence Intervals to 
# identify countries whose removal will significantly increase or decrease 
# diversity. This justifies the removal of numerically dominant countries
# from the analyses. 

library(tidyverse)
AllData$Journal<-as.factor(AllData$Journal)
AllData$Code<-as.factor(AllData$Code)

# Select only the PW articles in PW journals
PW_articles <- AllData %>%
  filter(JrnlType == "PW") %>% 
  filter(ArticleType=="PW") %>% 
  group_by(refID) %>% 
  slice(1) %>% 
  drop_na(Code)
PW_articles$Code<-droplevels(PW_articles$Code)

# each country is assigned a number; this allows for the loop 
# removing one number (country) at at time and then calclulating 
# diversity
PW_articles$Code.numeric<-as.numeric(PW_articles$Code)

# this is each 3-letter country code and its respective numeric code
codes<-PW_articles %>% ungroup() %>% select(Code, Code.numeric) %>% unique()
############################################
# now sample and iterate!
# 1<-Richness  2<-InvSimpsons 3<-Countries
############################################
source("./Rscript/functions/DivRichCalc.R")
PW_articles<-as.data.frame(PW_articles)
# levels(as.factor(PW_articles$Code))
# This is the number of countriesm
codemax<-max(PW_articles$Code.numeric,na.rm = TRUE)
#create a df to fill with results
PW_div<-as.data.frame(rep(NA, codemax))
names(PW_div)<-c("Richness")
replicate<-data.frame(replicate=as.numeric())
# Countries_PW<-rep(NA, codemax)

for(i in 1:codemax){
  
  #starting at i, eliminate one country at a time. Note - 0 is all countries 
  filtered_data<-filter(PW_articles,Code.numeric!=i)
  
  AuPosition<-"author_first"
  ArticleType<-"PW"
  JrnlType<-"PW"
  results<-DivRichCalc(filtered_data,AuPosition,JrnlType,ArticleType)
  PW_div[i,1]<-(results)[1]
  PW_div[i,2]<-(results)[2]
  PW_div[i,3]<-(results)[4]
  PW_div[i,4]<-(results)[5]
  PW_div[i,5]<-i
  # Countries_PW[i]<-(results_mirror)[3]
  count<-data.frame(replicate=rep(i, each=PW_div[i,1]))
  replicate<- bind_rows(replicate,count)
  #this is the counter to let you know where it is in the loop
  cat("\r", i, "of", codemax) 
  flush.console()
}

# clean up the resulting df of diversity stats after excluding each country
PW_div<-PW_div %>% 
  dplyr::rename("Code.numeric"="V5") %>% 
  left_join(codes) %>% 
  arrange(InvSimpsons)

# calclulate diversity when ALL countries are included
DivAllCountries<-as.data.frame(DivRichCalc(PW_articles,AuPosition,JrnlType,ArticleType))

# Add the results for all countries to the dataframe
DivAllCountries<-DivAllCountries %>% 
  slice(1) %>% 
  select(-Countries.n) %>% 
  mutate(Countries.Code="All Countries") %>% 
  mutate(Code.numeric=0) %>% 
  dplyr::rename("Code"="Countries.Code")

PW_div<-bind_rows(DivAllCountries,PW_div)

# calc the change in diversity after removing each country by subtracting
PW_div<-PW_div %>%
  mutate(Change_in_Div=InvSimpsons-DivAllCountries$InvSimpsons) %>% 
  arrange(desc(Change_in_Div)) 
# round the result to 4 decimal points
PW_div$Change_in_Div<-round(PW_div$Change_in_Div,4)

# Bootstrap the results to calclulate the mean change in diveristy 
# and 95% CIs

nboot<-1000
BS_results<-rep(NA, nboot)
BS_results<-as.data.frame(BS_results)
N_countries<-as.numeric(PW_div %>% summarize(n_distinct(Code)))
for(i in 1:nboot){
  PW_div_1out<-filter(PW_div,Code!="All Countries")
  boot<-sample_n(PW_div_1out,N_countries,replace=TRUE)
  BS_results[i,1]<-mean(boot$InvSimp)
  count<-data.frame(replicate=rep(i, each=BS_results[i,1]))
  replicate<- bind_rows(replicate,count)
  #this is the counter to let you know where it is in the loop
  cat("\r", i, "of", nboot) 
  flush.console()
}

hist(BS_results$BS_results)

# Alpha for percentile confidence intervals see
# https://cran.r-project.org/web/packages/broom/vignettes/bootstrapping.html
alpha <- .05
BootInfo<-BS_results %>% 
  summarize(avg_div=mean(BS_results),
            sd_avg_div=sd(BS_results),
            count = n(),
            CIlow=quantile(BS_results, alpha/2),
            CIhigh=quantile(BS_results, 1-alpha/2))

BootInfo$avg_div<-round(BootInfo$avg_div,3)
BootInfo$sd_avg_div<-round(BootInfo$sd_avg_div,3)
BootInfo$CIlow<-round(BootInfo$CIlow,2)
BootInfo$CIhigh<-round(BootInfo$CIhigh,2)

# Results: mean, SD, CIs
BootInfo
plot(PWboot$code_as_numeric,PWboot$InvSimp)

# These are the countries for countries whose loss makes 
# diversity lower than the lower 95% CI  
# TRUE = higher than upper CI, ie sig increases div
PW_div$above_upper_ci<-PW_div$InvSimp>BootInfo$CIhigh
# TRUE = lower than lower CI, ie sig lowers div
PW_div$below_lower_ci<-PW_div$InvSimp<BootInfo$CIlow 


codes_lowering<-which(PW_div$InvSimp<lower95CI)
codes_increasing<-which(PW_div$InvSimp>upper95CI)

Code_lowering_div<-PW_div %>% filter(below_lower_ci==TRUE) %>% select(Code)
Code_increasing_div<-PW_div %>% filter(above_upper_ci==TRUE) %>% select(Code)




# Are there any countries that have big effects AFTER removing CHN? 

PW_noCHN<-PW_articles %>% 
  filter(Code!="CHN") %>% 
  filter(Code!="All Countries")
PW_noCHN<-as.data.frame(PW_noCHN)
# levels(as.factor(PW_articles$Code))
# This is the number of countriesm
codemax2<-nlevels((as.factor(PW_noCHN$Code.numeric)))
#create a df to fill with results
PW_div_noCHN<-as.data.frame(rep(NA, codemax))
names(PW_div_noCHN)<-c("Richness")
replicate<-data.frame(replicate=as.numeric())
# Countries_PW<-rep(NA, codemax)

for(i in 1:codemax2){
  
  #starting at i, eliminate one country at a time. Note - 0 is all countries 
  filtered_data<-filter(PW_noCHN,Code.numeric!=i)
  
  AuPosition<-"author_first"
  ArticleType<-"PW"
  JrnlType<-"PW"
  results<-DivRichCalc(filtered_data,AuPosition,JrnlType,ArticleType)
  PW_div_noCHN[i,1]<-(results)[1]
  PW_div_noCHN[i,2]<-(results)[2]
  PW_div_noCHN[i,3]<-(results)[4]
  PW_div_noCHN[i,4]<-(results)[5]
  PW_div_noCHN[i,5]<-i
  # Countries_PW[i]<-(results_mirror)[3]
  count<-data.frame(replicate=rep(i, each=PW_div_noCHN[i,1]))
  replicate<- bind_rows(replicate,count)
  #this is the counter to let you know where it is in the loop
  cat("\r", i, "of", codemax2) 
  flush.console()
}

# clean up the resulting df of diversity stats after excluding each country
PW_div_noCHN<-PW_div_noCHN %>% 
  dplyr::rename("Code.numeric"="V5") %>% 
  left_join(codes) %>% 
  arrange(InvSimpsons)



# calclulate diversity when ALL countries are included
Div_noCHN<-as.data.frame(DivRichCalc(PW_noCHN,AuPosition,JrnlType,ArticleType))
PW_div_noCHN
# Add the results for all countries to the dataframe
Div_noCHN<-Div_noCHN %>% 
  slice(1) %>% 
  select(-Countries.n) %>% 
  mutate(Countries.Code="All Countries") %>% 
  mutate(Code.numeric=0) %>% 
  dplyr::rename("Code"="Countries.Code")

PW_div_noCHN<-bind_rows(Div_noCHN,PW_div_noCHN)

# calc the change in diversity after removing each country by subtracting
PW_div_noCHN<-PW_div_noCHN %>%
  mutate(Change_in_Div=InvSimpsons-Div_noCHN$InvSimpsons) %>% 
  arrange(desc(Change_in_Div)) 
# round the result to 4 decimal points
PW_div_noCHN$Change_in_Div<-round(PW_div_noCHN$Change_in_Div,4)


# REMOVING USA INCREASES DIV from 17->25. 
# No other country's removal increases diversity.
# Only a few countries' removal increases diversity a small amount.




PW_div_noCHN$above_upper_ci<-PW_div_noCHN$InvSimp>BootInfo$CIhigh
# TRUE = lower than lower CI, ie sig lowers div
PW_div_noCHN$below_lower_ci<-PW_div_noCHN$InvSimp<BootInfo$CIlow 

Code_lowering_div_noCHN<-PW_div_noCHN %>% filter(below_lower_ci==TRUE) %>% select(Code)
Code_increasing_div_noCHN<-PW_div_noCHN %>% filter(above_upper_ci==TRUE) %>% select(Code)


Plot_PW_div <- PW_div %>%
  arrange(Code.numeric) %>% 
  filter(Code!="All Countries") %>%
  mutate(Effect_on_Div=(ifelse(above_upper_ci==TRUE,"Significant Increase",NA))) %>% 
  mutate(Effect_on_Div=(ifelse(below_lower_ci==TRUE,"Significant Decrease",Effect_on_Div))) %>% 
  mutate(Effect_on_Div=ifelse(is.na(Effect_on_Div)==TRUE,"None",Effect_on_Div)) %>% 
  dplyr::rename("Effect on Diversity"="Effect_on_Div")




PW_div



# Alpha for percentile confidence intervals see
# https://cran.r-project.org/web/packages/broom/vignettes/bootstrapping.html
alpha <- .05
BootInfo_noCHN <- na.omit(PW_div_noCHN) %>% 
  summarize(avg_div=mean(Change_in_Div),
            sd_avg_div=sd(Change_in_Div),
            count = n(),
            CIlow=quantile(Change_in_Div, alpha/2),
            CIhigh=quantile(Change_in_Div, 1-alpha/2))

BootInfo_noCHN$avg_div<-round(BootInfo_noCHN$avg_div,3)
BootInfo_noCHN$sd_avg_div<-round(BootInfo_noCHN$sd_avg_div,3)
BootInfo_noCHN$CIlow<-round(BootInfo_noCHN$CIlow,2)
BootInfo_noCHN$CIhigh<-round(BootInfo_noCHN$CIhigh,2)
write_csv(Plot_PW_div,"./output/div_remove_one_country.csv")
write_csv(plot_PW_div_noCHN,"./output/div_remove_second_country.csv")

plot_PW_div_noCHN<-PW_div_noCHN %>% 
  mutate(above_upper_ci=Change_in_Div>BootInfo_noCHN$CIhigh) %>% 
  mutate(below_lower_ci=Change_in_Div<BootInfo_noCHN$CIlow) %>% 
  arrange(Code.numeric) %>% 
  filter(Code!="All Countries") %>% 
  mutate(Effect_on_Div=(ifelse(above_upper_ci==TRUE,"Significant Increase",NA))) %>% 
  mutate(Effect_on_Div=(ifelse(below_lower_ci==TRUE,"Significant Decrease",Effect_on_Div))) %>% 
  mutate(Effect_on_Div=ifelse(is.na(Effect_on_Div)==TRUE,"None",Effect_on_Div)) %>% 
  dplyr::rename("Effect on Diversity"="Effect_on_Div")
# mutate(Effect_on_Div=(ifelse(above_upper_ci==TRUE,"Significant Increase",NA))) %>% 
#   mutate(Effect_on_Div=(ifelse(below_lower_ci==TRUE,"Significant Decrease",Effect_on_Div))) %>% 
#   mutate(Effect_on_Div=ifelse(is.na(Effect_on_Div)==TRUE,"None",Effect_on_Div))






