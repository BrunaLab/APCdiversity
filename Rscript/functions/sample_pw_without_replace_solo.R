sample_pw_without_replace_solo

# Generate the samples to plot the OA value against PW value

###################################################
# ALL OA vs. sampled PW
# sampling WITHOUT Replacement given the number available
################################################### 

# WITH ALL COUNTRIES
sampling_data <-AllData %>%
  filter(author=="solo") %>% 
  group_by(refID) %>%
  filter(pair_key!=18) %>% # exclude only for overall, first
  filter(pair_key!=25) %>% # for all
  filter(pair_key!=31) %>% # for solo only
  filter(pair_key!=35) %>% # for solo only
  filter(pair_key!=16) %>% # for solo only
  filter(pair_key!=11) %>% # for solo only
  filter(pair_key!=22) %>% # for solo only
  filter(pair_key!=28) %>% # for solo only
  filter(pair_key!=36) %>% # for solo only
  filter(pair_key!=30) %>% # for solo only
  filter(pair_key!=32) %>% # for solo only
  filter(pair_key!=38) %>% # for solo only
  slice(1) 
# %>%
#   filter(pair_key!=22) 
# # turns out there are more OA than PW, so can't sample without replacement.
# There are very few articles in theis journal, so exzcluding is no problem 
sampling_data$pair_key<-droplevels(sampling_data$pair_key)

# WITHOUT CHN and the USA
sampling_data_noCHNUSA<-sampling_data %>% 
  filter(Code!="USA") %>% 
  filter(Code!="CHN") %>% 
  filter(pair_key!=7) %>% # for solo only
  filter(pair_key!=1) %>% # for solo only
  filter(pair_key!=12) # for solo only
sampling_data_noCHNUSA$pair_key<-droplevels(sampling_data_noCHNUSA$pair_key)

source("./Rscript/functions/bootstrap_PW.R")
Sampling<-bootstrap_PW(sampling_data,"author_first")
Div_NoReplace<-as.data.frame(Sampling[1])
Countries_NoReplace<-as.data.frame(Sampling[2])
hist(Div_NoReplace$InvSimp)

source("./Rscript/functions/bootstrap_PW.R")
Sampling_noCHNUSA<-bootstrap_PW(sampling_data_noCHNUSA,"author_first")
Div_NoReplace_NoCHNUSA<-as.data.frame(Sampling_noCHNUSA[1])
Countries_NoReplace_NoCHUSA<-as.data.frame(Sampling_noCHNUSA[2])
hist(Div_NoReplace_NoCHNUSA$InvSimp)


NoReplacePW_RichDiv<-bind_rows(Div_NoReplace,
                                    Div_NoReplace_NoCHNUSA)
NoReplacePW_RichDiv$BootType<-"solo"
NoReplacePW_RichDiv$data<-Sys.Date()

NoReplacePW_Countries<-bind_rows(Countries_NoReplace,
                                 Countries_NoReplace_NoCHUSA)
NoReplacePW_Countries$BootType<-"solo"
NoReplacePW_Countries$date<-Sys.Date()

# summary(boot_results_countries)
write.csv(NoReplacePW_Countries, "./output/Sampled_Countries_solo.csv", row.names = FALSE)
# Boot_Countries<-read_csv("./output/Boot_Countries.csv")
write.csv(NoReplacePW_RichDiv,'./output/Sampled_RichDiv_solo.csv',row.names = FALSE)
# Boot_RichDiv<-read_csv("./output/Boot_RichDiv.csv")


