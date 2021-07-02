# sample_pw_without_replace_allOA

# Generate the samples to plot the OA value against PW value

###################################################
# ALL OA vs. sampled PW
# sampling WITHOUT Replacement given the number available
################################################### 

# WITH ALL COUNTRIES
sampling_data <-AllData %>%
  group_by(refID) %>%
  slice(1) %>%
  filter(pair_key!=22) 
# turns out there are more OA than PW, so can't sample without replacement.
# There are very few articles in theis journal, so exzcluding is no problem 
sampling_data$pair_key<-droplevels(sampling_data$pair_key)

# WITHOUT CHN and the USA
sampling_data_noCHNUSA<-sampling_data %>% 
  filter(Code!="USA") %>% 
  filter(Code!="CHN") 
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
NoReplacePW_RichDiv$BootType<-"all_OA"
NoReplacePW_RichDiv$data<-Sys.Date()

NoReplacePW_Countries<-bind_rows(Countries_NoReplace,
                                 Countries_NoReplace_NoCHUSA)
NoReplacePW_Countries$BootType<-"all_OA"
NoReplacePW_Countries$date<-Sys.Date()

# summary(boot_results_countries)
write.csv(NoReplacePW_Countries, "./output/Sampled_Countries_allOA.csv", row.names = FALSE)
# Boot_Countries<-read_csv("./output/Boot_Countries.csv")
write.csv(NoReplacePW_RichDiv,'./output/Sampled_RichDiv_allOA.csv',row.names = FALSE)
# Boot_RichDiv<-read_csv("./output/Boot_RichDiv.csv")


