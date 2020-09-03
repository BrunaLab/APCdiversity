Table2<-function(sole_author_pubs_ALL_first_author,sole_author_pubsNOCHNUSA_first_author,
                 coauthor_pubs_ALL_first_author,coauthor_pubsNOCHNUSA_first_author){
  vars<-list(sole_author_pubs_ALL_first_author,
             sole_author_pubsNOCHNUSA_first_author,
             coauthor_pubs_ALL_first_author,
             coauthor_pubsNOCHNUSA_first_author)
source("./Rscript/functions/DivRichCalcTable_Solo.R")
source("./Rscript/functions/DivRichCalcSummaryTable_sampled.R")


SubsampledPW.results_Solo<-read_csv('output/SubsampledPW.results_RichDiv_SOLO_ALL.csv')
SubsampledPW.results_Solo_NoUSACHN<-read_csv('output/SubsampledPW.results_RichDiv_SOLO_NoUSACHN.csv')
SubsampledPW.results_First<-read_csv('output/SubsampledPW.results_RichDiv_CO_ALL.csv')
SubsampledPW.results_First_NoUSACHN<-read_csv('output/SubsampledPW.results_RichDiv_CO_NOUSACHN.csv')

Table2_Solo<-DivRichCalcTable_Solo(as.data.frame(vars[1]),
                                   as.data.frame(vars[2]),
                                   SubsampledPW.results_Solo,
                                   SubsampledPW.results_Solo_NoUSACHN)


Table2_CoAuthored<-DivRichCalcSummaryTable_sampled(as.data.frame(vars[3]),
                                                   as.data.frame(vars[4]),
                                                   SubsampledPW.results_First,
                                                   SubsampledPW.results_First_NoUSACHN)


 
  #######################################
  # put together in a a Table 
  #######################################

Table2<-bind_rows(Table2_CoAuthored,Table2_Solo)
Table2$author <- ordered(Table2$author, levels = c("solo","first", "last","all"))
Table2<-Table2 %>% 
  select(-CIlow,-CIhigh,-CIlow1,-CIhigh1) %>% 
  filter(author!="all") %>% 
  arrange(desc(metric),author)
  
Table2$PW_AllCountries<-as.character(Table2$PW_AllCountries)
Table2$PW_noUSAorCHN<-as.character(Table2$PW_noUSAorCHN)
Table2$PW_AllCountries<-str_replace(Table2$PW_AllCountries, "[+]", "\u00B1")
Table2$PW_AllCountries<-str_replace(Table2$PW_AllCountries, "[/]", "")
Table2$PW_AllCountries<-str_replace(Table2$PW_AllCountries, "[-]", "")
Table2$PW_noUSAorCHN<-str_replace(Table2$PW_noUSAorCHN, "[+]", "\u00B1")
Table2$PW_noUSAorCHN<-str_replace(Table2$PW_noUSAorCHN, "[/]", "")
Table2$PW_noUSAorCHN<-str_replace(Table2$PW_noUSAorCHN, "[-]", "")
Table2$author<-str_to_title(Table2$author)
Table2$author<-gsub("Solo","Single",Table2$author)
names(Table2)<-c("Author","Metric","OA (All Countries)","Mean PW (All Countries)",
                       "PW 95% CI (All Countries)", "OA (USA & CHN excluded)",
                       "Mean PW (USA & CHN excluded)","PW 95% CI (USA & CHN excluded)")
Table2 <- Table2 %>% select("Metric","Author","OA (All Countries)",
                                        "Mean PW (All Countries)","PW 95% CI (All Countries)",
                                        "OA (USA & CHN excluded)","Mean PW (USA & CHN excluded)",
                                        "PW 95% CI (USA & CHN excluded)")

Table2<-as.data.frame(Table2)

# Table2$Metric <-c("Richness","","Diversity","")




sole_ALL<-read_csv("./data_clean/sole_author_pubs_ALL_first_author.csv")
sole_NOCHNUSA<-read_csv("./data_clean/one_author_pubsNOCHNUSA.csv")
coauthor_ALL<-read_csv("./data_clean/coauthor_pubs_ALL_first_author.csv")
coauthor_NOCHNUSA<-read_csv("./data_clean/coauthor_pubsNOCHNUSA.csv")


source("./Rscript/functions/DivRichCalc.R")
crit_solo_all<-DivRichCalc(sole_ALL,"author_first","OA")
crit_solo_no_CHNUSA<-DivRichCalc(sole_NOCHNUSA,"author_first","OA")
crit_first_all<-DivRichCalc(coauthor_ALL,"author_first","OA")
crit_first_no_CHNUSA<-DivRichCalc(coauthor_NOCHNUSA,"author_first","OA")

# SOLO, ALL, RICH
R_crit_solo_all<-as.numeric(crit_solo_all[1])
perc_R_SOLO_ALL<-SubsampledPW.results_Solo %>% 
  tally(Richness>R_crit_solo_all)/1000
perc_R_SOLO_ALL
perc_R_SOLO_ALL$Author<-"Single"
perc_R_SOLO_ALL$Dataset<-"All Countries"
perc_R_SOLO_ALL$Metric<-"Richness"

# SOLO, NO CHN USA, RICH
R_crit_solo_no_CHNUSA<-as.numeric(crit_solo_no_CHNUSA[1])
perc_R_SOLO_NOCHNUSA<-SubsampledPW.results_Solo_NoUSACHN %>% 
  tally(Richness>R_crit_solo_no_CHNUSA)/1000
perc_R_SOLO_NOCHNUSA
perc_R_SOLO_NOCHNUSA$Author<-"Single"
perc_R_SOLO_NOCHNUSA$Dataset<-"Without China & USA"
perc_R_SOLO_NOCHNUSA$Metric<-"Richness"



# FIRST, ALL, RICH
R_crit_first_all<-as.numeric(crit_first_all[1])
perc_R_first_all<-SubsampledPW.results_First %>% 
  tally(Richness>R_crit_first_all)/1000
perc_R_first_all
perc_R_first_all$Author<-"First"
perc_R_first_all$Dataset<-"All Countries"
perc_R_first_all$Metric<-"Richness"


# FIRST, NO CHN USA, RICH
R_crit_first_no_CHNUSA<-as.numeric(crit_first_no_CHNUSA[1])
perc_R_first_NOCHNUSA<-SubsampledPW.results_First_NoUSACHN %>% 
  tally(Richness>R_crit_first_no_CHNUSA)/1000
perc_R_first_NOCHNUSA
perc_R_first_NOCHNUSA$Author<-"First"
perc_R_first_NOCHNUSA$Dataset<-"Without China & USA"
perc_R_first_NOCHNUSA$Metric<-"Richness"


# SOLO, ALL, DIV
Div_crit_solo_all<-as.numeric(crit_solo_all[2])
perc_Div_SOLO_ALL<-SubsampledPW.results_Solo %>% 
  tally(InvSimp>Div_crit_solo_all)/1000
perc_Div_SOLO_ALL
perc_Div_SOLO_ALL$Author<-"Single"
perc_Div_SOLO_ALL$Dataset<-"All Countries"
perc_Div_SOLO_ALL$Metric<-"Diversity"

# SOLO, NO CHN USA, DIV
Div_crit_solo_no_CHNUSA<-as.numeric(crit_solo_no_CHNUSA[2])
perc_D_SOLO_NOCHNUSA<-SubsampledPW.results_Solo_NoUSACHN %>% 
  tally(InvSimp>Div_crit_solo_no_CHNUSA)/1000
perc_D_SOLO_NOCHNUSA
perc_D_SOLO_NOCHNUSA$Author<-"Single"
perc_D_SOLO_NOCHNUSA$Dataset<-"Without China & USA"
perc_D_SOLO_NOCHNUSA$Metric<-"Diversity"



# FIRST, ALL, DIV
Div_crit_first_all<-as.numeric(crit_first_all[2])
perc_Div_first_ALL<-SubsampledPW.results_First %>% 
  tally(InvSimp>Div_crit_first_all)/1000
perc_Div_first_ALL<-perc_Div_first_ALL
perc_Div_first_ALL$Author<-"First"
perc_Div_first_ALL$Dataset<-"All Countries"
perc_Div_first_ALL$Metric<-"Diversity"


# FIRST, NO CHN USA, DIV
Div_crit_first_no_CHNUSA<-as.numeric(crit_first_no_CHNUSA[2])
perc_D_first_NOCHNUSA<-SubsampledPW.results_First_NoUSACHN %>% 
  tally(InvSimp>Div_crit_first_no_CHNUSA)/1000
perc_D_first_NOCHNUSA<-perc_D_first_NOCHNUSA
perc_D_first_NOCHNUSA$Author<-"First"
perc_D_first_NOCHNUSA$Dataset<-"Without China & USA"
perc_D_first_NOCHNUSA$Metric<-"Diversity"


probs<-bind_rows(perc_R_SOLO_ALL,
                 perc_R_SOLO_NOCHNUSA,
                 perc_R_first_all,
                 perc_R_first_NOCHNUSA,
                 perc_Div_SOLO_ALL,
                 perc_D_SOLO_NOCHNUSA,
                 perc_Div_first_ALL,
                 perc_D_first_NOCHNUSA)

probs<-probs %>% dplyr::rename("phat"="n")



probs_all<-probs %>% 
  filter(Dataset=="All Countries") %>%
  select(-Dataset) %>% 
  dplyr::rename("P_Hat_All"="phat")

Table2<-left_join(Table2,probs_all)

probs_without<-probs %>% 
  filter(Dataset=="Without China & USA") %>%
  select(-Dataset) %>% 
  dplyr::rename("P_Hat_no"="phat")

Table2<-left_join(Table2,probs_without)

return(Table2)

}
