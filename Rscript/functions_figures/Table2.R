Table2<-function(sole_author_pubs_ALL_first_author,sole_author_pubsNOCHNUSA_first_author,
                 coauthor_pubs_ALL_first_author,coauthor_pubsNOCHNUSA_first_author){
  vars<-list(sole_author_pubs_ALL_first_author,
             sole_author_pubsNOCHNUSA_first_author,
             coauthor_pubs_ALL_first_author,
             coauthor_pubsNOCHNUSA_first_author)
source("./Rscript/functions/DivRichCalcTable_Solo.R")
source("./Rscript/functions/DivRichCalcSummaryTable_sampled.R")


SubsampledPW.results_Solo<-read_csv('output/SubsampledPW.results_RichDiv_SOLO_ALL.csv')
SubsampledPW.results_Solo_NoUSACHN<-read_csv('output/SubsampledPW.results_RichDiv_SOLO_ALL.csv')
SubsampledPW.results_First<-read_csv('output/SubsampledPW.results_RichDiv_CO_ALL.csv')
SubsampledPW.results_First_NOUSACHN<-read_csv('output/SubsampledPW.results_RichDiv_CO_NOUSACHN.csv')
# SubsampledPW.results_Last<-read_csv('output/SubsampledPW.results_RichDiv_LAST_AUTHOR.csv')
# SubsampledPW.results_All<-read_csv('output/SubsampledPW.results_RichDiv_ALL_AUTHOR.csv')

# SubsampledPW.results_Last_NOUSACHN<-read_csv('output/SubsampledPW.results_RichDiv_LAST_AUTHOR_NOUSACHN.csv')
# SubsampledPW.results_All_NOUSACHN<-read_csv('output/SubsampledPW.results_RichDiv_ALL_AUTHOR_NOUSACHN.csv')


Table2_Solo<-DivRichCalcTable_Solo(as.data.frame(vars[1]),
                                   as.data.frame(vars[2]),
                                   SubsampledPW.results_Solo,
                                   SubsampledPW.results_Solo_NoUSACHN)


Table2_CoAuthored<-DivRichCalcSummaryTable_sampled(as.data.frame(vars[3]),
                                                   as.data.frame(vars[4]),
                                                   SubsampledPW.results_First,
                                                   # SubsampledPW.results_Last,
                                                   # SubsampledPW.results_All,
                                                   SubsampledPW.results_First_NOUSACHN)
                                                   # SubsampledPW.results_Last_NOUSACHN,
                                                   # SubsampledPW.results_All_NOUSACHN)


 
  #######################################
  # put together in a a Table 
  #######################################

Table2_Joint<-bind_rows(Table2_CoAuthored,Table2_Solo)
Table2_Joint$author <- ordered(Table2_Joint$author, levels = c("solo","first", "last","all"))
Table2_Joint<-Table2_Joint %>% 
  select(-CIlow,-CIhigh,-CIlow1,-CIhigh1) %>% 
  filter(author!="all") %>% 
  arrange(desc(metric),author)
  
Table2_Joint$PW_AllCountries<-as.character(Table2_Joint$PW_AllCountries)
Table2_Joint$PW_noUSAorCHN<-as.character(Table2_Joint$PW_noUSAorCHN)
Table2_Joint$PW_AllCountries<-str_replace(Table2_Joint$PW_AllCountries, "[+]", "\u00B1")
Table2_Joint$PW_AllCountries<-str_replace(Table2_Joint$PW_AllCountries, "[/]", "")
Table2_Joint$PW_AllCountries<-str_replace(Table2_Joint$PW_AllCountries, "[-]", "")
Table2_Joint$PW_noUSAorCHN<-str_replace(Table2_Joint$PW_noUSAorCHN, "[+]", "\u00B1")
Table2_Joint$PW_noUSAorCHN<-str_replace(Table2_Joint$PW_noUSAorCHN, "[/]", "")
Table2_Joint$PW_noUSAorCHN<-str_replace(Table2_Joint$PW_noUSAorCHN, "[-]", "")
Table2_Joint$author<-str_to_title(Table2_Joint$author)
Table2_Joint$author<-gsub("Solo","Single",Table2_Joint$author)
names(Table2_Joint)<-c("Author","Metric","OA (All Countries)","Mean PW (All Countries)",
                       "PW 95% CI (All Countries)", "OA (USA & CHN excluded)",
                       "Mean PW (USA & CHN excluded)","PW 95% CI (USA & CHN excluded)")
Table2_Joint <- Table2_Joint %>% select("Metric","Author","OA (All Countries)",
                                        "Mean PW (All Countries)","PW 95% CI (All Countries)",
                                        "OA (USA & CHN excluded)","Mean PW (USA & CHN excluded)",
                                        "PW 95% CI (USA & CHN excluded)")
Table2_Joint$Metric <-c("Richness","","Diversity","")
return(Table2_Joint)

}
