DivRichCalcTable_Solo<-function(one_author_pubs,
                                          one_author_pubsNOCHNUSA,
                                          SubsampledPW.results_Solo,
                                          SubsampledPW.results_Solo_NoUSACHN){
  
  
  # browser()
  
  # DataSet1<-one_author_pubs
  # DataSet2<-one_author_pubsNOCHNUSA
  vars<-list(one_author_pubs,
             one_author_pubsNOCHNUSA,
             SubsampledPW.results_Solo[1],
             SubsampledPW.results_Solo_NoUSACHN[1])
             
  # browser()
  # 
  Original_data_All<-as.data.frame(vars[1])
  Original_data_noCHNorUSA<-as.data.frame(vars[2])
  SampledData_All<-as.data.frame(vars[3])
  SampledData_NoUSACHN<-as.data.frame(vars[4])
  
  
  
  library(vegan)
  library(reshape)
  library(tidyr)
  library(dplyr)
  
  ##################################################
  # OA STATS _ALL COUNTRIES
  
  source("./Rscript/functions/DivRichCalc.R") 
  
  AuPosition<-"author_first"
  JrnlType<-"OA"
  Div_OA_pool_first<-DivRichCalc(Original_data_All,AuPosition,JrnlType)
  
  # Binding together
  DivMetricsPubsPooled_OA <- as.data.frame(cbind(Div_OA_pool_first[1],
                                                 Div_OA_pool_first[2]))
  
  
  
  DivMetricsPubsPooled_OA<-DivMetricsPubsPooled_OA %>% 
    dplyr::rename(OA_richness_solo=V1,
                  OA_invSimp_solo=V2)
                  
  DivMetricsPubsPooled_OA<-as.data.frame(DivMetricsPubsPooled_OA)
  DivMetricsPubsPooled_OA
  
  # Alpha for percentile confidence intervals see
  # https://cran.r-project.org/web/packages/broom/vignettes/bootstrapping.html
  alpha <- .05
  
  
  #######################################################################
  ##################################################
  # OA STATS _noChina or usa
  
  source("./Rscript/functions/DivRichCalc.R") 
  AuPosition<-"author_first"
  JrnlType<-"OA"
  Div_OA_pool_first_noCHNorUSA<-DivRichCalc(one_author_pubsNOCHNUSA,AuPosition,JrnlType)
  
  # Binding together
  DivMetricsPubsPooled_OA_noCHNorUSA <- as.data.frame(cbind(Div_OA_pool_first_noCHNorUSA[1],
                                                            Div_OA_pool_first_noCHNorUSA[2]))
  
  DivMetricsPubsPooled_OA_noCHNorUSA<-DivMetricsPubsPooled_OA_noCHNorUSA %>% 
    dplyr::rename(OA_richness_first=V1,
                  OA_invSimp_first=V2)
  DivMetricsPubsPooled_OA_noCHNorUSA<-as.data.frame(DivMetricsPubsPooled_OA_noCHNorUSA)
  DivMetricsPubsPooled_OA_noCHNorUSA
  
  
  
  
  
  
  #######################################################################
  
  PW_sampled<-SampledData_All
  colnames(PW_sampled)
  PW_Richness<-PW_sampled %>% group_by(author) %>% 
    summarize(PW_avg_richness=mean(Richness),
              SD_avg_richness=sd(Richness),
              count = n(),
              CIlow=quantile(Richness, alpha/2),
              CIhigh=quantile(Richness, 1-alpha/2))
  
  PW_Richness$SD_avg_richness<-round(PW_Richness$SD_avg_richness,1)
  PW_Richness$PW_avg_richness<-round(PW_Richness$PW_avg_richness,1)
  PW_Richness$CIlow<-round(PW_Richness$CIlow,1)
  PW_Richness$CIhigh<-round(PW_Richness$CIhigh,1)
  
  PW_Richness$author<-gsub("author_","",PW_Richness$author)
  PW_Richness$PW_avg_richness<-paste(PW_Richness$PW_avg_richness,
                                     PW_Richness$SD_avg_richness,
                                     sep="+/-")
  PW_Richness$CIs<-paste(PW_Richness$CIlow,
                         PW_Richness$CIhigh,
                         sep="-")
  PW_Richness$metric<-"Richness"
  PW_Richness$author <-as.factor(PW_Richness$author)
  PW_Richness$author <- ordered(PW_Richness$author, levels = c("first", "last","all"))
  PW_Richness<-PW_Richness %>% 
    select(author,metric,PW_AllCountries=PW_avg_richness,CIs_AllCountries=CIs,CIlow,CIhigh) %>% 
    arrange((author))
  PW_Richness
  
  PW_Diversity<-PW_sampled %>% group_by(author) %>% 
    summarize(PW_avg_richness=mean(InvSimp),
              SD_avg_richness=sd(InvSimp),
              count = n(),
              CIlow=quantile(InvSimp, alpha/2),
              CIhigh=quantile(InvSimp, 1-alpha/2))
  PW_Diversity$SD_avg_richness<-round(PW_Diversity$SD_avg_richness,1)
  PW_Diversity$PW_avg_richness<-round(PW_Diversity$PW_avg_richness,1)
  PW_Diversity$CIlow<-round(PW_Diversity$CIlow,1)
  PW_Diversity$CIhigh<-round(PW_Diversity$CIhigh,1)
  
  PW_Diversity$author<-gsub("author_","",PW_Diversity$author)
  PW_Diversity$PW_avg_richness<-paste(PW_Diversity$PW_avg_richness,
                                      PW_Diversity$SD_avg_richness,
                                      sep="+/-")
  PW_Diversity$CIs<-paste(PW_Diversity$CIlow,
                          PW_Diversity$CIhigh,
                          sep="-")
  
  PW_Diversity$metric<-"Diversity"
  PW_Diversity$author <-as.factor(PW_Diversity$author)
  PW_Diversity$author <- ordered(PW_Diversity$author, levels = c("first", "last","all"))
  PW_Diversity<-PW_Diversity %>% 
    select(author,metric,PW_AllCountries=PW_avg_richness,CIs_AllCountries=CIs,CIlow,CIhigh) %>% 
    arrange((author))
  PW_Diversity
  
  #######################################################################
  PW_sampled_noUSACHN<-SampledData_NoUSACHN
  colnames(PW_sampled_noUSACHN)
  PW_Richness_noUSACHN<-PW_sampled_noUSACHN %>% group_by(author) %>% 
    summarize(PW_avg_richness=mean(Richness),
              SD_avg_richness=sd(Richness),
              count = n(),
              CIlow=quantile(Richness, alpha/2),
              CIhigh=quantile(Richness, 1-alpha/2))
  PW_Richness_noUSACHN$SD_avg_richness<-round(PW_Richness_noUSACHN$SD_avg_richness,1)
  PW_Richness_noUSACHN$PW_avg_richness<-round(PW_Richness_noUSACHN$PW_avg_richness,1)
  PW_Richness_noUSACHN$CIlow<-round(PW_Richness_noUSACHN$CIlow,1)
  PW_Richness_noUSACHN$CIhigh<-round(PW_Richness_noUSACHN$CIhigh,1)
  
  PW_Richness_noUSACHN$author<-gsub("author_","",PW_Richness_noUSACHN$author)
  PW_Richness_noUSACHN$PW_avg_richness<-paste(PW_Richness_noUSACHN$PW_avg_richness,
                                              PW_Richness_noUSACHN$SD_avg_richness,
                                              sep="+/-")
  PW_Richness_noUSACHN$CIs<-paste(PW_Richness_noUSACHN$CIlow,
                                  PW_Richness_noUSACHN$CIhigh,
                                  sep="-")
  
  
  
  PW_Richness_noUSACHN$metric<-"Richness"
  PW_Richness_noUSACHN$author <-as.factor(PW_Richness_noUSACHN$author)
  PW_Richness_noUSACHN$author <- ordered(PW_Richness_noUSACHN$author, levels = c("first", "last","all"))
  PW_Richness_noUSACHN<-PW_Richness_noUSACHN %>% 
    select(author,metric,PW_noUSAorCHN=PW_avg_richness,CIs_noUSAorCHN=CIs,CIlow,CIhigh) %>% 
    arrange((author))
  PW_Richness_noUSACHN
  
  PW_Diversity_noUSACHN<-PW_sampled_noUSACHN %>% group_by(author) %>% 
    summarize(PW_avg_richness=mean(InvSimp),
              SD_avg_richness=sd(InvSimp),
              count = n(),
              CIlow=quantile(InvSimp, alpha/2),
              CIhigh=quantile(InvSimp, 1-alpha/2))
  PW_Diversity_noUSACHN$SD_avg_richness<-round(PW_Diversity_noUSACHN$SD_avg_richness,1)
  PW_Diversity_noUSACHN$PW_avg_richness<-round(PW_Diversity_noUSACHN$PW_avg_richness,1)
  PW_Diversity_noUSACHN$CIlow<-round(PW_Diversity_noUSACHN$CIlow,1)
  PW_Diversity_noUSACHN$CIhigh<-round(PW_Diversity_noUSACHN$CIhigh,1)
  
  
  PW_Diversity_noUSACHN$author<-gsub("author_","",PW_Diversity_noUSACHN$author)
    PW_Diversity_noUSACHN$PW_avg_richness<-paste(PW_Diversity_noUSACHN$PW_avg_richness,
                                               PW_Diversity_noUSACHN$SD_avg_richness,
                                               sep=" +/- ")
  
  PW_Diversity_noUSACHN$CIs<-paste(PW_Diversity_noUSACHN$CIlow,
                                   PW_Diversity_noUSACHN$CIhigh,
                                   sep="-")
  
  PW_Diversity_noUSACHN$metric<-"Diversity"
  PW_Diversity_noUSACHN$author <-as.factor(PW_Diversity_noUSACHN$author)
  PW_Diversity_noUSACHN$author <- ordered(PW_Diversity_noUSACHN$author, levels = c("first", "last","all"))
  PW_Diversity_noUSACHN<-PW_Diversity_noUSACHN %>% 
    select(author,metric,PW_noUSAorCHN=PW_avg_richness,CIs_noUSAorCHN=CIs,CIlow,CIhigh) %>% 
    arrange((author))
  
  PW_Stats_ALL<-bind_rows(PW_Richness,PW_Diversity)
  PW_Stats_noUSA_CHN<-bind_rows(PW_Richness_noUSACHN,PW_Diversity_noUSACHN)
  PW_Stats_ALL<-bind_cols(PW_Stats_ALL,PW_Stats_noUSA_CHN)
  
  DivMetricsPubsPooled_OA<-unlist(DivMetricsPubsPooled_OA)
  DivMetricsPubsPooled_OA<-round(DivMetricsPubsPooled_OA,1)
  PW_Stats_ALL$OA_AllCountries<-DivMetricsPubsPooled_OA
  
  DivMetricsPubsPooled_OA_noCHNorUSA<-unlist(DivMetricsPubsPooled_OA_noCHNorUSA)
  DivMetricsPubsPooled_OA_noCHNorUSA<-round(DivMetricsPubsPooled_OA_noCHNorUSA,1)
  PW_Stats_ALL$OA_noCHNorUSA<-DivMetricsPubsPooled_OA_noCHNorUSA
  RichDiv_Stats_ALL <- PW_Stats_ALL %>% 
    select(author,metric,OA_AllCountries,PW_AllCountries,CIs_AllCountries,CIlow,CIhigh,OA_noCHNorUSA,PW_noUSAorCHN,CIs_noUSAorCHN,CIlow1,CIhigh1)
  colnames(RichDiv_Stats_ALL)
  RichDiv_Stats_ALL$author<-"solo"
  
  #######################################
  # put together in a a Table 
  #######################################
  
  return(RichDiv_Stats_ALL)
  
  
}