DivRichCalcSummaryTable_sampled<-function(Dataset,
                                          Dataset2,
                                          SubsampledPW.results_First,
                                          SubsampledPW.results_Last,
                                          SubsampledPW.results_All,
                                          SubsampledPW.results_First_NOUSACHN,
                                          SubsampledPW.results_Last_NOUSACHN,
                                          SubsampledPW.results_All_NOUSACHN){
  
  # browser()
  # DataSet<-coauthor_pubs
  # DataSet2<-coauthor_pubsNOCHNUSA
  vars<-list(DataSet,
             DataSet2,
             SubsampledPW.results_First[1],
             SubsampledPW.results_Last[1],
             SubsampledPW.results_All[1],
             SubsampledPW.results_First_NOUSACHN[1],
             SubsampledPW.results_Last_NOUSACHN[1],
             SubsampledPW.results_All_NOUSACHN[1])
  # browser()
  # 
  DataSet<-as.data.frame(vars[1])
  DataSet2<-as.data.frame(vars[2])
  SubPW_First<-as.data.frame(vars[3])
  SubPW_Last<-as.data.frame(vars[4])
  SubPW_All<-as.data.frame(vars[5])
  SubPW_First_NOUSACHN<-as.data.frame(vars[6])
  SubPW_Last_NOUSACHN<-as.data.frame(vars[7])
  SubPW_All_NOUSACHN<-as.data.frame(vars[8])
  
  
  library(vegan)
  library(reshape)
  library(tidyr)
  library(dplyr)
  

  ##################################################
  # OA STATS _ALL COUNTRIES
  
  source("./Rscript/functions/DivRichCalc.R") 

  AuPosition<-"author_first"
  JrnlType<-"OA"
  Div_OA_pool_first<-DivRichCalc(DataSet,AuPosition,JrnlType)
  
  AuPosition<-"author_last"
  JrnlType<-"OA"
  Div_OA_pool_last<-DivRichCalc(DataSet,AuPosition,JrnlType)
  
  AuPosition<-"author_all"
  JrnlType<-"OA"
  Div_OA_pool_all<-DivRichCalc(DataSet,AuPosition,JrnlType)
  
  # Binding together
  DivMetricsPubsPooled_OA <- as.data.frame(cbind(Div_OA_pool_first[1],
                                                 Div_OA_pool_last[1],
                                                 Div_OA_pool_all[1],
                                                 Div_OA_pool_first[2],
                                                 Div_OA_pool_last[2],
                                                 Div_OA_pool_all[2]))
  
  DivMetricsPubsPooled_OA<-DivMetricsPubsPooled_OA %>% 
    dplyr::rename(OA_richness_first=V1,
                  OA_richness_last=V2,
                  OA_richness_all=V3,
                  OA_invSimp_first=V4,
                  OA_invSimp_last=V5,
                  OA_invSimp_all=V6)
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
  Div_OA_pool_first_noCHNorUSA<-DivRichCalc(DataSet2,AuPosition,JrnlType)
  
  AuPosition<-"author_last"
  JrnlType<-"OA"
  Div_OA_pool_last_noCHNorUSA<-DivRichCalc(DataSet2,AuPosition,JrnlType)
  
  AuPosition<-"author_all"
  JrnlType<-"OA"
  Div_OA_pool_all_noCHNorUSA<-DivRichCalc(DataSet2,AuPosition,JrnlType)
  
  # Binding together
  DivMetricsPubsPooled_OA_noCHNorUSA <- as.data.frame(cbind(Div_OA_pool_first_noCHNorUSA[1],
                                                 Div_OA_pool_last_noCHNorUSA[1],
                                                 Div_OA_pool_all_noCHNorUSA[1],
                                                 Div_OA_pool_first_noCHNorUSA[2],
                                                 Div_OA_pool_last_noCHNorUSA[2],
                                                 Div_OA_pool_all_noCHNorUSA[2]))
  
  DivMetricsPubsPooled_OA_noCHNorUSA<-DivMetricsPubsPooled_OA_noCHNorUSA %>% 
    dplyr::rename(OA_richness_first=V1,
                  OA_richness_last=V2,
                  OA_richness_all=V3,
                  OA_invSimp_first=V4,
                  OA_invSimp_last=V5,
                  OA_invSimp_all=V6)
  DivMetricsPubsPooled_OA_noCHNorUSA<-as.data.frame(DivMetricsPubsPooled_OA_noCHNorUSA)
  DivMetricsPubsPooled_OA_noCHNorUSA
  #######################################################################
  
  #######################################################################
# WHY DIDN';T THE CALC OF CI WORK USING FORMULA?
    # #Function to calculate confidence intervals
  # z_star_95 <- qnorm(0.975)
  # 
  # lower_ci <- function(mean, sd, n, z_star_95){
  #   lower_ci <- mean - z_star_95 * (sd / sqrt(n))
  # }
  # upper_ci <- function(mean, sd, n, z_star_95){
  #   upper_ci <- mean + z_star_95 * (sd / sqrt(n))
  # }
  # 
  # # mutate(lower_ci = lower_ci(PW_avg_richness, SD_avg_richness, count,z_star_95),
  # #        upper_ci = upper_ci(SD_avg_richness, SD_avg_richness, count,z_star_95))
  # https://rpubs.com/maulikpatel/212123
  #######################################################################
  
  PW_sampled<-bind_rows(SubPW_First,
            SubPW_Last,
            SubPW_All)
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
  PW_sampled_noUSACHN<-bind_rows(SubPW_First_NOUSACHN,
                        SubPW_Last_NOUSACHN,
                        SubPW_All_NOUSACHN)
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
  
  
  
  #######################################
  # put together in a a Table 
  #######################################
  
  return(RichDiv_Stats_ALL)
  
  
}