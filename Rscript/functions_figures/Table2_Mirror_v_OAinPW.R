Table2_Mirror_v_OAinPW<-function(AllData,
                                 BootPW_SoloAll,
                                 BootPW_SoloNoUSACHN,
                                 BootPW_CoAll,
                                 BootPW_CoNoUSACHN,
                                 ArticleType){

  library(tidyverse)
  # Need to generate subsets of sole-author pubs, coauthor pubs 
  sole_ALL_OAinPW <- AllData %>%
    filter(author=="solo") %>% 
    filter(ArticleType=="OA") %>% 
    filter(JrnlType=="PW") %>% 
    mutate(Dataset="All Countries") %>% 
    drop_na("Code")
  # n_distinct(sole_author_pubs_ALL$refID)
  
  first_ALL_OAinPW<- AllData %>%
    filter(author=="coauthored") %>% 
    filter(ArticleType=="OA") %>% 
    filter(JrnlType=="PW") %>% 
    filter(AuthorNum==1) %>% 
    mutate(author="author_first") 
  
  levels(as.factor(sole_NOCHNUSA_OAinPW$First_Author_Country))
  
  # subset of data with no first authors form CHN or USA
  sole_NOCHNUSA_OAinPW <- AllData %>%
    filter(author=="solo") %>%
    filter(ArticleType=="OA") %>% 
    filter(JrnlType=="PW") %>% 
    filter(Code!="CHN") %>% 
    filter(Code!="USA") %>% 
    mutate(Dataset="CHN & USA excluded")
  
  first_NOCHNUSA_OAinPW<- AllData %>%
    filter(author=="coauthored") %>% 
    filter(AuthorNum==1) %>% 
    filter(ArticleType=="OA") %>% 
    filter(JrnlType=="PW") %>% 
    filter(Code!="CHN") %>% 
    filter(Code!="USA") %>% 
    mutate(Dataset="CHN & USA excluded") %>% 
    mutate(author="author_first")
  
  #   
  
  
  library(vegan)
  library(reshape)
  library(tidyr)
  library(dplyr)
  
  # FOR THESE
  ArticleType<-"OA"
  JrnlType<-"OA"
  # Mirror OA STATS - SOLO-ALL COUNTRIES
  source("./Rscript/functions/DivRichCalc.R") 
  sole_ALL_Mirror <- AllData %>%
    filter(author=="solo") %>% 
    filter(JrnlType=="OA") %>% 
    mutate(Dataset="All Countries") %>% 
    drop_na("Code")
  AuPosition<-"author_first"

  Div_OA_solo<-DivRichCalc(sole_ALL_Mirror,AuPosition,JrnlType,ArticleType)
  
  # Binding together
  DivMetrics_Mirror_solo <- as.data.frame(cbind(Div_OA_solo[1],
                                                Div_OA_solo[2]))
  DivMetrics_Mirror_solo<-DivMetrics_Mirror_solo %>% 
    dplyr::rename(OA_richness_solo=V1,
                  OA_invSimp_solo=V2)
  
  # Mirror OA STATS - SOLO-NO CHN USA
  sole_NOCHNUSA_Mirror <- AllData %>%
    filter(author=="solo") %>%
    filter(JrnlType=="OA") %>% 
    filter(Code!="CHN") %>% 
    filter(Code!="USA") %>% 
    mutate(Dataset="CHN & USA excluded")
  
  AuPosition<-"author_first"
  Div_OA_solo_noCHNUSA<-DivRichCalc(sole_NOCHNUSA_Mirror,AuPosition,JrnlType,ArticleType)
  
  # Binding together
  DivMetrics_Mirror_solo_noCHNUSA <- as.data.frame(cbind(Div_OA_solo_noCHNUSA[1],
                                                         Div_OA_solo_noCHNUSA[2]))
  DivMetrics_Mirror_solo_noCHNUSA<-DivMetrics_Mirror_solo_noCHNUSA %>% 
    dplyr::rename(OA_richness_solo=V1,
                  OA_invSimp_solo=V2)
  
  
  
  #####################################
  #####################################
  
  first_ALL_Mirror<- AllData %>%
    filter(author=="coauthored") %>% 
    filter(ArticleType=="OA") %>% 
    filter(JrnlType=="OA") %>% 
    filter(AuthorNum==1) %>% 
    mutate(author="author_first") 
  
  
  Div_OA_mirror_first<-DivRichCalc(first_ALL_Mirror,AuPosition,JrnlType,ArticleType)
  
  DivMetrics_mirror_first <- as.data.frame(cbind(Div_OA_mirror_first[1],
                                                 Div_OA_mirror_first[2]))
  
  DivMetrics_mirror_CO<-DivMetrics_mirror_first %>% 
    dplyr::rename(MirrorOA_richness_first=V1,
                  MirrorOA_invSimp_first=V2)
  
  # OA STATS mirror - noChina or usa
  
  first_NOCHNUSA_Mirror<- AllData %>%
    filter(author=="coauthored") %>% 
    filter(AuthorNum==1) %>% 
    filter(ArticleType=="OA") %>% 
    filter(JrnlType=="OA") %>% 
    filter(Code!="CHN") %>% 
    filter(Code!="USA") %>% 
    mutate(Dataset="CHN & USA excluded") %>% 
    mutate(author="author_first")
  Div_OA_mirror_noCHNorUSA<-DivRichCalc(first_NOCHNUSA_Mirror,"author_first",JrnlType,ArticleType)
  
  DivMetrics_mirror_noCHNorUSA <- as.data.frame(cbind(Div_OA_mirror_noCHNorUSA[1],
                                                      Div_OA_mirror_noCHNorUSA[2]))
  
  DivMetrics_mirror_COnoCHNorUSA<-DivMetrics_mirror_noCHNorUSA %>% 
    dplyr::rename(MirrorOA_richness_first=V1,
                  MirrorOA_invSimp_first=V2)
  
  #################################
  DivMetrics_Mirror_solo
  DivMetrics_Mirror_solo_noCHNUSA
  DivMetrics_mirror_CO
  DivMetrics_mirror_COnoCHNorUSA
  #################################
  
  #######################################################################
  # Alpha for percentile confidence intervals see
  # https://cran.r-project.org/web/packages/broom/vignettes/bootstrapping.html
  alpha <- .05
  
  #######################################################################
  # Bootstrap of OAinPW - Solo All Countries
  OAinPW_sampled<-as.data.frame(BootPW_SoloAll[1])
  colnames(OAinPW_sampled)
  OAinPW_Richness<-OAinPW_sampled %>% group_by(author) %>% 
    summarize(OAinPW_avg_richness=mean(Richness),
              SD_avg_richness=sd(Richness),
              count = n(),
              CIlow=quantile(Richness, alpha/2),
              CIhigh=quantile(Richness, 1-alpha/2))
  
  OAinPW_Richness$SD_avg_richness<-round(OAinPW_Richness$SD_avg_richness,1)
  OAinPW_Richness$OAinPW_avg_richness<-round(OAinPW_Richness$OAinPW_avg_richness,1)
  OAinPW_Richness$CIlow<-round(OAinPW_Richness$CIlow,1)
  OAinPW_Richness$CIhigh<-round(OAinPW_Richness$CIhigh,1)
  
  OAinPW_Richness$author<-gsub("author_","",OAinPW_Richness$author)
  OAinPW_Richness$OAinPW_avg_richness<-paste(OAinPW_Richness$OAinPW_avg_richness,
                                             OAinPW_Richness$SD_avg_richness,
                                             sep="+/-")
  OAinPW_Richness$CIs<-paste(OAinPW_Richness$CIlow,
                             OAinPW_Richness$CIhigh,
                             sep="-")
  OAinPW_Richness$metric<-"Richness"
  OAinPW_Richness$author <-as.factor(OAinPW_Richness$author)
  # OAinPW_Richness$author <- ordered(OAinPW_Richness$author, levels = c("first", "last","all"))
  OAinPW_Richness<-OAinPW_Richness %>% 
    select(author,metric,OAinPW_AllCountries=OAinPW_avg_richness,CIs_AllCountries=CIs,CIlow,CIhigh) %>% 
    arrange((author))
  OAinPW_Richness
  
  OAinPW_Diversity<-OAinPW_sampled %>% group_by(author) %>% 
    summarize(OAinPW_avg_richness=mean(InvSimp),
              SD_avg_richness=sd(InvSimp),
              count = n(),
              CIlow=quantile(InvSimp, alpha/2),
              CIhigh=quantile(InvSimp, 1-alpha/2))
  OAinPW_Diversity$SD_avg_richness<-round(OAinPW_Diversity$SD_avg_richness,1)
  OAinPW_Diversity$OAinPW_avg_richness<-round(OAinPW_Diversity$OAinPW_avg_richness,1)
  OAinPW_Diversity$CIlow<-round(OAinPW_Diversity$CIlow,1)
  OAinPW_Diversity$CIhigh<-round(OAinPW_Diversity$CIhigh,1)
  
  OAinPW_Diversity$author<-gsub("author_","",OAinPW_Diversity$author)
  OAinPW_Diversity$OAinPW_avg_richness<-paste(OAinPW_Diversity$OAinPW_avg_richness,
                                              OAinPW_Diversity$SD_avg_richness,
                                              sep="+/-")
  OAinPW_Diversity$CIs<-paste(OAinPW_Diversity$CIlow,
                              OAinPW_Diversity$CIhigh,
                              sep="-")
  
  OAinPW_Diversity$metric<-"Diversity"
  OAinPW_Diversity$author <-as.factor(OAinPW_Diversity$author)
  # OAinPW_Diversity$author <- ordered(OAinPW_Diversity$author, levels = c("first", "last","all"))
  OAinPW_Diversity<-OAinPW_Diversity %>% 
    select(author,metric,OAinPW_AllCountries=OAinPW_avg_richness,CIs_AllCountries=CIs,CIlow,CIhigh) %>% 
    arrange(author)
  OAinPW_Diversity
  
  OAinPW_Diversity
  OAinPW_Richness
  #######################################################################
  # Bootstrap of OAinOAinPW - Solo No USA or CHN
  OAinPW_sampled_noUSACHN<-as.data.frame(BootPW_SoloNoUSACHN[1])
  colnames(OAinPW_sampled_noUSACHN)
  OAinPW_Richness_noUSACHN<-OAinPW_sampled_noUSACHN %>% group_by(author) %>% 
    summarize(OAinPW_avg_richness=mean(Richness),
              SD_avg_richness=sd(Richness),
              count = n(),
              CIlow=quantile(Richness, alpha/2),
              CIhigh=quantile(Richness, 1-alpha/2))
  OAinPW_Richness_noUSACHN$SD_avg_richness<-round(OAinPW_Richness_noUSACHN$SD_avg_richness,1)
  OAinPW_Richness_noUSACHN$OAinPW_avg_richness<-round(OAinPW_Richness_noUSACHN$OAinPW_avg_richness,1)
  OAinPW_Richness_noUSACHN$CIlow<-round(OAinPW_Richness_noUSACHN$CIlow,1)
  OAinPW_Richness_noUSACHN$CIhigh<-round(OAinPW_Richness_noUSACHN$CIhigh,1)
  
  OAinPW_Richness_noUSACHN$author<-gsub("author_","",OAinPW_Richness_noUSACHN$author)
  OAinPW_Richness_noUSACHN$OAinPW_avg_richness<-paste(OAinPW_Richness_noUSACHN$OAinPW_avg_richness,
                                                      OAinPW_Richness_noUSACHN$SD_avg_richness,
                                                      sep="+/-")
  OAinPW_Richness_noUSACHN$CIs<-paste(OAinPW_Richness_noUSACHN$CIlow,
                                      OAinPW_Richness_noUSACHN$CIhigh,
                                      sep="-")
  
  
  
  OAinPW_Richness_noUSACHN$metric<-"Richness"
  OAinPW_Richness_noUSACHN$author <-as.factor(OAinPW_Richness_noUSACHN$author)
  # OAinPW_Richness_noUSACHN$author <- ordered(OAinPW_Richness_noUSACHN$author, levels = c("first", "last","all"))
  OAinPW_Richness_noUSACHN<-OAinPW_Richness_noUSACHN %>% 
    select(author,metric,OAinPW_noUSAorCHN=OAinPW_avg_richness,CIs_noUSAorCHN=CIs,CIlow,CIhigh) %>% 
    arrange(author)
  OAinPW_Richness_noUSACHN
  
  OAinPW_Diversity_noUSACHN<-OAinPW_sampled_noUSACHN %>% group_by(author) %>% 
    summarize(OAinPW_avg_richness=mean(InvSimp),
              SD_avg_richness=sd(InvSimp),
              count = n(),
              CIlow=quantile(InvSimp, alpha/2),
              CIhigh=quantile(InvSimp, 1-alpha/2))
  
  # boot_out<-as.matrix(boot_out)
  # d<-quantile(boot_out, c(0.025, 0.975))
  # xbar<-mean(boot_out)
  # ci = xbar - c(d[2], d[1])
  # cat('Confidence interval: ',ci, '\n')
  # 
  OAinPW_Diversity_noUSACHN$SD_avg_richness<-round(OAinPW_Diversity_noUSACHN$SD_avg_richness,1)
  OAinPW_Diversity_noUSACHN$OAinPW_avg_richness<-round(OAinPW_Diversity_noUSACHN$OAinPW_avg_richness,1)
  OAinPW_Diversity_noUSACHN$CIlow<-round(OAinPW_Diversity_noUSACHN$CIlow,1)
  OAinPW_Diversity_noUSACHN$CIhigh<-round(OAinPW_Diversity_noUSACHN$CIhigh,1)
  
  
  OAinPW_Diversity_noUSACHN$author<-gsub("author_","",OAinPW_Diversity_noUSACHN$author)
  OAinPW_Diversity_noUSACHN$OAinPW_avg_richness<-paste(OAinPW_Diversity_noUSACHN$OAinPW_avg_richness,
                                                       OAinPW_Diversity_noUSACHN$SD_avg_richness,
                                                       sep=" +/- ")
  
  OAinPW_Diversity_noUSACHN$CIs<-paste(OAinPW_Diversity_noUSACHN$CIlow,
                                       OAinPW_Diversity_noUSACHN$CIhigh,
                                       sep="-")
  
  OAinPW_Diversity_noUSACHN$metric<-"Diversity"
  OAinPW_Diversity_noUSACHN$author <-as.factor(OAinPW_Diversity_noUSACHN$author)
  # OAinPW_Diversity_noUSACHN$author <- ordered(OAinPW_Diversity_noUSACHN$author, levels = c("first", "last","all"))
  OAinPW_Diversity_noUSACHN<-OAinPW_Diversity_noUSACHN %>% 
    select(author,metric,OAinPW_noUSAorCHN=OAinPW_avg_richness,CIs_noUSAorCHN=CIs,CIlow,CIhigh) %>% 
    arrange(author)
  
  
  
  ##########################
  OAinPW_Diversity
  OAinPW_Richness
  OAinPW_Diversity_noUSACHN
  OAinPW_Richness_noUSACHN
  ##########################
  
  
  OAinPW_Stats_ALL<-bind_rows(OAinPW_Richness,OAinPW_Diversity)
  OAinPW_Stats_noUSA_CHN<-bind_rows(OAinPW_Richness_noUSACHN,OAinPW_Diversity_noUSACHN)
  OAinPW_Stats_ALL<-bind_cols(OAinPW_Stats_ALL,OAinPW_Stats_noUSA_CHN)
  
  
  
  #################################
  DivMetrics_Mirror_solo
  DivMetrics_Mirror_solo_noCHNUSA
  DivMetrics_mirror_CO
  DivMetrics_mirror_COnoCHNorUSA
  #################################
  
  DivMetricsPubsPooled_OA<-unlist(DivMetrics_Mirror_solo)
  DivMetricsPubsPooled_OA<-round(DivMetricsPubsPooled_OA,1)
  OAinPW_Stats_ALL$OA_AllCountries<-DivMetricsPubsPooled_OA
  
  DivMetricsPubsPooled_OA_noCHNorUSA<-unlist(DivMetrics_Mirror_solo_noCHNUSA)
  DivMetricsPubsPooled_OA_noCHNorUSA<-round(DivMetricsPubsPooled_OA_noCHNorUSA,1)
  OAinPW_Stats_ALL$OA_noCHNorUSA<-DivMetricsPubsPooled_OA_noCHNorUSA
  RichDiv_Stats_ALL <- OAinPW_Stats_ALL %>% 
    select(author=author...1,metric=metric...2,OA_AllCountries,
           OAinPW_AllCountries,CIs_AllCountries,CIlow=CIlow...5,CIhigh=CIhigh...6,
           OA_noCHNorUSA,OAinPW_noUSAorCHN,CIs_noUSAorCHN,CIlow1=CIlow...11,CIhigh1=CIhigh...12)
  colnames(RichDiv_Stats_ALL)
  RichDiv_Stats_ALL$author<-"solo"
  RichDiv_Stats_SOLO<-RichDiv_Stats_ALL
  #############################################
  
  ##################################################
  # Mirror OA STATS - CO-ALL COUNTRIES
  
  first_ALL_Mirror<- AllData %>%
    filter(author=="coauthored") %>% 
    filter(ArticleType=="OA") %>% 
    filter(JrnlType=="OA") %>% 
    filter(AuthorNum==1) %>% 
    mutate(author="author_first") 
  
  Div_OA_mirror_first<-DivRichCalc(first_ALL_Mirror,AuPosition,ArticleType)
  
  DivMetrics_mirror_first <- as.data.frame(cbind(Div_OA_mirror_first[1],
                                                 Div_OA_mirror_first[2]))
  
  DivMetrics_mirror_first<-DivMetrics_mirror_first %>% 
    dplyr::rename(OA_richness_first=V1,
                  OA_invSimp_first=V2)
  
  # OA STATS mirror - noChina or usa
  
  first_NOCHNUSA_Mirror<- AllData %>%
    filter(author=="coauthored") %>% 
    filter(AuthorNum==1) %>% 
    filter(ArticleType=="OA") %>% 
    filter(JrnlType=="OA") %>% 
    filter(Code!="CHN") %>% 
    filter(Code!="USA") %>% 
    mutate(Dataset="CHN & USA excluded") %>% 
    mutate(author="author_first")
  Div_OA_mirror_noCHNorUSA<-DivRichCalc(first_NOCHNUSA_Mirror,"author_first",ArticleType)
  
  DivMetrics_mirror_noCHNorUSA <- as.data.frame(cbind(Div_OA_mirror_noCHNorUSA[1],
                                                      Div_OA_mirror_noCHNorUSA[2]))
  
  DivMetrics_mirror_first_noCHNorUSA<-DivMetrics_mirror_noCHNorUSA %>% 
    dplyr::rename(OA_richness_first=V1,
                  OA_invSimp_first=V2)
  
  #######################################################################
  # Alpha for percentile confidence intervals see
  # https://cran.r-project.org/web/packages/broom/vignettes/bootstrapping.html
  alpha <- .05
  
  #######################################################################
  
  OAinPW_sampled<-as.data.frame(BootPW_CoAll[1])
  colnames(OAinPW_sampled)
  OAinPW_Richness<-OAinPW_sampled %>% group_by(author) %>% 
    summarize(OAinPW_avg_richness=mean(Richness),
              SD_avg_richness=sd(Richness),
              count = n(),
              CIlow=quantile(Richness, alpha/2),
              CIhigh=quantile(Richness, 1-alpha/2))
  
  OAinPW_Richness$SD_avg_richness<-round(OAinPW_Richness$SD_avg_richness,1)
  OAinPW_Richness$OAinPW_avg_richness<-round(OAinPW_Richness$OAinPW_avg_richness,1)
  OAinPW_Richness$CIlow<-round(OAinPW_Richness$CIlow,1)
  OAinPW_Richness$CIhigh<-round(OAinPW_Richness$CIhigh,1)
  
  OAinPW_Richness$author<-gsub("author_","",OAinPW_Richness$author)
  OAinPW_Richness$OAinPW_avg_richness<-paste(OAinPW_Richness$OAinPW_avg_richness,
                                             OAinPW_Richness$SD_avg_richness,
                                             sep="+/-")
  OAinPW_Richness$CIs<-paste(OAinPW_Richness$CIlow,
                             OAinPW_Richness$CIhigh,
                             sep="-")
  OAinPW_Richness$metric<-"Richness"
  OAinPW_Richness$author <-as.factor(OAinPW_Richness$author)
  # OAinPW_Richness$author <- ordered(OAinPW_Richness$author, levels = c("first", "last","all"))
  OAinPW_Richness<-OAinPW_Richness %>% 
    select(author,metric,OAinPW_AllCountries=OAinPW_avg_richness,CIs_AllCountries=CIs,CIlow,CIhigh) %>% 
    arrange(author)
  OAinPW_Richness
  
  ##########
  
  
  OAinPW_Diversity<-OAinPW_sampled %>% group_by(author) %>% 
    summarize(OAinPW_avg_richness=mean(InvSimp),
              SD_avg_richness=sd(InvSimp),
              count = n(),
              CIlow=quantile(InvSimp, alpha/2),
              CIhigh=quantile(InvSimp, 1-alpha/2))
  OAinPW_Diversity$SD_avg_richness<-round(OAinPW_Diversity$SD_avg_richness,1)
  OAinPW_Diversity$OAinPW_avg_richness<-round(OAinPW_Diversity$OAinPW_avg_richness,1)
  OAinPW_Diversity$CIlow<-round(OAinPW_Diversity$CIlow,1)
  OAinPW_Diversity$CIhigh<-round(OAinPW_Diversity$CIhigh,1)
  
  OAinPW_Diversity$author<-gsub("author_","",OAinPW_Diversity$author)
  OAinPW_Diversity$OAinPW_avg_richness<-paste(OAinPW_Diversity$OAinPW_avg_richness,
                                              OAinPW_Diversity$SD_avg_richness,
                                              sep="+/-")
  OAinPW_Diversity$CIs<-paste(OAinPW_Diversity$CIlow,
                              OAinPW_Diversity$CIhigh,
                              sep="-")
  
  OAinPW_Diversity$metric<-"Diversity"
  OAinPW_Diversity$author <-as.factor(OAinPW_Diversity$author)
  # OAinPW_Diversity$author <- ordered(OAinPW_Diversity$author, levels = c("first", "last","all"))
  OAinPW_Diversity<-OAinPW_Diversity %>% 
    select(author,metric,OAinPW_AllCountries=OAinPW_avg_richness,CIs_AllCountries=CIs,CIlow,CIhigh) %>% 
    arrange(author)
  
  
  #######################################################################
  OAinPW_sampled_noUSACHN<-as.data.frame(BootPW_CoNoUSACHN[1])
  colnames(OAinPW_sampled_noUSACHN)
  OAinPW_Richness_noUSACHN<-OAinPW_sampled_noUSACHN %>% group_by(author) %>% 
    summarize(OAinPW_avg_richness=mean(Richness),
              SD_avg_richness=sd(Richness),
              count = n(),
              CIlow=quantile(Richness, alpha/2),
              CIhigh=quantile(Richness, 1-alpha/2))
  OAinPW_Richness_noUSACHN$SD_avg_richness<-round(OAinPW_Richness_noUSACHN$SD_avg_richness,1)
  OAinPW_Richness_noUSACHN$OAinPW_avg_richness<-round(OAinPW_Richness_noUSACHN$OAinPW_avg_richness,1)
  OAinPW_Richness_noUSACHN$CIlow<-round(OAinPW_Richness_noUSACHN$CIlow,1)
  OAinPW_Richness_noUSACHN$CIhigh<-round(OAinPW_Richness_noUSACHN$CIhigh,1)
  
  OAinPW_Richness_noUSACHN$author<-gsub("author_","",OAinPW_Richness_noUSACHN$author)
  OAinPW_Richness_noUSACHN$OAinPW_avg_richness<-paste(OAinPW_Richness_noUSACHN$OAinPW_avg_richness,
                                                      OAinPW_Richness_noUSACHN$SD_avg_richness,
                                                      sep="+/-")
  OAinPW_Richness_noUSACHN$CIs<-paste(OAinPW_Richness_noUSACHN$CIlow,
                                      OAinPW_Richness_noUSACHN$CIhigh,
                                      sep="-")
  
  
  
  OAinPW_Richness_noUSACHN$metric<-"Richness"
  OAinPW_Richness_noUSACHN$author <-as.factor(OAinPW_Richness_noUSACHN$author)
  # OAinPW_Richness_noUSACHN$author <- ordered(OAinPW_Richness_noUSACHN$author, levels = c("first", "last","all"))
  OAinPW_Richness_noUSACHN<-OAinPW_Richness_noUSACHN %>% 
    select(author,metric,OAinPW_noUSAorCHN=OAinPW_avg_richness,CIs_noUSAorCHN=CIs,CIlow,CIhigh) %>% 
    arrange(author)
  OAinPW_Richness_noUSACHN
  
  
  
  OAinPW_Diversity_noUSACHN<-OAinPW_sampled_noUSACHN %>% group_by(author) %>% 
    summarize(OAinPW_avg_richness=mean(InvSimp),
              SD_avg_richness=sd(InvSimp),
              count = n(),
              CIlow=quantile(InvSimp, alpha/2),
              CIhigh=quantile(InvSimp, 1-alpha/2))
  OAinPW_Diversity_noUSACHN$SD_avg_richness<-round(OAinPW_Diversity_noUSACHN$SD_avg_richness,1)
  OAinPW_Diversity_noUSACHN$OAinPW_avg_richness<-round(OAinPW_Diversity_noUSACHN$OAinPW_avg_richness,1)
  OAinPW_Diversity_noUSACHN$CIlow<-round(OAinPW_Diversity_noUSACHN$CIlow,1)
  OAinPW_Diversity_noUSACHN$CIhigh<-round(OAinPW_Diversity_noUSACHN$CIhigh,1)
  
  
  OAinPW_Diversity_noUSACHN$author<-gsub("author_","",OAinPW_Diversity_noUSACHN$author)
  OAinPW_Diversity_noUSACHN$OAinPW_avg_richness<-paste(OAinPW_Diversity_noUSACHN$OAinPW_avg_richness,
                                                       OAinPW_Diversity_noUSACHN$SD_avg_richness,
                                                       sep=" +/- ")
  
  OAinPW_Diversity_noUSACHN$CIs<-paste(OAinPW_Diversity_noUSACHN$CIlow,
                                       OAinPW_Diversity_noUSACHN$CIhigh,
                                       sep="-")
  
  OAinPW_Diversity_noUSACHN$metric<-"Diversity"
  OAinPW_Diversity_noUSACHN$author <-as.factor(OAinPW_Diversity_noUSACHN$author)
  # OAinPW_Diversity_noUSACHN$author <- ordered(OAinPW_Diversity_noUSACHN$author, levels = c("first", "last","all"))
  OAinPW_Diversity_noUSACHN<-OAinPW_Diversity_noUSACHN %>% 
    select(author,metric,OAinPW_noUSAorCHN=OAinPW_avg_richness,CIs_noUSAorCHN=CIs,CIlow,CIhigh) %>% 
    arrange(author)
  
  #############################
  
  
  OAinPW_Stats_ALL<-bind_rows(OAinPW_Richness,OAinPW_Diversity)
  OAinPW_Stats_noUSA_CHN<-bind_rows(OAinPW_Richness_noUSACHN,OAinPW_Diversity_noUSACHN)
  OAinPW_Stats_ALL<-bind_cols(OAinPW_Stats_ALL,OAinPW_Stats_noUSA_CHN)
  
  
  DivMetricsPubsPooled_OA<-unlist(DivMetrics_mirror_first)
  DivMetricsPubsPooled_OA<-round(DivMetricsPubsPooled_OA,1)
  OAinPW_Stats_ALL$OA_AllCountries<-DivMetricsPubsPooled_OA
  
  DivMetricsPubsPooled_OA_noCHNorUSA<-unlist(DivMetrics_mirror_noCHNorUSA)
  DivMetricsPubsPooled_OA_noCHNorUSA<-round(DivMetricsPubsPooled_OA_noCHNorUSA,1)
  OAinPW_Stats_ALL$OA_noCHNorUSA<-DivMetricsPubsPooled_OA_noCHNorUSA
  
  RichDiv_Stats_ALL <- OAinPW_Stats_ALL %>% 
    select(author=author...1,metric=metric...2,OA_AllCountries,OAinPW_AllCountries,CIs_AllCountries,
           CIlow=CIlow...5,CIhigh=CIhigh...6,OA_noCHNorUSA,OAinPW_noUSAorCHN,CIs_noUSAorCHN,
           CIlow1=CIlow...11,CIhigh1=CIhigh...12)
  colnames(RichDiv_Stats_ALL)
  
  
  
  RichDiv_Stats_SOLO
  RichDiv_Stats_ALL
  
  
  Table2<-bind_rows(RichDiv_Stats_ALL,RichDiv_Stats_SOLO)
  Table2$author <- ordered(Table2$author, levels = c("solo","first", "last","all"))
  Table2<-Table2 %>% 
    select(-CIlow,-CIhigh,-CIlow1,-CIhigh1) %>% 
    filter(author!="all") %>% 
    arrange(desc(metric),author)
  
  Table2$OAinPW_AllCountries<-as.character(Table2$OAinPW_AllCountries)
  Table2$OAinPW_noUSAorCHN<-as.character(Table2$OAinPW_noUSAorCHN)
  Table2$OAinPW_AllCountries<-str_replace(Table2$OAinPW_AllCountries, "[+]", "\u00B1")
  Table2$OAinPW_AllCountries<-str_replace(Table2$OAinPW_AllCountries, "[/]", "")
  Table2$OAinPW_AllCountries<-str_replace(Table2$OAinPW_AllCountries, "[-]", "")
  Table2$OAinPW_noUSAorCHN<-str_replace(Table2$OAinPW_noUSAorCHN, "[+]", "\u00B1")
  Table2$OAinPW_noUSAorCHN<-str_replace(Table2$OAinPW_noUSAorCHN, "[/]", "")
  Table2$OAinPW_noUSAorCHN<-str_replace(Table2$OAinPW_noUSAorCHN, "[-]", "")
  Table2$author<-str_to_title(Table2$author)
  Table2$author<-gsub("Solo","Single",Table2$author)
  names(Table2)<-c("Author","Metric","Mirror OA (All Countries)","Mean OAinPW (All Countries)",
                   "OAinPW 95% CI (All Countries)", "Mirror OA (USA & CHN excluded)",
                   "Mean OAinPW (USA & CHN excluded)","OAinPW 95% CI (USA & CHN excluded)")
  Table2 <- Table2 %>% select("Metric","Author","Mirror OA (All Countries)",
                              "Mean OAinPW (All Countries)","OAinPW 95% CI (All Countries)",
                              "Mirror OA (USA & CHN excluded)","Mean OAinPW (USA & CHN excluded)",
                              "OAinPW 95% CI (USA & CHN excluded)")
  
  Table2<-as.data.frame(Table2)
  
  
  
  
  
  
  # SOLO, ALL, RICH
  R_crit_solo_all<-as.numeric(DivMetrics_Mirror_solo[1,1])
  perc_R_SOLO_ALL<-as.data.frame(BootPW_SoloAll[1]) %>% 
    tally(Richness>R_crit_solo_all)/1000
  perc_R_SOLO_ALL
  perc_R_SOLO_ALL$Author<-"Single"
  perc_R_SOLO_ALL$Dataset<-"All Countries"
  perc_R_SOLO_ALL$Metric<-"Richness"
  
  # SOLO, NO CHN USA, RICH
  R_crit_solo_no_CHNUSA<-as.numeric(DivMetrics_Mirror_solo_noCHNUSA[1,1])
  perc_R_SOLO_NOCHNUSA<-as.data.frame(BootPW_SoloNoUSACHN[1]) %>% 
    tally(Richness>R_crit_solo_no_CHNUSA)/1000
  perc_R_SOLO_NOCHNUSA
  perc_R_SOLO_NOCHNUSA$Author<-"Single"
  perc_R_SOLO_NOCHNUSA$Dataset<-"Without China & USA"
  perc_R_SOLO_NOCHNUSA$Metric<-"Richness"
  
  
  
  # FIRST, ALL, RICH
  R_crit_first_all<-as.numeric(DivMetrics_mirror_CO[1,1])
  perc_R_first_all<-as.data.frame(BootPW_CoAll[1]) %>% 
    tally(Richness>R_crit_first_all)/1000
  perc_R_first_all
  perc_R_first_all$Author<-"First"
  perc_R_first_all$Dataset<-"All Countries"
  perc_R_first_all$Metric<-"Richness"
  
  
  # FIRST, NO CHN USA, RICH
  R_crit_first_no_CHNUSA<-as.numeric(DivMetrics_mirror_COnoCHNorUSA[1,1])
  perc_R_first_NOCHNUSA<-as.data.frame(BootPW_CoNoUSACHN[1]) %>% 
    tally(Richness>R_crit_first_no_CHNUSA)/1000
  perc_R_first_NOCHNUSA
  perc_R_first_NOCHNUSA$Author<-"First"
  perc_R_first_NOCHNUSA$Dataset<-"Without China & USA"
  perc_R_first_NOCHNUSA$Metric<-"Richness"
  
  
  # SOLO, ALL, DIV
  Div_crit_solo_all<-as.numeric(DivMetrics_Mirror_solo[1,2])
  perc_Div_SOLO_ALL<-as.data.frame(BootPW_SoloAll[1]) %>% 
    tally(InvSimp>Div_crit_solo_all)/1000
  perc_Div_SOLO_ALL
  perc_Div_SOLO_ALL$Author<-"Single"
  perc_Div_SOLO_ALL$Dataset<-"All Countries"
  perc_Div_SOLO_ALL$Metric<-"Diversity"
  
  # SOLO, NO CHN USA, DIV
  Div_crit_solo_no_CHNUSA<-as.numeric(DivMetrics_Mirror_solo_noCHNUSA[1,2])
  perc_D_SOLO_NOCHNUSA<-as.data.frame(BootPW_SoloNoUSACHN[1]) %>% 
    tally(InvSimp>Div_crit_solo_no_CHNUSA)/1000
  perc_D_SOLO_NOCHNUSA
  perc_D_SOLO_NOCHNUSA$Author<-"Single"
  perc_D_SOLO_NOCHNUSA$Dataset<-"Without China & USA"
  perc_D_SOLO_NOCHNUSA$Metric<-"Diversity"
  
  
  
  # FIRST, ALL, DIV
  Div_crit_first_all<-as.numeric(DivMetrics_mirror_CO[1,2])
  perc_Div_first_ALL<-as.data.frame(BootPW_CoAll[1]) %>% 
    tally(InvSimp>Div_crit_first_all)/1000
  perc_Div_first_ALL<-perc_Div_first_ALL
  perc_Div_first_ALL$Author<-"First"
  perc_Div_first_ALL$Dataset<-"All Countries"
  perc_Div_first_ALL$Metric<-"Diversity"
  
  
  # FIRST, NO CHN USA, DIV
  Div_crit_first_no_CHNUSA<-as.numeric(DivMetrics_mirror_COnoCHNorUSA[1,2])
  perc_D_first_NOCHNUSA<-as.data.frame(BootPW_CoNoUSACHN[1]) %>% 
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
