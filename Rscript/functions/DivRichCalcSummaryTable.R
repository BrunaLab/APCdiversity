DivRichCalcSummaryTable<-function(DataSet) {
  
  library(vegan)
  library(reshape)
  library(tidyr)
  library(dplyr)
  
  ##################################################
  # FIRST AUTHOR DIVERSITY STATS
  source("./Rscript/functions/DivRichCalc.R") 
  # all articles together
  AuPosition<-"author_first"
  JrnlType<-"All"
  Div_pool_allpubs_first<-DivRichCalc(DataSet,AuPosition,JrnlType)
  
  # OA articles
  AuPosition<-"author_first"
  JrnlType<-"OA"
  Div_OA_pool_first<-DivRichCalc(DataSet,AuPosition,JrnlType)
  
  # PW articles
  AuPosition<-"author_first"
  JrnlType<-"PW"
  Div_PW_pool_first<-DivRichCalc(DataSet,AuPosition,JrnlType)
  
  # Binding together
  DivMetricsPubsPooled_first <- as.data.frame(cbind(Div_OA_pool_first[1],
                                                    Div_PW_pool_first[1],
                                                    Div_pool_allpubs_first[1],
                                                    Div_OA_pool_first[2],
                                                    Div_PW_pool_first[2],
                                                    Div_pool_allpubs_first[2]))
  DivMetricsPubsPooled_first <-DivMetricsPubsPooled_first %>% 
    dplyr::rename(OA_richness=V1,PW_richness=V2,ALL_richness=V3,
                  OA_invSimp=V4,PW_invSimp=V5,ALL_invSimp=V6)
  rownames(DivMetricsPubsPooled_first)<-nrow(DivMetricsPubsPooled_first)
  DivMetricsPubsPooled_first
  
  #################
  # By Last Author
  #################
  # All journals
  AuPosition<-"author_last"
  JrnlType<-"All"
  Div_allpubs_pool_last<-DivRichCalc(DataSet,AuPosition,JrnlType)
  
  #OA journals
  AuPosition<-"author_last"
  JrnlType<-"OA"
  Div_OA_pool_last<-DivRichCalc(DataSet,AuPosition,JrnlType)
  
  #PW journals
  AuPosition<-"author_last"
  JrnlType<-"PW"
  Div_PW_pool_last<-DivRichCalc(DataSet,AuPosition,JrnlType)
  
  # Binding together
  DivMetricsPubsPooled_last <- as.data.frame(cbind(Div_OA_pool_last[1],
                                                   Div_PW_pool_last[1],
                                                   Div_allpubs_pool_last[1],
                                                   Div_OA_pool_last[2],
                                                   Div_PW_pool_last[2],
                                                   Div_allpubs_pool_last[2]))
  DivMetricsPubsPooled_last <-DivMetricsPubsPooled_last %>%
    dplyr::rename(OA_richness=V1,PW_richness=V2,ALL_richness=V3,
                  OA_invSimp=V4,PW_invSimp=V5,ALL_invSimp=V6)
  rownames(DivMetricsPubsPooled_last)<-nrow(DivMetricsPubsPooled_last)
  DivMetricsPubsPooled_last
  
  
  #################
  # By All Author
  #################
  AuPosition<-"author_all"
  JrnlType<-"All"
  Div_pool_allpubs_all<-DivRichCalc(DataSet,AuPosition,JrnlType)
  
  AuPosition<-"author_all"
  JrnlType<-"OA"
  Div_OA_pool_all<-DivRichCalc(DataSet,AuPosition,JrnlType)
  
  AuPosition<-"author_all"
  JrnlType<-"PW"
  Div_PW_pool_all<-DivRichCalc(DataSet,AuPosition,JrnlType)
  
  # Binding together
  DivMetricsPubsPooled_all <- as.data.frame(cbind(Div_OA_pool_all[1],
                                                  Div_PW_pool_all[1],
                                                  Div_pool_allpubs_all[1],
                                                  Div_OA_pool_all[2],
                                                  Div_PW_pool_all[2],
                                                  Div_pool_allpubs_all[2]))
  DivMetricsPubsPooled_all <-DivMetricsPubsPooled_all %>%
    dplyr::rename(OA_richness=V1,PW_richness=V2,ALL_richness=V3,
                  OA_invSimp=V4,PW_invSimp=V5,ALL_invSimp=V6)
  rownames(DivMetricsPubsPooled_all)<-nrow(DivMetricsPubsPooled_all)
  DivMetricsPubsPooled_all
  
  #######################################
  # put together in a a Table 
  #######################################
  Table2<-bind_rows(unlist(DivMetricsPubsPooled_first),
                    unlist(DivMetricsPubsPooled_last),
                    unlist(DivMetricsPubsPooled_all))
  Table2<-round(Table2,2) #rounding vlaues for diverstity
  Table2$Authors<-c("First","Last","All")
  
  Table2<- Table2 %>% select("Authors",
                             "OA_richness.Richness",
                             "PW_richness.Richness",
                             "ALL_richness.Richness",
                             "OA_invSimp.Richness",
                             "PW_invSimp.Richness",
                             "ALL_invSimp.Richness")  
  
  Table2<-mutate(Table2, OA_Evenness = OA_invSimp.Richness/OA_richness.Richness)
  Table2<-mutate(Table2, PW_Evenness = PW_invSimp.Richness/PW_richness.Richness)
  Table2<-mutate(Table2, ALL_Evenness = ALL_invSimp.Richness/ALL_richness.Richness)
  
  names(Table2)<-c("Authors",
                   "OA Richness","PW Richness","All Richness",
                   "OA Diversity","PW Diversity","All Diversity",
                   "OA Evenness","PW Evenness","All Evenness")
  
  
  return(Table2)
  
  
}