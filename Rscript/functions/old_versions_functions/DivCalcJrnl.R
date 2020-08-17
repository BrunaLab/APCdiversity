DivCalcJrnl<-function(DataSet) {
  
  library(vegan)
  library(reshape)
  library(tidyr)
  library(dplyr)
  # DataSet<-OA_papers
  # Author=1
  SiteBySpec1<-DataSet %>%
    filter(First_Author_Country != "NA" & Code != "NA") %>%
    filter(AuthorNum==1) %>%
    group_by(Journal, Code)%>%
    tally()
  
  SiteBySpec1 <- cast(SiteBySpec1, Journal ~ Code, value = 'n')
  SiteBySpec1[is.na(SiteBySpec1)] <- 0
  ncols_SiteBySpec1<-ncol(SiteBySpec1)
  Countries <- names(SiteBySpec1[,2:ncols_SiteBySpec1]) #check this to be sure this 
  
  ####
  #Add diversity metrics
  country_counts <- SiteBySpec1[,Countries] #final site by species matrix
  row.names(country_counts) <- SiteBySpec1$Journal #add rownames for journals
  
  #simpson diversity index
  DivSimpson <-diversity(country_counts, index = "simpson")
  
  #abundance
  abund <-rowSums(country_counts)
  
  #richness
  rich <- rowSums(country_counts > 0)
  
  DivMetrics <- as.data.frame(cbind(rich, abund, DivSimpson)) 
  DivMetrics$Journal <- SiteBySpec1$Journal
  DivMetrics$EffectSpecNum <- 1/(1-DivMetrics$DivSimpson)
  
  MirrorPairs<-AllData %>% 
    select(JrnlType,pair_key,Journal) %>% 
    group_by(JrnlType,pair_key,Journal) %>% 
    slice(1)
  
  DivMetrics<-left_join(DivMetrics,MirrorPairs,by="Journal")
  DivMetrics$Journal<-as.factor(DivMetrics$Journal)
  DivMetrics<-select(DivMetrics, Journal, pair_key,JrnlType,
                      rich, DivSimpson,abund, EffectSpecNum)
  
  DivMetricsPW_wide<-DivMetrics %>% 
    filter(JrnlType=="PW") %>% 
    # select(-Journal) %>% 
    spread(JrnlType,DivSimpson) %>% 
    dplyr::rename(PW_Journal=Journal,PW_DivSimpson=PW,PW_rich=rich,PW_abund=abund,PW_EffectSpecNum=EffectSpecNum)
  DivMetricsOA_wide<-DivMetrics %>% 
    filter(JrnlType=="OA") %>% 
    spread(JrnlType,DivSimpson) %>% 
    dplyr::rename(OA_Journal=Journal,OA_DivSimpson=OA,OA_rich=rich,OA_abund=abund,OA_EffectSpecNum=EffectSpecNum)
  DivMetricsALL<-left_join(DivMetricsPW_wide,DivMetricsOA_wide,by="pair_key")
  # Add Difference in diversity score (DeltaDiv)
  DivMetricsALL$DeltaDiv <- DivMetricsALL$PW_DivSimpson - DivMetricsALL$OA_DivSimpson
  DivMetricsALL$DeltaRich <- DivMetricsALL$PW_rich - DivMetricsALL$OA_rich
  
  
  return(DivMetricsALL)
}