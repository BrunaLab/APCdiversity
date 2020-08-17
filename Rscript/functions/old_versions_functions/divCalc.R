divCalc<-function(DataSet,Author) {
  
  library(vegan)
  library(reshape)
  library(tidyr)
  library(dplyr)
  # DataSet<-OA_papers
  # Author=1
  SiteBySpec1<-DataSet %>%
    filter(Country != "NA" & Code != "NA") %>%
    filter(AuthorNum==Author) %>%
    group_by(Journal, Country)%>%
    tally()
  
  SiteBySpec1 <- cast(SiteBySpec1, Journal ~ Country, value = 'n')
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
  
  
  MirrorPairs<-read_csv(file="./data/MirrorPairs.csv")
  MirrorPairs<-MirrorPairs %>% dplyr::rename("Journal"="SO")
   
  DivMetrics<-left_join(DivMetrics,MirrorPairs,by="Journal")
  DivMetrics$Journal<-as.factor(DivMetrics$Journal)
  DivMetrics<-select(DivMetrics, Journal, pair_key,journal_cat,
                      rich, DivSimpson,abund, EffectSpecNum)
  DivMetrics<-DivMetrics<-DivMetrics %>% dplyr::rename("JrnlType"="journal_cat")

  return(DivMetrics)
  
  
}