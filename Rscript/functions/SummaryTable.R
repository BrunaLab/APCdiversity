SummaryTable<-function(DataSet) {
  library(tidyr)
  library(dplyr)
  DataSet<-AllData
  
# DataSet<-AllData  
  NumbArticles_JrnlType <- DataSet %>% #number of papers per journal
    group_by(JrnlType) %>% 
    summarize(n=n_distinct(DOI))
  
  TOTAL_NumbArtPW<-NumbArticles_JrnlType %>% 
    filter(JrnlType=="PW") %>% 
    select(n)
  TOTAL_NumbArtPW<-as.numeric(TOTAL_NumbArtPW)
  
  TOTAL_NumbArtOA<-NumbArticles_JrnlType %>% 
    filter(JrnlType=="OA") %>% 
    select(n)
  TOTAL_NumbArtOA<-as.numeric(TOTAL_NumbArtOA)
  
  Table1<- DataSet %>% #number of papers per journal
    group_by(pair_key,JrnlType,Journal) %>% 
    summarize(n=n_distinct(DOI)) %>% 
    arrange(Journal) %>% 
    ungroup(Table1) %>%
    select(JrnlType,Journal,pair_key) %>% 
    spread(JrnlType,Journal)
  
  Table1.1 <- DataSet %>% #number of papers per journal
    select(JrnlType,pair_key,DOI) %>% 
    group_by(JrnlType,pair_key) %>% 
    summarize(n=n_distinct(DOI))
  Table1.1<-ungroup(Table1.1) %>%
    select(JrnlType,n,pair_key) %>% 
    spread(JrnlType,n)
  
  Table1<-bind_cols(Table1,Table1.1) %>% 
    select(PW,PW1,OA,OA1)
  
  Table1$PW<-as.character(Table1$PW)
  Table1$PW<-str_to_title(Table1$PW)
  Table1$OA<-as.character(Table1$OA)
  Table1$OA<-str_to_title(Table1$OA)
  Table1$OA<-str_replace(Table1$OA, " And ", " and ")
  Table1$OA<-str_replace(Table1$OA, " Of ", " of ")
  Table1$PW<-str_replace(Table1$PW, " And ", " and ")
  Table1$PW<-str_replace(Table1$PW, " Of ", " of ")
  names(Table1)<-c("Journal","Articles (n)","Open Access Mirror","Articles (n)")
  pub_totals<-c("Total: ",TOTAL_NumbArtPW,"Total:",TOTAL_NumbArtOA)
  Table1<-rbind(Table1,pub_totals)
  rm(Table1.1,pub_totals)
  
  ####ALTERNATIVE, MORE EFFICIENT TABLE 1
  Table1v2<-Table1
  names(Table1v2)<- names(Table1v2)<-c("Journal","No. Articles","Open Access Mirror","No. Articles - OA Mirror")
  Table1v2<-Table1v2 %>% select(-'Open Access Mirror')
  
  Table1<-list(Table1,Table1v2)
  
  return(Table1)
}