SummaryTable<-function(DataSet) {
  library(tidyr)
  library(dplyr)
  # DataSet<-AllData
MirrorPairs<-read.csv("./data_clean/MirrorPairs.csv")
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
    # ungroup(Table1) %>%
    select(JrnlType,Journal,pair_key) %>% 
    spread(JrnlType,Journal)  
  APC<-MirrorPairs %>% select(APC,pair_key)
Table1<-left_join(Table1,APC,by="pair_key") %>% 
    filter(APC!="NA")
  
  Table1.1 <- DataSet %>% #number of papers per journal
    select(JrnlType,pair_key,DOI) %>% 
    group_by(JrnlType,pair_key) %>% 
    summarize(n=n_distinct(DOI))
  Table1.1<-ungroup(Table1.1) 
  Table1.1<-Table1.1 %>%
    select(JrnlType,n,pair_key) %>% 
    spread(JrnlType,n)
  
  Table1<-left_join(Table1,Table1.1,by="pair_key") %>% 
    select(PW=PW.x,PW1=PW.y,OA=OA.x,OA2=OA.y,APC)  
  Table1<-ungroup(Table1)  
  Table1<-Table1 %>% select(-pair_key)
  
  Table1$PW<-as.character(Table1$PW)
  Table1$PW<-str_to_title(Table1$PW)
  Table1$OA<-as.character(Table1$OA)
  Table1$OA<-str_to_title(Table1$OA)
  Table1$OA<-str_replace(Table1$OA, " And ", " and ")
  Table1$OA<-str_replace(Table1$OA, " Of ", " of ")
  Table1$PW<-str_replace(Table1$PW, " And ", " and ")
  Table1$PW<-str_replace(Table1$PW, " Of ", " of ")
  names(Table1)<-c("Journal","Articles (n)","Open Access Mirror","Articles (n)","APC ($)")
  pub_totals<-c("Total: ",TOTAL_NumbArtPW,"Total:",TOTAL_NumbArtOA)
  Table1<-rbind(Table1,pub_totals)
  rm(Table1.1,pub_totals)
  
  ####ALTERNATIVE, MORE EFFICIENT TABLE 1
  Table1v2<-Table1
  names(Table1v2)<- names(Table1v2)<-c("Journal","Articles (n)","OA Mirror","OA Mirror Articles (n)","APC ($)")
  Table1v2<-Table1v2 %>% select(-'OA Mirror')
  
  Tables<-list(Table1,Table1v2)
  
  return(Tables)
}