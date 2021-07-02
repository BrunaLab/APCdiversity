SummaryTable<-function(DataSet, MirrorPairs) {
  library(tidyr)
  library(dplyr)
  

# DataSet<-AllData  
  NumbArticles_JrnlType <- DataSet %>% #number of papers per journal
    group_by(JrnlType, ArticleType) %>% 
    summarize(n=n_distinct(refID))
  
  TOTAL_NumbArtPW<-NumbArticles_JrnlType %>% 
    filter(JrnlType=="PW" & ArticleType=="PW") %>% 
    select(n)
  TOTAL_NumbArtPW<-as.numeric(TOTAL_NumbArtPW$n)
  
  TOTAL_NumbArtOAinPW<-NumbArticles_JrnlType %>% 
    filter(JrnlType=="PW" & ArticleType=="OA") %>% 
    select(n)
  TOTAL_NumbArtOAinPW<-as.numeric(TOTAL_NumbArtOAinPW$n)
  
  TOTAL_NumbArtMirror<-NumbArticles_JrnlType %>% 
    filter(JrnlType=="OA" & ArticleType=="OA") %>% 
    select(n)
  TOTAL_NumbArtMirror<-as.numeric(TOTAL_NumbArtMirror$n)
  
  
  Table1<- DataSet %>% #number of papers per journal
    group_by(pair_key,JrnlType,ArticleType) %>% 
    summarize(n=n_distinct(refID)) %>% 
    arrange(pair_key) 
  Table1$pair_key <- as.factor(Table1$pair_key)
    # ungroup(Table1) %>%
  Table1<-Table1 %>% pivot_wider(values_from=n, names_from=c(JrnlType,ArticleType))
  APC<-MirrorPairs %>% select(APC,pair_key,Journal,JrnlType)
  APC$pair_key <- as.factor(APC$pair_key)
  PW_journals<-MirrorPairs %>% 
    select(APC,pair_key,Journal,JrnlType) %>% 
    # filter(is.na(APC)) %>% 
    filter(JrnlType=="PW") %>% 
    select(pair_key,Journal) %>% 
    dplyr::rename("JournalPW"="Journal")
  
  PW_journals$pair_key <- as.factor(PW_journals$pair_key)
  Journal <- APC %>% 
    filter(JrnlType=="OA") %>% 
    select(Journal,pair_key)
  APC <- APC %>% 
    select(-Journal) %>% 
    pivot_wider(values_from=APC, names_from=c(JrnlType)) %>% 
    select(-`NA`) %>% 
    left_join(Journal)

    
Table1<-left_join(Table1,APC,by="pair_key") %>%
  # filter(APC!="NA") %>% 
  replace_na(list(PW_OA=0)) %>%
  select(PW_PW,PW_OA,Journal,OA_OA,OA,PW) %>% 
  left_join(PW_journals)
  
  Table1$Journal<-as.character(Table1$Journal)
  Table1$Journal<-str_to_title(Table1$Journal)
  Table1$Journal<-str_replace(Table1$Journal, " And ", " and ")
  Table1$Journal<-str_replace(Table1$Journal, " Of ", " of ")
  
  Table1$JournalPW<-as.character(Table1$JournalPW)
  Table1$JournalPW<-str_to_title(Table1$JournalPW)
  Table1$JournalPW<-str_replace(Table1$JournalPW, " And ", " and ")
  Table1$JournalPW<-str_replace(Table1$JournalPW, " Of ", " of ")
  
  Table1 <- Table1 %>% ungroup() %>% select(-pair_key)
  
  Table1<-dplyr::rename(Table1,
                        "PW (n)"="PW_PW",
                        "OAinPW (n)"="PW_OA",
                        "Open Access Mirror"="Journal",
                        "Journal"="JournalPW",
                        "Mirror (n)"="OA_OA",
                        "Mirror APC ($)"="OA",
                        "Parent APC ($)"="PW")
  
  Table1<-Table1 %>% select(Journal,
                            `PW (n)`,
                            `OAinPW (n)`,
                            `Open Access Mirror`,
                            `Mirror (n)`,
                            `Mirror APC ($)`,
                            `Parent APC ($)`)
  pub_totals<-data.frame("a"=NA,
                         "b"=sum(Table1$`PW (n)`),
                         "c"=sum(Table1$`OAinPW (n)`),
                         "d"=NA,
                         "e"=sum(Table1$`Mirror (n)`),
                         "f"=NA,
                         "g"=NA)
  names(pub_totals)<-names(Table1)
  
  Table1<-bind_rows(Table1,pub_totals)
  
    ####ALTERNATIVE, MORE EFFICIENT TABLE 1
  Table1v2<-Table1
  names(Table1v2)<- names(Table1v2)<-c("Journal","Articles PW (n)",
                                       "Articles OAinPW (n)",
                                       "OA Mirror",
                                       "Mirror Articles (n)","Mirror APC ($)",
                                       "Parent APC ($)")
  Table1v2<-Table1v2 %>% select(-'OA Mirror')
  
  Tables<-list(Table1,Table1v2)
  
  return(Tables)
}