DivRichCalc<-function(DataSet,AuPosition,JrnlType) {
  
  sole_NOCHNUSA<-read_csv("./data_clean/one_author_pubsNOCHNUSA.csv")
  coauthor_NOCHNUSA<-read_csv("./data_clean/coauthor_pubsNOCHNUSA.csv")
  sole_ALL<-read_csv("./data_clean/sole_author_pubs_ALL_first_author.csv")
  coauthor_ALL<-read_csv("./data_clean/coauthor_pubs_ALL_first_author.csv")
  
  # AuPosition<-"author_first"
  # JrnlType<-"OA"
  # DataSet<-sole_ALL
  
  vars<-list(DataSet,AuPosition,JrnlType)
  
  
  if (((vars[2]=="author_first")==TRUE) & ((vars[3]=="OA")==TRUE)) {
    DataSet<-as.data.frame(vars[1]) %>% 
      filter(JrnlType == "OA") %>% 
      filter(AuthorNum==1)
  
  } else if (((vars[2]=="author_first")==TRUE) & ((vars[3]=="PW")==TRUE))  {
    DataSet<-as.data.frame(vars[1]) %>% 
      filter(JrnlType == "PW") %>% 
      filter(AuthorNum==1)
    
  } else {
    stop("Please enter 'author_first', for 'AuPosition' ")
  }
  
  
  library(vegan)
  library(reshape)
  library(tidyr)
  library(dplyr)
  
  SiteBySpec<-DataSet %>%
    filter(First_Author_Country != "NA" & Code != "NA") %>%
    group_by(Code)%>%
    tally()
  Countries<-SiteBySpec
  Richness<-nrow(SiteBySpec)
  SiteBySpec <- SiteBySpec %>% spread(Code, n)
  
  InvSimpsons <- diversity(SiteBySpec, index = "invsimpson")
  Shannon <- diversity(SiteBySpec, index = "shannon")
  Even <- Shannon/log(Richness)
  return(list(Richness=Richness,InvSimpsons=InvSimpsons,Countries=Countries,Shannon=Shannon,Even=Even))
  
}