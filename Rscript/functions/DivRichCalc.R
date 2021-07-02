DivRichCalc<-function(DataSet,AuPosition,JrnlType,ArticleType) {

  # sole_NOCHNUSA<-read_csv("./data_clean/one_author_pubsNOCHNUSA.csv",col_types = cols())
  # coauthor_NOCHNUSA<-read_csv("./data_clean/coauthor_pubsNOCHNUSA.csv",col_types = cols())
  # sole_ALL<-read_csv("./data_clean/sole_author_pubs_ALL_first_author.csv",col_types = cols())
  # coauthor_ALL<-read_csv("./data_clean/coauthor_pubs_ALL_first_author.csv",col_types = cols())
  # DataSet<-OA_paper_pool
  # AuPosition<-"author_first"
  # ArticleType<-"PW"
  # DataSet<-OAinPW_sample
  # AuPosition<-"author_first"
  # ArticleType<-"OA"
  # ArticleType<-"OAinPW"
  # JrnlType<-"both"
  
  vars<-list(DataSet,AuPosition,ArticleType)
  ArticleType<-as.character(vars[3])
  AuPosition<-as.character(vars[2])
  
  if (((AuPosition=="author_first")==TRUE) & ((JrnlType=="OA")==TRUE) & ((ArticleType=="OA")==TRUE)) {
    DataSet<-as.data.frame(vars[1]) %>% 
      filter(ArticleType == "OA") %>% 
      filter(JrnlType == "OA") %>% 
      filter(AuthorNum==1)
  
  } else if (((AuPosition=="author_first")==TRUE) & ((JrnlType=="PW")==TRUE) & ((ArticleType=="OA")==TRUE))  {
    DataSet<-as.data.frame(vars[1]) %>% 
      filter(ArticleType == "OA") %>% 
      filter(JrnlType == "PW") %>% 
      filter(AuthorNum==1)
    
  } else if (((AuPosition=="author_first")==TRUE) & ((JrnlType=="PW")==TRUE) & ((ArticleType=="PW")==TRUE))  {
    DataSet<-as.data.frame(vars[1]) %>% 
      filter(ArticleType == "PW") %>% 
      filter(JrnlType == "PW") %>% 
      filter(AuthorNum==1)

    
  } else if (((AuPosition=="author_first")==TRUE) & ((JrnlType=="both")==TRUE) & ((ArticleType=="OA")==TRUE))  {
    DataSet<-as.data.frame(vars[1]) %>% 
      filter(ArticleType == "OA") %>% 
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
