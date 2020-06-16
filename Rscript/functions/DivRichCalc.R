DivRichCalc<-function(DataSet,AuPosition,JrnlType) {
  
  vars<-list(DataSet,AuPosition,JrnlType)
  
  if ((vars[2]=="author_first")==TRUE) {
    
    DataSet<-DataSet %>%
      group_by(DOI) %>% 
      filter(AuthorNum == 1)
    
  } else if ((vars[2]=="author_last")==TRUE) {
    
    DataSet <-DataSet %>%
      group_by(DOI) %>%
      filter(AuthorNum == max(AuthorNum)) %>% 
      filter(AuthorNum>1)  # This removes any single authior papers
    
    
  } else if ((vars[2]=="author_all")==TRUE) {
    
    DataSet <-DataSet
      
  } else {
    stop("Please enter 'author_first', 'author_last', or 'author_all' for AuPosition")
    
  }
  
  # summary(as.factor(DataSet$JrnlType))
  
  if ((vars[3]=="OA")==TRUE) {
    
    DataSet<-DataSet %>% filter(JrnlType == "OA")
    
  } else if ((vars[3]=="PW")==TRUE) {
    
    DataSet<-DataSet %>% filter(JrnlType == "PW")
  
    } else if ((vars[3]=="All")==TRUE) {
    
    DataSet<-DataSet
    
  } else {
    stop("Please enter 'PW' , 'OA' , or 'All' for JrnlType")
    
  }
  
  
  library(vegan)
  library(reshape)
  library(tidyr)
  library(dplyr)
  
  SiteBySpec<-DataSet %>%
    filter(Country != "NA" & Code != "NA") %>%
    group_by(Country)%>%
    tally()
  Countries<-SiteBySpec
  Richness<-nrow(SiteBySpec)
  SiteBySpec <- SiteBySpec %>% spread(Country, n)
  InvSimpsons <- diversity(SiteBySpec, index = "invsimpson")
  # InvSimpsons <- diversity(SiteBySpec, index = "shannon")
  return(list(Richness=Richness,InvSimpsons=InvSimpsons,Countries=Countries))
  
}