DivCalcPooledFirst<-function(DataSet) {
  
  library(vegan)
  library(reshape)
  library(tidyr)
  library(dplyr)
  # DataSet<-OA_papers
  # Author=1
  SiteBySpec<-DataSet %>%
    filter(Country != "NA" & Code != "NA") %>%
    filter(AuthorNum==1) %>%
    group_by(Country)%>%
    tally()
  Countries<-SiteBySpec
  Richness<-nrow(SiteBySpec)
  SiteBySpec <- SiteBySpec %>% spread(Country, n)
  InvSimpsons <- diversity(SiteBySpec, index = "invsimpson")
  return(list(Richness=Richness,InvSimpsons=InvSimpsons,Countries=Countries))
  
}