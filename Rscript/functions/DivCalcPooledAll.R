DivCalcPooledAll<-function(DataSet) {
  
  library(vegan)
  library(reshape)
  library(tidyr)
  library(dplyr)
  # DataSet<-PW_papers
  SiteBySpec<-DataSet %>%
    filter(Country != "NA" & Code != "NA") %>%
    group_by(Country) %>% 
    tally()
  Countries<-SiteBySpec
  Richness<-nrow(SiteBySpec)
  SiteBySpec <- SiteBySpec %>% spread(Country, n)
  InvSimpsons <- diversity(SiteBySpec, index = "invsimpson")
  return(list(Richness=Richness,InvSimpsons=InvSimpsons,Countries=Countries))
  
}