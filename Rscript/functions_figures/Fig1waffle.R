Fig1waffle<-function(AllData) {
  library(waffle)
  #data
  first_author_income_cats<-AllData %>% 
    filter(AuthorNum==1) %>% 
    group_by(JrnlType,IncomeGroup) %>% 
    tally() %>% 
    mutate(percentage=n/sum(n)*100)
  
  waffle_df<-filter(first_author_income_cats,JrnlType=="OA")
  percOA<-waffle_df$percentage
  names(percOA)<-as.character(waffle_df$IncomeGroup)
  waffle_df2<-filter(first_author_income_cats,JrnlType=="PW")
  percPW<-waffle_df2$percentage
  names(percPW)<-as.character(waffle_df2$IncomeGroup)
  
  waffle(percPW,rows=14, size=1,legend_pos = "top", title = "Paywalled Articles")
  waffle(percOA,rows=6, size=1,legend_pos = "top", title = "Open Access Articles")
  
  plot1waffle<- gridExtra::grid.arrange(
    waffle(percOA,rows=14, size=1,legend_pos = "right", xlab = "Open Access Articles",flip=TRUE,title="Fig. 1: 1st Author National Income Category (%)"),
    waffle(percPW,rows=14, size=1,legend_pos = "right", xlab = "Paywalled Articles", flip=TRUE, title =" ") #to get the spacing right include a blank title
  )
  rm(waffle_df,waffle_df2)
  
  return(plot1waffle)
  
}