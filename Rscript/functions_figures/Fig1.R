Fig1<-function(DataSet,AuPosition) {
  
    vars<-list(DataSet,AuPosition)
    DataSet<-as.data.frame(DataSet)
    DataSet$IncomeGroup <-as.factor(DataSet$IncomeGroup)
    DataSet$IncomeGroup <- ordered(DataSet$IncomeGroup, levels = c("High", "Upper middle","Lower middle","Low"))
    # levels(DataSet$IncomeGroup)
    # DataSet<-AllData
    # AuPosition<-"author_first"
    if ((AuPosition=="author_first")==TRUE) {
      first_author_income_cats<-as.data.frame(DataSet) %>% 
        filter(AuthorNum==1) %>% 
        group_by(JrnlType,IncomeGroup) %>% 
        tally() %>% 
        mutate(percentage=n/sum(n)*100)
      
      xlabeltext="First Author National Income Category"
      title_text=paste("Fig. 1a: Percentage of each article type with first", 
      "authors in different national income categories.", sep= " ")
      
    } else if ((AuPosition=="author_last")==TRUE) {
      first_author_income_cats<-as.data.frame(DataSet) %>% 
        group_by(DOI) %>% 
        filter(AuthorNum == max(AuthorNum)) %>% 
        group_by(JrnlType,IncomeGroup) %>% 
        tally() %>% 
        mutate(percentage=n/sum(n)*100)
      
      xlabeltext="Last Author National Income Category"
      title_text=paste("Fig. 1b: Percentage of each article type with last",
      "authors in different national income categories.",sep= " ")
      
    } else {
      stop("Please enter 'author_first' or 'author_last'")
      
    }
    # 
    # 
    # 
    
  # Figure
  
  labels <- c(OA = "Open Access Articles", PW = "Paywalled Articles")
  plot1<-ggplot(first_author_income_cats, aes(x=IncomeGroup,y = percentage))+
    geom_bar(stat = "identity")+
    xlab(xlabeltext) + ylab("% of Articles")+
    # scale_x_discrete(limits = bar_order)+
    facet_grid(cols = vars(JrnlType),labeller=labeller(JrnlType = labels))+
    ggtitle(title_text)
  plot1<-plot1+
    theme_classic()+
    theme(
      axis.text.x = element_text(size=18,angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size=18),
      axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 24, vjust=2),
      strip.text.x = element_text(size = 18),
      panel.spacing.x =unit(2.5, "lines") , panel.spacing.y=unit(1,"lines"),
      plot.margin =unit(c(1,1,1,1.5), "lines")   #plot margin - top, right, bottom, left
    )
  plot1
  
  return(plot1)
  
}