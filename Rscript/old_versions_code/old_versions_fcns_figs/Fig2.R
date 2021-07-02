Fig2<-function(DataSet,AuPosition) {
    vars<-list(DataSet,AuPosition)
    DataSet<-as.data.frame(DataSet)
    DataSet$IncomeGroup <-as.factor(DataSet$IncomeGroup)
    DataSet$IncomeGroup <- ordered(DataSet$IncomeGroup, 
                                   levels = c("High", 
                                              "Upper middle",
                                              "Lower middle",
                                              "Low"))
    # levels(DataSet$IncomeGroup)
    # DataSet<-AllData
    # AuPosition<-"author_first"
    if ((AuPosition=="author_first")==TRUE) {
    plot2data<-as.data.frame(DataSet) %>% 
      filter(AuthorNum==1) %>% 
      group_by(IncomeGroup,JrnlType) %>% 
      drop_na("IncomeGroup") %>% 
      tally() %>% 
      mutate(percentage=n/sum(n)*100)
    
    # label_x="Journal Type"
    # label_x="Journal Type"
    title_text=paste("Fig. 2a: Percentage of articles by authors from", 
                     "different national categories that were published",
                     "in OA vs. PW journals (First Authors)",sep=" ")
    
  } else if ((AuPosition=="author_last")==TRUE) {
    plot2data<-as.data.frame(DataSet) %>% 
      group_by(DOI) %>% 
      filter(AuthorNum == max(AuthorNum)) %>% 
      group_by(IncomeGroup,JrnlType) %>% 
      drop_na("IncomeGroup") %>% 
      tally() %>% 
      mutate(percentage=n/sum(n)*100)
    
    # label_x="Journal Type"
    title_text=paste("Fig. 2b: Percentage of articles by authors from", 
                     "different national categories that were published",
                     "in OA vs. PW journals (Last Authors)",sep=" ")
    
  } else {
    stop("Please enter 'author_first' or 'author_last'")
    
  }
    # 
    # 
    # 
  
    plot2<-ggplot(plot2data, aes(x=JrnlType,
                                 y = percentage, 
                                 fill=JrnlType))+
    geom_bar(stat = "identity")+
    facet_wrap(~IncomeGroup,nrow=2, scales = "free_x")+ 
      #free_x adds the x axis line to all facets
    # xlab(label_x) +
    ylab("Percentage") +
    # scale_fill_manual(values=c("midnightblue","dimgray"),
      # name="Journal Category",
    scale_fill_manual(values=c("dimgray","dimgray"),
                      name="Journal Category",
                      breaks=c("OA", "PW"),
                      labels=c("Open Access", "Paywalled"))+
    scale_y_continuous(breaks = seq(0, 100, 20))+
    # coord_flip()+
    ggtitle(title_text)
  plot2<-plot2+
    theme_classic()+
    theme(
      axis.title.x=element_blank(),
      # axis.text.x = element_text(size=18),
      axis.text.y = element_text(size=18),
      axis.text.x = element_text(size=18),
      # axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 24, vjust=2),
      legend.position="none",
      strip.text.x = element_text(size = 20),
      panel.spacing.x =unit(2, "lines") , panel.spacing.y=unit(2,"lines"),
      plot.margin =unit(c(1,1,1,1.5), "lines")   
      #plot margin - top, right, bottom, left
      # legend.text =element_text(colour="black", size = 16),
      # legend.title =element_text(colour="black", size = 18),
    )
  plot2

  return(plot2)
  
}