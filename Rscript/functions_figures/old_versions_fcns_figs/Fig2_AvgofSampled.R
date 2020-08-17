Fig2_AvgofSampled<-function(DataSet,AuPosition) {
  # AuPosition<-"author_first"
  # DataSet<-Subsampled_Countries_summary
  vars<-list(DataSet,AuPosition)
  DataSet$IncomeGroup<-ordered(DataSet$IncomeGroup, 
                               levels = c("High", 
                                          "Upper middle",
                                          "Lower middle",
                                          "Low"))  
    # levels(DataSet$IncomeGroup)
    # DataSet<-AllData
    # AuPosition<-"author_first"
    if ((AuPosition=="author_first")==TRUE) {
    plot2data<-as.data.frame(DataSet) %>% 
      filter(AuthorNum==1) 
    
    # label_x="Journal Type"
    # label_x="Journal Type"
    title_text=paste("Fig. 2a: Avergae Percentage of articles by authors from", 
                     "different national categories that were published",
                     "in PW journals (First Authors,1000 reps)",sep=" ")
    
  } else if ((AuPosition=="author_last")==TRUE) {
    plot2data<-as.data.frame(DataSet) %>% 
      filter(AuthorNum == "last")
    
    # label_x="Journal Type"
    title_text=paste("Fig. 2b: Percentage of articles by authors from", 
                     "different national categories that were published",
                     "in PW journals (Last Authors,1000 reps)",sep=" ")
    
  } else {
    stop("Please enter 'author_first' or 'author_last'")
    
  }
  
  
  
  # labels <- c(OA = "Open Access Articles", PW = "Paywalled Articles")
  plot1_avg<-ggplot(plot2data, aes(x=IncomeGroup,y = avg_perc))+
    geom_bar(stat="identity", color="black") +
    geom_errorbar(aes(ymin=avg_perc-sd_perc, ymax=avg_perc+sd_perc), width=.2)+
    xlab(xlabeltext) + ylab("% of Articles")+
    # scale_x_discrete(limits = bar_order)+
    # facet_grid(cols = vars(JrnlType),labeller=labeller(JrnlType = labels))+
    ggtitle(title_text)
  plot1_avg<-plot1_avg+
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
  plot1_avg
  
  return(plot1_avg)
    # 
    # 
    # 
  
  #   plot2<-ggplot(plot2data, aes(x=IncomeGroup,
  #                                y = avg))+
  #     geom_bar(stat="identity", color="black") +
  #     geom_errorbar(aes(ymin=avg-sd, ymax=avg+sd), width=.2)+ 
  # #   # facet_wrap(~IncomeGroup,nrow=2, scales = "free_x")+ 
  # #     #free_x adds the x axis line to all facets
  # #   # xlab(label_x) +
  #    ylab("Percentage") +
  # #   # scale_fill_manual(values=c("midnightblue","dimgray"),
  # #     # name="Journal Category",
  # #   # scale_fill_manual(values=c("dimgray","dimgray"),
  # #   #                   name="Journal Category",
  # #   #                   breaks=c("OA", "PW"),
  # #   #                   labels=c("Open Access", "Paywalled"))+
  #    scale_y_continuous(breaks = seq(0, 100, 20))
  # #   # coord_flip()+
  # #   ggtitle(title_text)
  # plot2<-plot2+
  #   theme_classic()+
  #   theme(
  #     axis.title.x=element_blank(),
  #     # axis.text.x = element_text(size=18),
  #     axis.text.y = element_text(size=18),
  #     axis.text.x = element_text(size=18),
  #     # axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
  #     axis.title.y=element_text(colour="black", size = 24, vjust=2),
  #     legend.position="none",
  # #     strip.text.x = element_text(size = 20),
  # #     panel.spacing.x =unit(2, "lines") , panel.spacing.y=unit(2,"lines"),
  # #     plot.margin =unit(c(1,1,1,1.5), "lines")   
  # #     #plot margin - top, right, bottom, left
  # #     # legend.text =element_text(colour="black", size = 16),
  # #     # legend.title =element_text(colour="black", size = 18),
  #   )
  # plot2
  # 
  # return(plot2)
  
}