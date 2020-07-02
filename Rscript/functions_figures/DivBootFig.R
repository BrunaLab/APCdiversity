DivBootFig<-function(bootstrap_results,Table2.2) {
  
  bootstrap_results_fig_data<-bootstrap_results %>% 
    filter(author!="author_all") %>% 
    filter(JrnlType=="PW")
  
  
  
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(RColorBrewer)
  
  # geom_text is scaled differently, no idea why. convert to correct using this:(14/5)*3
  
  
  
  
  
  yWO_div<-135
  yALL_div<-300
  author.labels <- c(author_first = "First Authors", author_last = "Last Authors", solo= "Single Authors")
  # bootstrap_results_fig_data$author<-as.factor(bootstrap_results_fig_data$author)
  # bootstrap_results_fig_data$Dataset<-as.factor(bootstrap_results_fig_data$Dataset)
  # bootstrap_results_fig_data$JrnlType<-as.factor(bootstrap_results_fig_data$JrnlType)
    pDiv<-
    ggplot(bootstrap_results_fig_data, aes(x=InvSimp,fill=JrnlType)) +
    geom_histogram(bins=60,color="black", size=0.1,alpha=0.4, position = 'identity') +
    scale_fill_brewer(palette = "Set1")+
    facet_grid(cols = vars(author), rows=vars(Dataset),
               labeller=labeller(author=author.labels),
               scales="free_y")+
      
      
      # FIRST AUTHOR, ALL
      # CONFIDENCE INTERVALS    
      # geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
      #                          author == "author_first" & Dataset == "All Countries"),
      #            aes(x = CIhigh_Div , y = 0, xend = CIhigh_Div, yend = yALL_div),linetype="dotted")+
      # geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
      #                          author == "author_first" & Dataset == "All Countries"),
      #            aes(x = CIlow_Div , y = 0, xend = CIlow_Div, yend = yALL_div), 
      #            linetype="dotted")+
      
      # MEAN OF BOOTSTRAP
      geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                                 author == "author_first" & Dataset == "All Countries"),
                   aes(x = Mean_Div, y = 0, xend = Mean_Div, yend = yALL_div), linetype="solid")+
      geom_text(data = subset(filter(Table2.2,JrnlType=="PW"),
                              author == "author_first" & Dataset == "All Countries"),
                aes(x=Mean_Div, y=yALL_div+5, label=(paste("PW['mean']",as.character(Mean_Div),sep=" == "))), 
                parse=TRUE,size=2)+
      
      # OA Value
      geom_segment(data = subset(filter(Table2.2,JrnlType=="OA"),
                               author == "author_first" & Dataset == "All Countries"),
                 aes(x = InvSimpOA , y = 0, xend = InvSimpOA, yend = yALL_div), 
                 colour = "#E41A1C",linetype="solid")+
      geom_text(data = subset(filter(Table2.2,JrnlType=="OA"),
                            author == "author_first" & Dataset == "All Countries"),
              aes(x=InvSimpOA, y=yALL_div+5, label=(paste(labeltext,as.character(InvSimpOA),sep=" == "))),
              parse=TRUE,color="#E41A1C", size=2)+
    
      # LAST AUTHOR, ALL  
      # CONFIDENCE INTERVALS  
      # geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
      #                          author == "author_last" & Dataset == "All Countries"),
      #            aes(x = CIhigh_Div , y = 0, xend = CIhigh_Div, yend = yALL_div),linetype="dotted")+
      # geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
      #                          author == "author_last" & Dataset == "All Countries"),
      #            aes(x = CIlow_Div , y = 0, xend = CIlow_Div, yend = yALL_div), 
      #            linetype="dotted")+
      
      
      # MEAN OF BOOTSTRAP
      geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                                 author == "author_last" & Dataset == "All Countries"),
                   aes(x = Mean_Div, y = 0, xend = Mean_Div, yend = yALL_div), linetype="solid")+
      geom_text(data = subset(filter(Table2.2,JrnlType=="PW"),
                              author == "author_last" & Dataset == "All Countries"),
                aes(x=Mean_Div+1.5, y=yALL_div+5, label=(paste("PW['mean']",as.character(Mean_Div),sep=" == "))), 
                parse=TRUE,size=2)+
      
      
      # OA Value
      geom_segment(data = subset(filter(Table2.2,JrnlType=="OA"),
                               author == "author_last" & Dataset == "All Countries"),
                 aes(x = InvSimpOA , y = 0, xend = InvSimpOA, yend = yALL_div), 
                 colour = "#E41A1C",linetype="solid")+
      geom_text(data = subset(filter(Table2.2,JrnlType=="OA"),
                            author == "author_last" & Dataset == "All Countries"),
              aes(x=InvSimpOA-1.5, y=yALL_div+5, label=(paste(labeltext,as.character(InvSimpOA),sep=" == "))),
              parse=TRUE,color="#E41A1C", size=2)+
    
      # SOLO, ALL
      # MEAN OF BOOTSTRAP
      geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                                 author == "solo" & Dataset == "All Countries"),
                   aes(x = Mean_Div, y = 0, xend = Mean_Div, yend = yALL_div), linetype="solid")+
      geom_text(data = subset(filter(Table2.2,JrnlType=="PW"),
                              author == "solo" & Dataset == "All Countries"),
                aes(x=Mean_Div-1.5, y=yALL_div+5, label=(paste("PW['mean']",as.character(Mean_Div),sep=" == "))), 
                parse=TRUE,size=2)+
      
      
      # CONFIDENCE INTERVALS
      # geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
      #                          author == "solo" & Dataset == "All Countries"),
      #            aes(x = CIhigh_Div , y = 0, xend = CIhigh_Div, yend = yALL_div),linetype="dotted")+
      # geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
      #                          author == "solo" & Dataset == "All Countries"),
      #            aes(x = CIlow_Div , y = 0, xend = CIlow_Div, yend = yALL_div), 
      #            linetype="dotted")+
      
      # OA Value
      geom_segment(data = subset(filter(Table2.2,JrnlType=="OA"),
                               author == "solo" & Dataset == "All Countries"),
                 aes(x = InvSimpOA , y = 0, xend = InvSimpOA, yend = yALL_div), 
                 colour = "#E41A1C",linetype="solid")+
      geom_text(data = subset(filter(Table2.2,JrnlType=="OA"),
                            author == "solo" & Dataset == "All Countries"),
              aes(x=InvSimpOA+1.5, y=yALL_div+5, label=(paste(labeltext,as.character(InvSimpOA),sep=" == "))),
              parse=TRUE,color="#E41A1C", size=2)+
      
      # FIRST AUTHOR, NO CHN & USA
      # CONFIDENCE INTERVALS
      # geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
      #                            author == "author_first" & Dataset == "CHN & USA excluded"),
      #              aes(x = CIhigh_Div , y = 0, xend = CIhigh_Div, yend = yWO_div),linetype="dotted")+
      # geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
      #                            author == "author_first" & Dataset == "CHN & USA excluded"),
      #              aes(x = CIlow_Div , y = 0, xend = CIlow_Div, yend = yWO_div), linetype="dotted")+
      # MEAN OF BOOTSTRAP
      geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                                 author == "author_first" & Dataset == "CHN & USA excluded"),
                   aes(x = Mean_Div, y = 0, xend = Mean_Div, yend = yWO_div), linetype="solid")+
      geom_text(data = subset(filter(Table2.2,JrnlType=="PW"),
                              author == "author_first" & Dataset == "CHN & USA excluded"),
                aes(x=Mean_Div+1.5, y=yWO_div+4, label=(paste("PW['mean']",as.character(Mean_Div),sep=" == "))), 
                parse=TRUE,size=2)+
      
      # OA Value
      geom_segment(data = subset(filter(Table2.2,JrnlType=="OA"),
                                 author == "author_first" & Dataset == "CHN & USA excluded"),
                   aes(x = InvSimpOA, y = 0, xend = InvSimpOA, yend = yWO_div), 
                   colour = "#E41A1C",linetype="solid")+
      geom_text(data = subset(filter(Table2.2,JrnlType=="OA"),
                              author == "author_first" & Dataset == "CHN & USA excluded"),
                aes(x=InvSimpOA-1.5, y=yWO_div+4, label=(paste(labeltext,as.character(InvSimpOA),sep=" == "))), 
                parse=TRUE,color="#E41A1C", size=2)+
      
      
      # LAST AUTHOR, NO CHN & USA
      # CONFIDENCE INTERVALS
      # geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
      #                            author == "author_last" & Dataset == "CHN & USA excluded"),
      #              aes(x = CIhigh_Div , y = 0, xend = CIhigh_Div, yend = yWO_div),linetype="dotted")+
      # geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
      #                            author == "author_last" & Dataset == "CHN & USA excluded"),
      #              aes(x = CIlow_Div , y = 0, xend = CIlow_Div, yend = yWO_div), linetype="dotted")+
      
      # MEAN OF BOOTSTRAP
      geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                                 author == "author_last" & Dataset == "CHN & USA excluded"),
                   aes(x = Mean_Div, y = 0, xend = Mean_Div, yend = yWO_div), linetype="solid")+
      geom_text(data = subset(filter(Table2.2,JrnlType=="PW"),
                              author == "author_last" & Dataset == "CHN & USA excluded"),
                aes(x=Mean_Div, y=yWO_div+4, label=(paste("PW['mean']",as.character(Mean_Div),sep=" == "))), 
                parse=TRUE,size=2)+
      
      # OA Value
      geom_segment(data = subset(filter(Table2.2,JrnlType=="OA"),
                                 author == "author_last" & Dataset == "CHN & USA excluded"),
                   aes(x = InvSimpOA , y = 0, xend = InvSimpOA, yend = yWO_div), 
                   parse=TRUE,color="#E41A1C")+
      geom_text(data = subset(filter(Table2.2,JrnlType=="OA"),
                              author == "author_last" & Dataset == "CHN & USA excluded"),
                aes(x=InvSimpOA, y=yWO_div+4, label=(paste(labeltext,as.character(InvSimpOA),sep=" == "))),
                parse=TRUE,color="#E41A1C", size=2)+
      
      # SOLO, NO CHN & USA
      # CONFIDENCE INTERVALS
      # geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
      #                          author == "solo" & Dataset == "CHN & USA excluded"),
      #            aes(x = CIhigh_Div , y = 0, xend = CIhigh_Div, yend = yWO_div),linetype="dotted")+
      # geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
      #                          author == "solo" & Dataset == "CHN & USA excluded"),
      #            aes(x = CIlow_Div , y = 0, xend = CIlow_Div, yend = yWO_div), 
      #            linetype="dotted")+
      
      # MEAN OF BOOTSTRAP
      geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                                 author == "solo" & Dataset == "CHN & USA excluded"),
                   aes(x = Mean_Div, y = 0, xend = Mean_Div, yend = yWO_div), linetype="solid")+
      geom_text(data = subset(filter(Table2.2,JrnlType=="PW"),
                              author == "solo" & Dataset == "CHN & USA excluded"),
                aes(x=Mean_Div+2, y=yWO_div+4, label=(paste("PW['mean']",as.character(Mean_Div),sep=" == "))), 
                parse=TRUE,size=2)+
      
      # OA Value
      geom_segment(data = subset(filter(Table2.2,JrnlType=="OA"),
                               author == "solo" & Dataset == "CHN & USA excluded"),
                 aes(x = InvSimpOA-.1 , y = 0, xend = InvSimpOA+.1, yend = yWO_div), #added +1 to dodge the line
                 colour = "#E41A1C",linetype="solid")+
      geom_text(data = subset(filter(Table2.2,JrnlType=="OA"),
                            author == "solo" & Dataset == "CHN & USA excluded"),
              aes(x=InvSimpOA-2, y=yWO_div+4, label=(paste(labeltext,as.character(InvSimpOA),sep=" == "))),
              parse=TRUE,color="#E41A1C", size=2)+
    
    
    
    guides(fill=guide_legend("Journal\nCategory"))+
    scale_x_continuous(breaks = seq(0,27, by=3),expand=c(0.1,0.02))+
    # scale_y_continuous(limits = c(0, 325),breaks = seq(0,325, by=50),expand=c(0,0.1))+
    # scale_y_continuous(expand = c(0,0.1))+
  
    xlab("Geographic Diversity")+
    ylab("Frequency")
  
  pDiv<-pDiv+
    theme_classic()+ 
    theme(
      axis.text.x = element_text(size=8),
      axis.text.y = element_text(size=8),
      axis.title.x=element_text(colour="black", size = 10, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 10, hjust=0.5,),
      strip.text.x = element_text(size = 8,face="bold"),
      strip.text.y = element_text(size = 8,face="bold"),
      legend.title = element_text(colour="black", size=10, 
                                  face="bold"),
      legend.text = element_text(colour="black", size=10),
      legend.position = ("none"),
      panel.spacing.x =unit(1.0, "lines"), 
      panel.spacing.y=unit(2,"lines"),
      # strip.text.y.right = element_text(angle = 0),
      plot.margin =unit(c(1,1,1,1.5), "lines")  
    )
  pDiv
  
  return(pDiv)
}