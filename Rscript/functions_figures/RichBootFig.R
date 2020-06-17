RichBootFig<-function(bootstrap_results,Table2.2) {
  
  bootstrap_results_fig_data<-bootstrap_results %>% 
    filter(author!="author_all") %>% 
    filter(JrnlType=="PW")
  
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(RColorBrewer)
  
  yWO_Rich<-160
  yALL_Rich<-160
  ySolo_Rich<-160
  author.labels <- c(author_first = "First Authors", author_last = "Last Authors", solo= "Single Authors")
  pRich<-
    ggplot(bootstrap_results_fig_data, aes(x=Richness,fill=JrnlType)) +
    geom_histogram(bins=40,color="black", alpha=0.8, position = 'identity') +
    scale_fill_brewer(palette = "Set1")+
    facet_grid(cols = vars(Dataset), rows=vars(author),
               labeller=labeller(author = author.labels),
               scales="free_y")+
    
    geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                               author == "author_first" & Dataset == "CHN & USA excluded"),
                 aes(x = CIhigh_Rich , y = 0, xend = CIhigh_Rich, yend = yWO_Rich),linetype="dotted")+
    geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                               author == "author_first" & Dataset == "CHN & USA excluded"),
                 aes(x = CIlow_Rich , y = 0, xend = CIlow_Rich, yend = yWO_Rich), linetype="dotted")+
    geom_segment(data = subset(filter(Table2.2,JrnlType=="OA"),
                               author == "author_first" & Dataset == "CHN & USA excluded"),
                 aes(x = RichnessOA, y = 0, xend = RichnessOA, yend = yWO_Rich), 
                 colour = "#E41A1C",linetype="solid")+
    geom_text(data = subset(filter(Table2.2,JrnlType=="OA"),
                            author == "author_first" & Dataset == "CHN & USA excluded"),
              aes(x=RichnessOA, y=yWO_Rich+15, label=(paste(labeltext,as.character(RichnessOA),sep=" == "))), 
              parse=TRUE,color="#E41A1C")+
    # geom_text(data = subset(filter(Table2.2,JrnlType=="OA"),
    #                         author == "author_first" & Dataset == "CHN & USA excludedA"),
    #           aes(x=11, y=300, label=facet_title),color="black")+
    
    geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                               author == "author_last" & Dataset == "CHN & USA excluded"),
                 aes(x = CIhigh_Rich , y = 0, xend = CIhigh_Rich, yend = yWO_Rich),linetype="dotted")+
    geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                               author == "author_last" & Dataset == "CHN & USA excluded"),
                 aes(x = CIlow_Rich , y = 0, xend = CIlow_Rich, yend = yWO_Rich), linetype="dotted")+
    geom_segment(data = subset(filter(Table2.2,JrnlType=="OA"),
                               author == "author_last" & Dataset == "CHN & USA excluded"),
                 aes(x = RichnessOA , y = 0, xend = RichnessOA, yend = yWO_Rich), 
                 parse=TRUE,color="#E41A1C")+
    geom_text(data = subset(filter(Table2.2,JrnlType=="OA"),
                            author == "author_last" & Dataset == "CHN & USA excluded"),
              aes(x=RichnessOA, y=yWO_Rich+15, label=(paste(labeltext,as.character(RichnessOA),sep=" == "))),
              parse=TRUE,color="#E41A1C")+
    
    geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                               author == "author_first" & Dataset == "All Countries"),
                 aes(x = CIhigh_Rich , y = 0, xend = CIhigh_Rich, yend = yALL_Rich),linetype="dotted")+
    geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                               author == "author_first" & Dataset == "All Countries"),
                 aes(x = CIlow_Rich , y = 0, xend = CIlow_Rich, yend = yALL_Rich), linetype="dotted")+
    geom_segment(data = subset(filter(Table2.2,JrnlType=="OA"),
                               author == "author_first" & Dataset == "All Countries"),
                 aes(x = RichnessOA , y = 0, xend = RichnessOA, yend = yALL_Rich), 
                 colour = "#E41A1C",linetype="solid")+
    geom_text(data = subset(filter(Table2.2,JrnlType=="OA"),
                            author == "author_first" & Dataset == "All Countries"),
              aes(x=RichnessOA, y=yALL_Rich+15, label=(paste(labeltext,as.character(RichnessOA),sep=" == "))),
              parse=TRUE,color="#E41A1C")+
    
    
    geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                               author == "author_last" & Dataset == "All Countries"),
                 aes(x = CIhigh_Rich , y = 0, xend = CIhigh_Rich, yend = yALL_Rich),linetype="dotted")+
    geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                               author == "author_last" & Dataset == "All Countries"),
                 aes(x = CIlow_Rich , y = 0, xend = CIlow_Rich, yend = yALL_Rich), linetype="dotted")+
    geom_segment(data = subset(filter(Table2.2,JrnlType=="OA"),
                               author == "author_last" & Dataset == "All Countries"),
                 aes(x = RichnessOA , y = 0, xend = RichnessOA, yend = yALL_Rich), 
                 colour = "#E41A1C",linetype="solid")+
    geom_text(data = subset(filter(Table2.2,JrnlType=="OA"),
                            author == "author_last" & Dataset == "All Countries"),
              aes(x=RichnessOA, y=yALL_Rich+15, label=(paste(labeltext,as.character(RichnessOA),sep=" == "))),
              parse=TRUE,color="#E41A1C")+
    
    
    geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                               author == "solo" & Dataset == "All Countries"),
                 aes(x = CIhigh_Rich , y = 0, xend = CIhigh_Rich, yend = ySolo_Rich),linetype="dotted")+
    geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                               author == "solo" & Dataset == "All Countries"),
                 aes(x = CIlow_Rich , y = 0, xend = CIlow_Rich, yend = ySolo_Rich), 
                 linetype="dotted")+
    geom_segment(data = subset(filter(Table2.2,JrnlType=="OA"),
                               author == "solo" & Dataset == "All Countries"),
                 aes(x = RichnessOA , y = 0, xend = RichnessOA, yend = ySolo_Rich), 
                 colour = "#E41A1C",linetype="solid")+
    geom_text(data = subset(filter(Table2.2,JrnlType=="OA"),
                            author == "solo" & Dataset == "All Countries"),
              aes(x=RichnessOA, y=ySolo_Rich+15, label=(paste(labeltext,as.character(RichnessOA),sep=" == "))),
              parse=TRUE,color="#E41A1C")+
    
    
    geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                               author == "solo" & Dataset == "CHN & USA excluded"),
                 aes(x = CIhigh_Rich , y = 0, xend = CIhigh_Rich, yend = ySolo_Rich),linetype="dotted")+
    geom_segment(data = subset(filter(Table2.2,JrnlType=="PW"),
                               author == "solo" & Dataset == "CHN & USA excluded"),
                 aes(x = CIlow_Rich , y = 0, xend = CIlow_Rich, yend = ySolo_Rich), 
                 linetype="dotted")+
    geom_segment(data = subset(filter(Table2.2,JrnlType=="OA"),
                               author == "solo" & Dataset == "CHN & USA excluded"),
                 aes(x = RichnessOA , y = 0, xend = RichnessOA, yend = ySolo_Rich), 
                 colour = "#E41A1C",linetype="solid")+
    geom_text(data = subset(filter(Table2.2,JrnlType=="OA"),
                            author == "solo" & Dataset == "CHN & USA excluded"),
              aes(x=RichnessOA, y=ySolo_Rich+15, label=(paste(labeltext,as.character(RichnessOA),sep=" == "))),
              parse=TRUE,color="#E41A1C")+
    guides(fill=guide_legend("Journal\nCategory"))+
    scale_x_continuous(breaks = seq(0,70, by=10),expand=c(0.1,0.02))+
    # scale_y_continuous(limits = c(0, 325),breaks = seq(0,325, by=50),expand=c(0,0.1))+
    # scale_y_continuous(expand = c(0,0))+
    xlab("Author Geographic Richness: Paywalled Journals")+
    ylab("Frequency")
  
  pRich<-pRich+
    theme_classic()+ 
    theme(
      axis.text.x = element_text(size=18),
      axis.text.y = element_text(size=18),
      axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 24, hjust=0.5,),
      strip.text.x = element_text(size = 18,face="bold"),
      strip.text.y = element_text(size = 18,face="bold"),
      legend.title = element_text(colour="black", size=18, 
                                  face="bold"),
      legend.text = element_text(colour="black", size=14),
      legend.position = ("none"),
      panel.spacing.x =unit(1.5, "lines"), 
      panel.spacing.y=unit(3,"lines"),
      # strip.text.y.right = element_text(angle = 0),
      plot.margin =unit(c(1,1,1,1.5), "lines")  
    )
  pRich
  
  return(pRich)
}