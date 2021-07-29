DivBootFig<-function(bootstrap_results) {
  
  
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(RColorBrewer)
  library(egg)
  
  sole_NOCHNUSA<-read_csv("./data_clean/one_author_pubsNOCHNUSA.csv")
  coauthor_NOCHNUSA<-read_csv("./data_clean/coauthor_pubsNOCHNUSA.csv")
  sole_ALL<-read_csv("./data_clean/sole_author_pubs_ALL_first_author.csv")
  coauthor_ALL<-read_csv("./data_clean/coauthor_pubs_ALL_first_author.csv")
  
  source("./Rscript/functions/DivRichCalc.R")
  OAdiv_coauthor_ALL<-DivRichCalc(coauthor_ALL,"author_first","OA")
  OAdiv_coauthor_ALL<-as.numeric(OAdiv_coauthor_ALL[2])
  
  OAdiv_coauthor_NOCHNUSA<-DivRichCalc(coauthor_NOCHNUSA,"author_first","OA")
  OAdiv_coauthor_NOCHNUSA<-as.numeric(OAdiv_coauthor_NOCHNUSA[2])
  
  OAdiv_sole_ALL<-DivRichCalc(sole_ALL,"author_first","OA")
  OAdiv_sole_ALL<-as.numeric(OAdiv_sole_ALL[2])
  
  OAdiv_sole_NOCHNUSA<-DivRichCalc(sole_NOCHNUSA,"author_first","OA")
  OAdiv_sole_NOCHNUSA<-as.numeric(OAdiv_sole_NOCHNUSA[2])
  
  OA_Diversity<-c(OAdiv_coauthor_ALL,OAdiv_coauthor_NOCHNUSA,
                 OAdiv_sole_ALL,OAdiv_sole_NOCHNUSA)
  OA_Diversity<-as_tibble(OA_Diversity)
  OA_Diversity<-dplyr::rename(OA_Diversity,"OA_Diversity"="value")
  
  means_bootstrap_results<-bootstrap_results %>% 
    group_by(author,Dataset) %>% 
    summarize(mean(InvSimp))
  
  figure_values<-bind_cols(means_bootstrap_results,OA_Diversity)
  figure_values
  names(figure_values)<-c("author","Dataset", "mean_Div", "OA_Div")
  figure_values$JrnlType<-"PW"
  figure_values$mean_Div<-round(figure_values$mean_Div,digits=1)
  figure_values$OA_Div<-round(figure_values$OA_Div,digits=1)
  summary(as.factor(bootstrap_results$author))
  summary(as.factor(bootstrap_results$Dataset))
  summary(as.factor(bootstrap_results$JrnlType))
  
  yWO_Div<-350
  yALL_Div<-350
  ySolo_Div<-350
  author.labels <- c(author_first = "First Authors", solo= "Single Authors")
  
  
  bootstrap_results$Dataset<-gsub("CHN & USA excluded", "Without China & USA",bootstrap_results$Dataset)
  figure_values$Dataset<-gsub("CHN & USA excluded", "Without China & USA",figure_values$Dataset)
  
  
  bootstrap_results$author <- factor(bootstrap_results$author,
                                     levels = c("solo","author_first"))
  figure_values$author <- factor(figure_values$author,
                                 levels = c("solo","author_first"))
  
  
  pDiv<-
    ggplot(bootstrap_results, aes(x=InvSimp,fill=JrnlType)) +
    geom_histogram(bins=40, color="black",fill="darkgray",
                   size=0.1,alpha=0.4, position = 'identity') +
    facet_grid(cols = vars(Dataset), rows=vars(author),
               labeller=labeller(author = author.labels),
               scales="free_y")+
    geom_hline((aes(yintercept=-Inf)), color="black") +
    geom_vline((aes(xintercept=-Inf)) , color="black")+
    guides(fill=guide_legend("Journal\nCategory"))+
    scale_x_continuous(breaks = seq(0,70, by=10),expand=c(0.1,0.02))+
    scale_y_continuous(limits = c(0, 375),breaks = seq(0,350, by=50),expand=c(0,0.1))+
    # scale_y_continuous(expand = c(0,0))+
    xlab("Geographic Diversity, D (Reciprocal of Simpson's Index)")+
    
    labs(x = "Geographic Diversity", y = "Frequency") +
    
  
    coord_cartesian(clip="off")+
    # ylim(0,170)+
    # median OF BOOTSTRAP
    geom_segment(data = subset(filter(figure_values, author == "author_first" & Dataset == "All Countries")),
                 aes(x = mean_Div, y = 0, xend = mean_Div, yend = yALL_Div), linetype="solid")+
    geom_text(data = subset(filter(figure_values, author == "author_first" & Dataset == "All Countries")),
              aes(x=mean_Div-1.8, y=yALL_Div+15, label=(paste("PW['mean']",as.character(mean_Div),sep=" == "))), 
              parse=TRUE,size=2)+
    
    # OA Value
    geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries")),
                 aes(x = OA_Div , y = 0, xend = OA_Div, yend = yALL_Div), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries")),
              aes(x=OA_Div+2, y=yALL_Div+15, label=(paste("OA",as.character(OA_Div),sep=" == "))),
              parse=TRUE,color="red", size=2)+
    # MEAN OF BOOTSTRAP
    geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries")),
                 aes(x = mean_Div, y = 0, xend = mean_Div, yend = yALL_Div), linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries")),
              aes(x=mean_Div+3, y=yALL_Div+15, label=(paste("PW['mean']",as.character(mean_Div),sep=" == "))), 
              parse=TRUE,size=2)+
    # OA Value
    
    geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries")),
                 aes(x = OA_Div , y = 0, xend = OA_Div, yend = ySolo_Div), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries")),
              aes(x=OA_Div-2, y=ySolo_Div+15, label=(paste("OA",as.character(OA_Div),sep=" == "))),
              parse=TRUE,color="red", size=2)+
    
    # MEAN OF BOOTSTRAP
    geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "Without China & USA")),
                 aes(x = mean_Div, y = 0, xend = mean_Div, yend = yWO_Div), linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "Without China & USA")),
              aes(x=mean_Div+3, y=yWO_Div+15, label=(paste("PW['mean']",as.character(mean_Div),sep=" == "))), 
              parse=TRUE,size=2)+
    
    # OA Value
    geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "Without China & USA")),
                 aes(x = OA_Div, y = 0, xend = OA_Div, yend = yWO_Div), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "Without China & USA")),
              aes(x=OA_Div-2, y=yWO_Div+15, label=(paste("OA",as.character(OA_Div),sep=" == "))), 
              parse=TRUE,color="red", size=2)+
    # MEAN OF BOOTSTRAP
    geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "Without China & USA")),
                 aes(x = mean_Div, y = 0, xend = mean_Div, yend = yWO_Div), linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "Without China & USA")),
              aes(x=mean_Div+3, y=yWO_Div+15, label=(paste("PW['mean']",as.character(mean_Div),sep=" == "))), 
              parse=TRUE,size=2)+
    
    # OA Value
    geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "Without China & USA")),
                 aes(x = OA_Div , y = 0, xend = OA_Div, yend = ySolo_Div), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "Without China & USA")),
              aes(x=OA_Div-2, y=ySolo_Div+15, label=(paste("OA",as.character(OA_Div),sep=" == "))),
              parse=TRUE,color="red", size=2)
  
  pDiv<-pDiv+
    theme_classic()+ 
    theme(
      axis.text.x = element_text(size=8),
      axis.text.y = element_text(size=8),
      axis.title.x=element_text(colour="black", size = 10, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 10, hjust=0.5,),
      strip.text.x = element_text(size = 10,margin = margin(0,0,3,0, "lines")),
      strip.text.y = element_text(size = 10, angle=0),
      strip.background.y = element_rect(fill = NA, colour = NA),
      strip.background.x = element_rect(fill = NA, colour = NA),
      legend.title = element_text(colour="black", size=60),
      legend.text = element_text(colour="black", size=60),
      legend.position = ("none"),
      panel.spacing.x =unit(1.0, "lines"), 
      panel.spacing.y=unit(2,"lines"),
      # strip.text.y.right = element_text(angle = 0),
      plot.margin =unit(c(1,1,1,1.5), "lines")  
    )
  
  facet_labels<-c("A","B","C","D")
  pDiv<-tag_facet(pDiv,open="", close="", tag_pool=facet_labels,vjust=-1)
  pDiv
  
  
  
  
  
  
  ##################################################################################
  # DIVERISTY
  ############################################################################
  figure_values<-ungroup(figure_values)
  P_Hat<-figure_values
  P_Hat$P_Hat<-NA
  P_Hat$JrnlType<-NULL
  ##########
  # All countries, coauthored
  crit<-figure_values %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    select(OA_Div)
  
  perc<-bootstrap_results %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    ungroup() %>% 
    tally(InvSimp<crit$OA_Div) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  P_Hat$P_Hat[P_Hat$author=="author_first" & 
                P_Hat$Dataset=="All Countries"]<-perc_belowOA
  ###########
  
  
  ##########
  # without USA CHN, coauthored
  crit<-figure_values %>% 
    filter(Dataset=="Without China & USA") %>% 
    filter(author=="author_first") %>% 
    select(OA_Div)
  
  perc<-bootstrap_results %>% 
    filter(Dataset=="Without China & USA") %>% 
    filter(author=="author_first") %>% 
    ungroup() %>% 
    tally(InvSimp<crit$OA_Div) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  P_Hat$P_Hat[P_Hat$author=="author_first" & 
                P_Hat$Dataset=="Without China & USA"]<-perc_belowOA
  ###########
  
  ##########
  # All countries, coauthored
  crit<-figure_values %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="solo") %>% 
    select(OA_Div)
  
  perc<-bootstrap_results %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="solo") %>% 
    ungroup() %>% 
    tally(InvSimp<crit$OA_Div) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  P_Hat$P_Hat[P_Hat$author=="solo" & 
                P_Hat$Dataset=="All Countries"]<-perc_belowOA
  ###########
  
  
  ##########
  # without USA CHN, coauthored
  crit<-figure_values %>% 
    filter(Dataset=="Without China & USA") %>% 
    filter(author=="solo") %>% 
    select(OA_Div)
  
  perc<-bootstrap_results %>% 
    filter(Dataset=="Without China & USA") %>% 
    filter(author=="solo") %>% 
    ungroup() %>% 
    tally(InvSimp<crit$OA_Div) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  P_Hat$P_Hat[P_Hat$author=="solo" & 
                P_Hat$Dataset=="Without China & USA"]<-perc_belowOA
  
  P_Hat<-P_Hat %>% arrange(Dataset,desc(author))
  
  #########
  
  
  return(list(pDiv,P_Hat))
}
