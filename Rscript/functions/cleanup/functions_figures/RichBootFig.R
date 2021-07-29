RichBootFig<-function(bootstrap_results) {
  
  
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
  OArich_coauthor_ALL<-DivRichCalc(coauthor_ALL,"author_first","OA")
  OArich_coauthor_ALL<-as.numeric(OArich_coauthor_ALL[1])
  
  OArich_coauthor_NOCHNUSA<-DivRichCalc(coauthor_NOCHNUSA,"author_first","OA")
  OArich_coauthor_NOCHNUSA<-as.numeric(OArich_coauthor_NOCHNUSA[1])
  
  OArich_sole_ALL<-DivRichCalc(sole_ALL,"author_first","OA")
  OArich_sole_ALL<-as.numeric(OArich_sole_ALL[1])
  
  OArich_sole_NOCHNUSA<-DivRichCalc(sole_NOCHNUSA,"author_first","OA")
  OArich_sole_NOCHNUSA<-as.numeric(OArich_sole_NOCHNUSA[1])
  
  OA_Richness<-c(OArich_coauthor_ALL,OArich_coauthor_NOCHNUSA,
                 OArich_sole_ALL,OArich_sole_NOCHNUSA)
  OA_Richness<-as_tibble(OA_Richness)
  OA_Richness<-dplyr::rename(OA_Richness,"OA_Richness"="value")
  
  mean_bootstrap_results<-bootstrap_results %>% 
    group_by(author,Dataset) %>% 
    summarize(mean(Richness))
  
  figure_values<-bind_cols(mean_bootstrap_results,OA_Richness)
  figure_values
  names(figure_values)<-c("author","Dataset", "mean_Rich", "OA_Rich")
  figure_values$JrnlType<-"PW"
  figure_values$mean_Rich<-round(figure_values$mean_Rich,digits=1)
  summary(as.factor(bootstrap_results$author))
  summary(as.factor(bootstrap_results$Dataset))
  summary(as.factor(bootstrap_results$JrnlType))
  
  yWO_Rich<-150
  yALL_Rich<-150
  ySolo_Rich<-150
  author.labels <- c(author_first = "First Authors", solo= "Single Authors")
  bootstrap_results$Dataset<-gsub("CHN & USA excluded", "Without China & USA",bootstrap_results$Dataset)
  figure_values$Dataset<-gsub("CHN & USA excluded", "Without China & USA",figure_values$Dataset)
  
  
  bootstrap_results$author <- factor(bootstrap_results$author,
                                  levels = c("solo","author_first"))
  figure_values$author <- factor(figure_values$author,
                                     levels = c("solo","author_first"))
  
  pRich<-
    ggplot(bootstrap_results, aes(x=Richness,fill=JrnlType)) +
    geom_histogram(bins=40, color="black",fill="darkgray",
                   size=0.1,alpha=0.4, position = 'identity') +
    facet_grid(cols = vars(Dataset), rows=vars(author),
               labeller=labeller(author = author.labels),
               scales="free_y")+
    geom_hline((aes(yintercept=-Inf)), color="black") +
    geom_vline((aes(xintercept=-Inf)) , color="black")+
    
    guides(fill=guide_legend("Journal\nCategory"))+
    scale_x_continuous(breaks = seq(0,70, by=10),expand=c(0.1,0.02))+
    scale_y_continuous(limits = c(0, 160),breaks = seq(0,150, by=25),expand=c(0,0.1))+
    # scale_y_continuous(expand = c(0,0))+
    xlab("Geographic Richness (R)")+
    ylab("Frequency")+
    coord_cartesian(clip="off")+
    # ylim(0,180)+
    # median OF BOOTSTRAP
    geom_segment(data = subset(filter(figure_values, author == "author_first" & Dataset == "All Countries")),
                 aes(x = mean_Rich, y = 0, xend = mean_Rich, yend = yALL_Rich), linetype="solid")+
    geom_text(data = subset(filter(figure_values, author == "author_first" & Dataset == "All Countries")),
              aes(x=mean_Rich+6, y=yALL_Rich+5, label=(paste("PW['mean']",as.character(mean_Rich),sep=" == "))), 
              parse=TRUE,size=2)+
    
    # OA Value
     geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries")),
                 aes(x = OA_Rich , y = 0, xend = OA_Rich, yend = yALL_Rich), 
                 colour = "red",linetype="solid")+
      geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries")),
                            aes(x=OA_Rich-3, y=yALL_Rich+5, label=(paste("OA",as.character(OA_Rich),sep=" == "))),
              parse=TRUE,color="red", size=2)+
    # MEAN OF BOOTSTRAP
    geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries")),
                 aes(x = mean_Rich, y = 0, xend = mean_Rich, yend = yALL_Rich), linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries")),
              aes(x=mean_Rich+5, y=yALL_Rich+6, label=(paste("PW['mean']",as.character(mean_Rich),sep=" == "))), 
              parse=TRUE,size=2)+
    # OA Value
    
    geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries")),
                 aes(x = OA_Rich , y = 0, xend = OA_Rich, yend = ySolo_Rich), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries")),
                      aes(x=OA_Rich-3, y=ySolo_Rich+5, label=(paste("OA",as.character(OA_Rich),sep=" == "))),
              parse=TRUE,color="red", size=2)+
    
    # MEAN OF BOOTSTRAP
    geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "Without China & USA")),
                 aes(x = mean_Rich, y = 0, xend = mean_Rich, yend = yWO_Rich), linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "Without China & USA")),
              aes(x=mean_Rich+6, y=yWO_Rich+5, label=(paste("PW['mean']",as.character(mean_Rich),sep=" == "))), 
              parse=TRUE,size=2)+
    
    # OA Value
    geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "Without China & USA")),
                 aes(x = OA_Rich, y = 0, xend = OA_Rich, yend = yWO_Rich), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "Without China & USA")),
              aes(x=OA_Rich-3, y=yWO_Rich+5, label=(paste("OA",as.character(OA_Rich),sep=" == "))), 
              parse=TRUE,color="red", size=2)+
    # MEAN OF BOOTSTRAP
    geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "Without China & USA")),
                 aes(x = mean_Rich, y = 0, xend = mean_Rich, yend = yWO_Rich), linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "Without China & USA")),
              aes(x=mean_Rich+6, y=yWO_Rich+5, label=(paste("PW['mean']",as.character(mean_Rich),sep=" == "))), 
              parse=TRUE,size=2)+
    
    # OA Value
     geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "Without China & USA")),
                 aes(x = OA_Rich , y = 0, xend = OA_Rich, yend = ySolo_Rich), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "Without China & USA")),
              aes(x=OA_Rich-3, y=ySolo_Rich+5, label=(paste("OA",as.character(OA_Rich),sep=" == "))),
              parse=TRUE,color="red", size=2)
  
  pRich<-pRich+
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
  pRich
  facet_labels<-c("A","B","C","D")
  pRich<-tag_facet(pRich, open="", close="", tag_pool=facet_labels,vjust=-1)
  pRich
  
  
  
  figure_values<-ungroup(figure_values)
  P_Hat<-figure_values
  P_Hat$P_Hat<-NA
  P_Hat$JrnlType<-NULL
  ##########
  # All countries, coauthored
  crit<-figure_values %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    select(OA_Rich)
  
  perc<-bootstrap_results %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    ungroup() %>% 
    tally(Richness<crit$OA_Rich) %>% 
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
    select(OA_Rich)
  
  perc<-bootstrap_results %>% 
    filter(Dataset=="Without China & USA") %>% 
    filter(author=="author_first") %>% 
    ungroup() %>% 
    tally(Richness<crit$OA_Rich) %>% 
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
    select(OA_Rich)
  
  perc<-bootstrap_results %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="solo") %>% 
    ungroup() %>% 
    tally(Richness<crit$OA_Rich) %>% 
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
    select(OA_Rich)
  
  perc<-bootstrap_results %>% 
    filter(Dataset=="Without China & USA") %>% 
    filter(author=="solo") %>% 
    ungroup() %>% 
    tally(Richness<crit$OA_Rich) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  P_Hat$P_Hat[P_Hat$author=="solo" & 
                P_Hat$Dataset=="Without China & USA"]<-perc_belowOA
  
  
  P_Hat<-P_Hat %>% arrange(Dataset,desc(author))
  ###########
  
  
  
  
  
  
  
  return(list(pRich,P_Hat))
}