DivBootFig_MSv2<-function(Boot_RichDiv,sole_ALL,sole_NOCHNUSA,author_type) {
  
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(RColorBrewer)
  library(egg)
  
  # author_type<-"solo"
  # author_type<-"author_first"
  
  source("./Rscript/functions/DivRichCalc.R")
  
  OAdiv_sole_ALL<-DivRichCalc(sole_ALL,"author_first","OA","OA")
  OAdiv_sole_ALL<-as.numeric(OAdiv_sole_ALL[2])
  
  OAdiv_sole_NOCHNUSA<-DivRichCalc(sole_NOCHNUSA,"author_first","OA","OA")
  OAdiv_sole_NOCHNUSA<-as.numeric(OAdiv_sole_NOCHNUSA[2])
  
  OAinPW_div_sole_ALL<-DivRichCalc(sole_ALL,"author_first","PW","OA")
  OAinPW_div_sole_ALL<-as.numeric(OAinPW_div_sole_ALL[2])
  
  OAinPW_div_sole_NOCHNUSA<-DivRichCalc(sole_NOCHNUSA,"author_first","PW","OA")
  OAinPW_div_sole_NOCHNUSA<-as.numeric(OAinPW_div_sole_NOCHNUSA[2])
 
  OAdiv_first_ALL<-DivRichCalc(first_ALL,"author_first","OA","OA")
  OAdiv_first_ALL<-as.numeric(OAdiv_first_ALL[2])
  
  OAdiv_first_NOCHNUSA<-DivRichCalc(first_NOCHNUSA,"author_first","OA","OA")
  OAdiv_first_NOCHNUSA<-as.numeric(OAdiv_first_NOCHNUSA[2])
  
  OAinPW_div_first_ALL<-DivRichCalc(first_ALL,"author_first","PW","OA")
  OAinPW_div_first_ALL<-as.numeric(OAinPW_div_first_ALL[2])
  
  OAinPW_div_first_NOCHNUSA<-DivRichCalc(first_NOCHNUSA,"author_first","PW","OA")
  OAinPW_div_first_NOCHNUSA<-as.numeric(OAinPW_div_first_NOCHNUSA[2])
  
  if ((author_type=="solo")==TRUE) {
    DatasetOA<-c(OAdiv_sole_ALL,
                 OAdiv_sole_NOCHNUSA,
                 OAinPW_div_sole_ALL,
                 OAinPW_div_sole_NOCHNUSA)
    
    author<-rep("solo",4)
    Dataset<-rep(c("All Countries","CHN & USA excluded"),2)
    OA_Diversity<-bind_cols(DatasetOA,author,Dataset)
    names(OA_Diversity)<-c("value","author","Dataset")
    OA_Diversity$OA_type<-c("mirror","mirror","OAinPW","OAinPW")
    
    
  } else if ((author_type=="author_first")==TRUE) {
    DatasetOA<-c(OAdiv_first_ALL,
                 OAdiv_first_NOCHNUSA,
                 OAinPW_div_first_ALL,
                 OAinPW_div_first_NOCHNUSA)
    
    author<-rep("author_first",4)
    Dataset<-rep(c("All Countries","CHN & USA excluded"),2)
    OA_Diversity<-bind_cols(DatasetOA,author,Dataset)
    names(OA_Diversity)<-c("value","author","Dataset")
    OA_Diversity$OA_type<-c("mirror","mirror","OAinPW","OAinPW")
    
  } else {
    stop("error in type selection")
  }
  
  

  
  means_bootstrap_results<-Boot_RichDiv %>% 
    filter(author==author_type) %>% 
    group_by(Dataset,BootType) %>% 
    summarize(mean(InvSimp))
  str(means_bootstrap_results)
  means_bootstrap_results$OA_type<-rep(c("mirror","mirror","OAinPW"),2)
  
  str(OA_Diversity)
  figure_values<-inner_join(means_bootstrap_results,OA_Diversity)
  figure_values
  names(figure_values)<-c("Dataset", "BootType","mean_Div","OA_type", "OA_Div","author")
  
  
  figure_values$mean_Div<-round(figure_values$mean_Div,digits=1)
  figure_values$OA_Div<-round(figure_values$OA_Div,digits=1)
  summary(as.factor(Boot_RichDiv$author))
  summary(as.factor(Boot_RichDiv$Dataset))
  summary(as.factor(Boot_RichDiv$ArticleCat))
  
  yWO_Div<-460
  ySolo_Div<-460
  ySolo_Div<-460
  author.labels <- c(author_first = "First Authors", solo= "Single Authors")
  
  # as.factor(bootstrap_results$Dataset)
  # as.factor(bootstrap_results$author)
  # 
  
  
  # Boot_RichDiv$Dataset<-gsub("CHN & USA excluded", "Without China & USA",Boot_RichDiv$Dataset)
  # figure_values$Dataset<-gsub("CHN & USA excluded", "Without China & USA",figure_values$Dataset)
  # bootstrap_results$OA_group<-gsub("OA", "PW",bootstrap_results$OA_group)
  
  
  figure_values$author<-as.factor(figure_values$author)

  # 
  bootstrap_results$author<-as.factor(means_bootstrap_results$author)
  # 
  # bootstrap_results$author <- factor(bootstrap_results$author,
  #                                    levels = c("solo","author_first"))
  # 
  
  # 
  # figure_values$author <- factor(figure_values$author,
  #                                levels = c("solo","author_first"))
  # 
  
  Boot_RichDiv_plot<-Boot_RichDiv %>% filter(author==author_type)
  
  pDiv<-
    ggplot(Boot_RichDiv_plot, aes(x=InvSimp,fill=author)) +
    geom_histogram(bins=40, color="black",
                   # fill="darkgray",
                   size=0.1,alpha=0.6, position = 'identity') +
    facet_grid(cols = vars(BootType), rows=vars(Dataset),
               labeller=labeller(author = author.labels),
               scales="free_y")+
    scale_fill_manual(values=c("gray65", "black"))+
    # scale_fill_manual(values=c("darkgray", "red4"))+
    # scale_fill_brewer(palette = "greys")+
    geom_hline((aes(yintercept=-Inf)), color="black") +
    geom_vline((aes(xintercept=-Inf)) , color="black")+
    guides(fill=guide_legend("Article\nCat."))+
    scale_x_continuous(breaks = seq(0,30, by=5),expand=c(0.1,0.02))+
    scale_y_continuous(limits = c(0, 1200),breaks = seq(0,1200, by=100),expand=c(0,0.1))+
    # scale_y_continuous(expand = c(0,0))+
    xlab("Geographic Diversity, D (Reciprocal of Simpson's Index)")+
    
    labs(x = "Geographic Diversity", y = "Frequency") +
    
  
    coord_cartesian(clip="off")+
    # ylim(0,170)+
    # ALL
    geom_segment(data = subset(filter(figure_values, BootType == "MirrorvPW" & Dataset == "All Countries" & author=="author_first")),
                 aes(x = OA_Div , y = 0, xend = OA_Div, yend = ySolo_Div-10), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values, author == "author_first" & Dataset == "All Countries" & BootType == "MirrorvPW")),
              aes(x=OA_Div-0, y=ySolo_Div+15, label=(paste("OA",as.character(OA_Div),sep=" == "))),
              parse=TRUE,color="red", size=2)+
    geom_segment(data = subset(filter(figure_values, BootType == "MirrorvPW" & Dataset == "All Countries" & author=="author_first")),
                 aes(x = mean_Div, y = 0, xend = mean_Div, yend = ySolo_Div), linetype="solid")+
    geom_text(data = subset(filter(figure_values, author == "author_first" & Dataset == "All Countries" & BootType == "MirrorvPW")),
              aes(x=mean_Div-1.8, y=ySolo_Div+15, label=(paste("PW['mean']",as.character(mean_Div),sep=" == "))), 
              parse=TRUE,size=2)+
    # NO CHN USA
    geom_segment(data = subset(filter(figure_values,BootType == "MirrorvPW"  & Dataset == "CHN & USA excluded" & author=="author_first")),
                   aes(x = OA_Div , y = 0, xend = OA_Div, yend = ySolo_Div-10), 
                   colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "CHN & USA excluded" & BootType == "MirrorvPW")),
                aes(x=OA_Div-0, y=ySolo_Div+15, label=(paste("OA",as.character(OA_Div),sep=" == "))),
                parse=TRUE,color="red", size=2)+
    geom_segment(data = subset(filter(figure_values,BootType == "MirrorvPW"  & Dataset == "CHN & USA excluded" & author=="author_first")),
                 aes(x = mean_Div, y = 0, xend = mean_Div, yend = yWO_Div), linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "CHN & USA excluded" & BootType == "MirrorvPW")),
              aes(x=mean_Div+0, y=yWO_Div+20, label=(paste("PW['mean']",as.character(mean_Div),sep=" == "))), 
              parse=TRUE,size=2)+
    
    
    geom_segment(data = subset(filter(figure_values,BootType == "MirrorvOAinPW" & Dataset == "All Countries" & author=="author_first")),
                 aes(x = OA_Div , y = 0, xend = OA_Div, yend = ySolo_Div), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries" & BootType == "MirrorvOAinPW")),
              aes(x=OA_Div+2, y=ySolo_Div+15, label=(paste("OA",as.character(OA_Div),sep=" == "))),
              parse=TRUE,color="red", size=2)+
    geom_segment(data = subset(filter(figure_values,BootType == "MirrorvOAinPW" & Dataset == "All Countries" & author=="author_first")),
                  aes(x = mean_Div, y = 0, xend = mean_Div, yend = ySolo_Div), linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries" & BootType == "MirrorvOAinPW")),
              aes(x=mean_Div+2.5, y=ySolo_Div+0, label=(paste("PW['mean']",as.character(mean_Div),sep=" == "))), 
              parse=TRUE,size=2)+
    # NO CHN USA
    geom_segment(data = subset(filter(figure_values,BootType == "MirrorvOAinPW" & Dataset == "CHN & USA excluded" & author=="author_first")),
                 aes(x = OA_Div, y = 0, xend = OA_Div, yend = yWO_Div), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "CHN & USA excluded" & BootType == "MirrorvOAinPW")),
              aes(x=OA_Div-0, y=yWO_Div+20, label=(paste("OA",as.character(OA_Div),sep=" == "))), 
              parse=TRUE,color="red", size=2)+
    geom_segment(data = subset(filter(figure_values,BootType == "MirrorvOAinPW" & Dataset == "CHN & USA excluded" & author=="author_first")),
                 aes(x = mean_Div, y = 0, xend = mean_Div, yend = ySolo_Div), linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "CHN & USA excluded" & BootType == "MirrorvOAinPW")),
              aes(x=mean_Div+2.5, y=ySolo_Div+0, label=(paste("PW['mean']",as.character(mean_Div),sep=" == "))), 
              parse=TRUE,size=2)+
    
    
    geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries" & BootType == "OAinPWvPW")),
                 aes(x = OA_Div, y = 0, xend = OA_Div, yend = yWO_Div),  
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries" & BootType == "OAinPWvPW")),
              aes(x=OA_Div-2, y=ySolo_Div+0, label=(paste("OA",as.character(OA_Div),sep=" == "))),
              parse=TRUE,color="red", size=2)+
    geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries" & BootType=="OAinPWvPW")),
                 aes(x = mean_Div, y = 0, xend = mean_Div, yend = ySolo_Div-50), linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries" & BootType=="OAinPWvPW")),
              aes(x=mean_Div+2, y=ySolo_Div-40, label=(paste("PW-Open['mean']",as.character(mean_Div),sep=" == "))),
              parse=TRUE,size=2)+
    
    geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "CHN & USA excluded" & BootType=="OAinPWvPW")),
                 aes(x = OA_Div, y = 0, xend = OA_Div, yend = yWO_Div), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "CHN & USA excluded" & BootType=="OAinPWvPW")),
              aes(x=OA_Div-0, y=ySolo_Div, label=(paste("PW['mean']",as.character(mean_Div),sep=" == "))), 
              parse=TRUE,size=2)+
    
    geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "CHN & USA excluded" & BootType=="OAinPWvPW")),
                 aes(x = mean_Div, y = 0, xend = mean_Div, yend = yWO_Div-25), linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "CHN & USA excluded" & BootType=="OAinPWvPW")),
              aes(x=mean_Div+3.5, y=ySolo_Div-15, label=(paste("PW-Open['mean']",as.character(mean_Div),sep=" == "))),
              parse=TRUE,size=2)
    # 
    
    # MEAN OF BOOTSTRAP
    
    # 
    # geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "CHN & USA excluded" & ArticleCat=="OAinPW")),
    #              aes(x = OA_Div, y = 0, xend = OA_Div, yend = yWO_Div-10), linetype="solid")+
    # geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "CHN & USA excluded" & ArticleCat=="OAinPW")),
    #           aes(x=OA_Div+1.5, y=yWO_Div+15, label=(paste("PW-Open['mean']",as.character(OA_Div),sep=" == "))), 
    #           parse=TRUE,size=2)+
    # 
    # OA Value

  
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
  # P-HAT for All countries, coauthored, PW
  crit<-figure_values %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    filter(ArticleCat=="PW") %>% 
    select(OA_Div)
  
  perc<-Boot_RichDiv %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    filter(ArticleCat=="PW") %>% 
    ungroup() %>% 
    tally(InvSimp<crit$OA_Div) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  P_Hat$P_Hat[P_Hat$author=="author_first" & 
                P_Hat$Dataset=="All Countries"&
                P_Hat$ArticleCat=="PW"]<-perc_belowOA
  ###########
  
  
  ##########
  # # P-HAT without USA CHN, coauthored, PW
  crit<-figure_values %>% 
    filter(Dataset=="CHN & USA excluded") %>% 
    filter(author=="author_first") %>% 
    filter(ArticleCat=="PW") %>% 
    select(OA_Div)
  
  perc<-Boot_RichDiv %>% 
    filter(Dataset=="CHN & USA excluded") %>% 
    filter(author=="author_first") %>% 
    filter(ArticleCat=="PW") %>% 
    ungroup() %>% 
    tally(InvSimp<crit$OA_Div) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  P_Hat$P_Hat[P_Hat$author=="author_first" & 
                P_Hat$Dataset=="CHN & USA excluded" &
                P_Hat$ArticleCat=="PW"]<-perc_belowOA
  ###########
  
  ##########
  # # P-HAT All countries, coauthored, PW
  crit<-figure_values %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    filter(ArticleCat=="PW") %>% 
    select(OA_Div)
  
  perc<-Boot_RichDiv %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    filter(ArticleCat=="PW") %>% 
    ungroup() %>% 
    tally(InvSimp<crit$OA_Div) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  P_Hat$P_Hat[P_Hat$author=="author_first" & 
                P_Hat$Dataset=="All Countries" & 
                P_Hat$ArticleCat=="PW"]<-perc_belowOA
  ###########
  
  
  ##########
  # # P-HAT without USA CHN, coauthored, PW
  crit<-figure_values %>% 
    filter(Dataset=="CHN & USA excluded") %>% 
    filter(author=="author_first") %>% 
    filter(ArticleCat=="PW") %>% 
    select(OA_Div)
  
  perc<-Boot_RichDiv %>% 
    filter(Dataset=="CHN & USA excluded") %>% 
    filter(author=="author_first") %>% 
    filter(ArticleCat=="PW") %>% 
    ungroup() %>% 
    tally(InvSimp<crit$OA_Div) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  P_Hat$P_Hat[P_Hat$author=="author_first" & 
                P_Hat$Dataset=="CHN & USA excluded" &
                P_Hat$ArticleCat=="PW"]<-perc_belowOA
  
  P_Hat<-P_Hat %>% arrange(Dataset,desc(author))
  
  #########
  # 
  # ##########
  # # P-HAT for All countries, coauthored, OAinPW
  # crit<-figure_values %>% 
  #   filter(Dataset=="All Countries") %>% 
  #   filter(author=="author_first") %>% 
  #   filter(ArticleCat=="OAinPW") %>% 
  #   select(OA_Div)
  # 
  # perc<-Boot_RichDiv %>% 
  #   filter(Dataset=="All Countries") %>% 
  #   filter(author=="author_first") %>% 
  #   filter(ArticleCat=="OAinPW") %>% 
  #   ungroup() %>% 
  #   tally(InvSimp<crit$OA_Div) %>% 
  #   mutate(perc_belowOA = n/1000)
  # perc_belowOA<-perc$perc_belowOA
  # perc_belowOA
  # 
  # P_Hat$P_Hat[P_Hat$author=="author_first" & 
  #               P_Hat$Dataset=="All Countries"&
  #               P_Hat$ArticleCat=="OAinPW"]<-perc_belowOA
  # ###########
  # 
  # 
  # ##########
  # # # P-HAT without USA CHN, coauthored, OAinPW
  # crit<-figure_values %>% 
  #   filter(Dataset=="CHN & USA excluded") %>% 
  #   filter(author=="author_first") %>% 
  #   filter(ArticleCat=="OAinPW") %>% 
  #   select(OA_Div)
  # 
  # perc<-Boot_RichDiv %>% 
  #   filter(Dataset=="CHN & USA excluded") %>% 
  #   filter(author=="author_first") %>% 
  #   filter(ArticleCat=="OAinPW") %>% 
  #   ungroup() %>% 
  #   tally(InvSimp<crit$OA_Div) %>% 
  #   mutate(perc_belowOA = n/1000)
  # perc_belowOA<-perc$perc_belowOA
  # perc_belowOA
  # 
  # P_Hat$P_Hat[P_Hat$author=="author_first" & 
  #               P_Hat$Dataset=="CHN & USA excluded" &
  #               P_Hat$ArticleCat=="OAinPW"]<-perc_belowOA
  # ###########
  # 
  # ##########
  # # # P-HAT All countries, coauthored, OAinPW
  # crit<-figure_values %>% 
  #   filter(Dataset=="All Countries") %>% 
  #   filter(author=="author_first") %>% 
  #   filter(ArticleCat=="OAinPW") %>% 
  #   select(OA_Div)
  # 
  # perc<-Boot_RichDiv %>% 
  #   filter(Dataset=="All Countries") %>% 
  #   filter(author=="author_first") %>% 
  #   filter(ArticleCat=="OAinPW") %>% 
  #   ungroup() %>% 
  #   tally(InvSimp<crit$OA_Div) %>% 
  #   mutate(perc_belowOA = n/1000)
  # perc_belowOA<-perc$perc_belowOA
  # perc_belowOA
  # 
  # P_Hat$P_Hat[P_Hat$author=="author_first" & 
  #               P_Hat$Dataset=="All Countries" & 
  #               P_Hat$ArticleCat=="OAinPW"]<-perc_belowOA
  # ###########
  # 
  # 
  # ##########
  # # # P-HAT without USA CHN, coauthored, OAinPW
  # crit<-figure_values %>% 
  #   filter(Dataset=="CHN & USA excluded") %>% 
  #   filter(author=="author_first") %>% 
  #   filter(ArticleCat=="OAinPW") %>% 
  #   select(OA_Div)
  # 
  # perc<-Boot_RichDiv %>% 
  #   filter(Dataset=="CHN & USA excluded") %>% 
  #   filter(author=="author_first") %>% 
  #   filter(ArticleCat=="OAinPW") %>% 
  #   ungroup() %>% 
  #   tally(InvSimp<crit$OA_Div) %>% 
  #   mutate(perc_belowOA = n/1000)
  # perc_belowOA<-perc$perc_belowOA
  # perc_belowOA
  # 
  # P_Hat$P_Hat[P_Hat$author=="author_first" & 
  #               P_Hat$Dataset=="CHN & USA excluded" &
  #               P_Hat$ArticleCat=="OAinPW"]<-perc_belowOA
  # 
  P_Hat<-P_Hat %>% arrange(Dataset,desc(author))
  
  #########
  
  return(list(pDiv,P_Hat))
}
