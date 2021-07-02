DivBootFig_MSv2<-function(Boot_RichDiv,sole_ALL,sole_NOCHNUSA,first_ALL,first_NOCHNUSA) {
  # Boot_RichDiv<-BootMirror_RichDiv
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(RColorBrewer)
  library(egg)
  levels(as.factor(Boot_RichDiv$Dataset))
  Boot_RichDiv<-Boot_RichDiv %>% 
  mutate(Dataset = ifelse(Dataset == "all countries", "All Countries", Dataset)) %>% 
    mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "China & USA excluded", Dataset))
  
  

  source("./Rscript/functions/DivRichCalc.R")
  
  # OA Mirror
  
  OAdiv_first_ALL_OAmirror<-DivRichCalc(first_ALL,"author_first","OA","OA")
  OAdiv_first_ALL_OAmirror<-as.numeric(OAdiv_first_ALL_OAmirror[2])
  
  OAdiv_first_NOCHNUSA_OAmirror<-DivRichCalc(first_NOCHNUSA,"author_first","OA","OA")
  OAdiv_first_NOCHNUSA_OAmirror<-as.numeric(OAdiv_first_NOCHNUSA_OAmirror[2])
  
  OAdiv_sole_ALL_OAmirror<-DivRichCalc(sole_ALL,"author_first","OA","OA")
  OAdiv_sole_ALL_OAmirror<-as.numeric(OAdiv_sole_ALL_OAmirror[2])
  
  OAdiv_sole_NOCHNUSA_OAmirror<-DivRichCalc(sole_NOCHNUSA,"author_first","OA","OA")
  OAdiv_sole_NOCHNUSA_OAmirror<-as.numeric(OAdiv_sole_NOCHNUSA_OAmirror[2])
  
  # OA - Parent
  
  OAdiv_first_ALL_OAparent<-DivRichCalc(first_ALL,"author_first","PW","OA")
  OAdiv_first_ALL_OAparent<-as.numeric(OAdiv_first_ALL_OAparent[2])
  
  OAdiv_first_NOCHNUSA_OAparent<-DivRichCalc(first_NOCHNUSA,"author_first","PW","OA")
  OAdiv_first_NOCHNUSA_OAparent<-as.numeric(OAdiv_first_NOCHNUSA_OAparent[2])
  
  OAdiv_sole_ALL_OAparent<-DivRichCalc(sole_ALL,"author_first","PW","OA")
  OAdiv_sole_ALL_OAparent<-as.numeric(OAdiv_sole_ALL_OAparent[2])
  
  OAdiv_sole_NOCHNUSA_OAparent<-DivRichCalc(sole_NOCHNUSA,"author_first","PW","OA")
  OAdiv_sole_NOCHNUSA_OAparent<-as.numeric(OAdiv_sole_NOCHNUSA_OAparent[2])
  
  

  
    author<-rep(c("author_first", "author_first", "solo", "solo"),2)
  Dataset<-rep(c("All Countries","China & USA excluded"),4)
  # OA_articleType<-c(rep("allOA",4),rep("mirror",4),rep("OAinPW",4))
  OA_articleType<-c(rep("OA-mirror",4),rep("Parent",4))
  
  OA_Diversity<-c(OAdiv_first_ALL_OAmirror,
                  OAdiv_first_NOCHNUSA_OAmirror,
                  OAdiv_sole_ALL_OAmirror,
                  OAdiv_sole_NOCHNUSA_OAmirror,
                  OAdiv_first_ALL_OAparent,
                  OAdiv_first_NOCHNUSA_OAparent,
                  OAdiv_sole_ALL_OAparent,
                  OAdiv_sole_NOCHNUSA_OAparent)
  
  
  OA_Diversity<-bind_cols(OA_Diversity,author,Dataset,OA_articleType)
  names(OA_Diversity)<-c("value","author","Dataset","Articles_Value")
  
  means_bootstrap_results<-Boot_RichDiv %>% 
    group_by(author,Dataset,BootType) %>% 
    summarize(mean(InvSimp)) %>% 
    mutate(Dataset = ifelse(Dataset == "all", "All Countries", Dataset)) %>% 
    mutate(Dataset = ifelse(Dataset == "no_CHN_USA", "China & USA excluded", Dataset))
  
  
  
  # means_bootstrap_results$BootType<-gsub("MirrorvOAinPW","Mirror",means_bootstrap_results$BootType)
  # means_bootstrap_results$BootType<-gsub("OAinPWvPW","OAinPW",means_bootstrap_results$BootType)
  # 
  figure_values<-inner_join(means_bootstrap_results,OA_Diversity,by=c("author","Dataset"))
  figure_values<-figure_values %>% 
    select(author,Dataset,"ArticleCat"=Articles_Value,"mean_Div"=`mean(InvSimp)`,"OA_Div"=value)
  # names(figure_values)<-c("author","Dataset", "ArticleCat","mean_Div", "OA_Div")
  
  # figure_values<-figure_values %>% filter(ArticleCat=="allOA")
  figure_values$ArticleCat2 <- figure_values$ArticleCat
  figure_values$ArticleCat<-rep(c("PW"),8)
  # figure_values$ArticleCat<-rep(c("PW","OAinPW"),4)
  
  figure_values$mean_Div<-round(figure_values$mean_Div,digits=1)
  figure_values$OA_Div<-round(figure_values$OA_Div,digits=1)
  summary(as.factor(Boot_RichDiv$author))
  summary(as.factor(Boot_RichDiv$Dataset))
  Boot_RichDiv$ArticleCat<-"PW"
  summary(as.factor(Boot_RichDiv$ArticleCat))
  
  yWO_Div<-460
  yALL_Div<-460
  ySolo_Div<-460
  author.labels <- c(author_first = "First Authors", solo= "Single Authors")
  
  # as.factor(bootstrap_results$Dataset)
  # as.factor(bootstrap_results$author)
  # 
  
  
  # Boot_RichDiv$Dataset<-gsub("China & USA excluded", "Without China & USA",Boot_RichDiv$Dataset)
  # figure_values$Dataset<-gsub("China & USA excluded", "Without China & USA",figure_values$Dataset)
  # bootstrap_results$OA_group<-gsub("OA", "PW",bootstrap_results$OA_group)
  
  
  figure_values$author<-as.factor(figure_values$author)

  # 
  Boot_RichDiv$author<-as.factor(Boot_RichDiv$author)
  # 
  Boot_RichDiv$author <- factor(Boot_RichDiv$author,
                                levels = c("solo","author_first"))
  # 
  
  # 
  figure_values$author <- factor(figure_values$author, 
                                 levels = c("solo","author_first"))
  # 
  
  
  
  pDiv<-
    ggplot(Boot_RichDiv, aes(x=InvSimp,fill=ArticleCat)) +
    geom_histogram(bins=40, color="black",
                   # fill="darkgray",
                   size=0.1,alpha=0.6, position = 'identity') +
    facet_grid(cols = vars(Dataset), rows=vars(author),
               labeller=labeller(author = author.labels),
               scales="free_y")+
    scale_fill_manual(values=c("gray65", "black","white"))+
    # scale_fill_manual(values=c("darkgray", "red4"))+
    # scale_fill_brewer(palette = "greys")+
    geom_hline((aes(yintercept=-Inf)), color="black") +
    geom_vline((aes(xintercept=-Inf)) , color="black")+
    guides(fill=guide_legend("Article\nCat."))+
    scale_x_continuous(breaks = seq(0,30, by=5),expand=c(0.1,0.02))+
    scale_y_continuous(limits = c(0, 600),breaks = seq(0,600, by=100),expand=c(0,0.1))+
    # scale_y_continuous(expand = c(0,0))+
    xlab("Geographic Diversity, D (Reciprocal of Simpson's Index)")+
    
    labs(x = "Geographic Diversity", y = "Frequency") +
    
  
    coord_cartesian(clip="off")+
    # ylim(0,170)+
    # median OF BOOTSTRAP
    # geom_segment(data = subset(filter(figure_values, author == "author_first" & Dataset == "All Countries" & ArticleCat=="PW")),
    #              aes(x = mean_Div, y = 0, xend = mean_Div, yend = yALL_Div), linetype="solid")+
    # geom_text(data = subset(filter(figure_values, author == "author_first" & Dataset == "All Countries" & ArticleCat=="PW")),
    #           aes(x=mean_Div-1.8, y=yALL_Div+15, label=(paste("PW['mean']",as.character(mean_Div),sep=" == "))), 
    #           parse=TRUE,size=2)+
    # 
    # geom_segment(data = subset(filter(figure_values, author == "author_first" & Dataset == "All Countries" & ArticleCat=="OAinPW")),
    #              aes(x = mean_Div, y = 0, xend = mean_Div, yend = yALL_Div-35), linetype="solid")+
    # geom_text(data = subset(filter(figure_values, author == "author_first" & Dataset == "All Countries" & ArticleCat=="OAinPW")),
    #           aes(x=mean_Div-1.8, y=yALL_Div-20, label=(paste("PW-Open['mean']",as.character(mean_Div),sep=" == "))), 
    #           parse=TRUE,size=2)+
    
    # OA Value - MIRROR
    geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries" & ArticleCat2=="OA-mirror")),
                 aes(x = OA_Div , y = 0, xend = OA_Div, yend = yALL_Div), 
                 colour = "red",linetype="dashed")+
    # geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries" & ArticleCat2=="OA-mirror")),
    #           aes(x=OA_Div+0, y=yALL_Div+25, label=(paste("Mirror",as.character(OA_Div),sep=" == "))),
    #           parse=TRUE,color="red", size=2)+
    # geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries" & ArticleCat2=="OA-mirror")),
    #           aes(x=OA_Div+0, y=yALL_Div+25, label="Mirror"),
    #           parse=TRUE,color="red", size=2)+
    
    
    # geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries" & ArticleCat2=="Parent")),
    #              aes(x = OA_Div , y = 0, xend = OA_Div, yend = yALL_Div-100), 
    #              colour = "blue")+
    # # geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries" & ArticleCat2=="Parent")),
    # #           aes(x=OA_Div+0, y=yALL_Div-100+25, label=(paste("Parent-OA",as.character(OA_Div),sep=" == "))),
    # #           parse=TRUE,color="blue", size=2)+
    # geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries" & ArticleCat2=="Parent")),
    #           aes(x=OA_Div+1.5, y=yALL_Div-100+25, label="Parent"),
    #           parse=TRUE,color="blue", size=2)+
    
    
    # MEAN OF BOOTSTRAP
    # geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries" & ArticleCat2=="PW" & ArticleCat2=="PW")),
    #              aes(x = mean_Div, y = 0, xend = mean_Div, yend = yALL_Div), linetype="solid")+
    # geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries" & ArticleCat2=="PW" & ArticleCat2=="PW")),
    #           aes(x=mean_Div+2.5, y=yALL_Div+0, label=(paste("PW['mean']",as.character(mean_Div),sep=" == "))), 
    #           parse=TRUE,size=2)+
    # 
    # geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries" & ArticleCat2=="OAinPW")),
    #              aes(x = mean_Div, y = 0, xend = mean_Div, yend = yALL_Div-50), linetype="solid")+
    # geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries" & ArticleCat2=="OAinPW")),
    #           aes(x=mean_Div+2, y=yALL_Div-40, label=(paste("PW-Open['mean']",as.character(mean_Div),sep=" == "))), 
    #           parse=TRUE,size=2)+
    # OA Value
    geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries" & ArticleCat2=="OA-mirror")),
                 aes(x = OA_Div , y = 0, xend = OA_Div, yend = ySolo_Div), 
                 colour = "red",linetype="dashed")+
    # geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries" & ArticleCat2=="OA-mirror")),
    #           aes(x=OA_Div-0, y=ySolo_Div+25, label=(paste("OA",as.character(OA_Div),sep=" == "))),
    #           parse=TRUE,color="red", size=2)+
    # geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries" & ArticleCat2=="OA-mirror")),
    #           aes(x=OA_Div-0, y=ySolo_Div+25, label="Mirror"),
    #           parse=TRUE,color="red",size=2)+
    
    
    # geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries" & ArticleCat2=="Parent")),
    #              aes(x = OA_Div , y = 0, xend = OA_Div, yend = yALL_Div-100), 
    #              colour = "blue")+
    # # geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries" & ArticleCat2=="Parent")),
    # #           aes(x=OA_Div+0, y=yALL_Div-100+25, label=(paste("Parent-OA",as.character(OA_Div),sep=" == "))),
    # #           parse=TRUE,color="blue", size=2)+
    # geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries" & ArticleCat2=="Parent")),
    #           aes(x=OA_Div+0, y=yALL_Div-100+25, label="Parent"),
    #           parse=TRUE,color="blue", size=2)+
    # MEAN OF BOOTSTRAP
    # geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "China & USA excluded" & ArticleCat2=="PW")),
    #              aes(x = mean_Div, y = 0, xend = mean_Div, yend = yWO_Div), linetype="solid")+
    # geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "China & USA excluded" & ArticleCat2=="PW")),
    #           aes(x=mean_Div+0, y=yWO_Div+20, label=(paste("PW['mean']",as.character(mean_Div),sep=" == "))), 
    #           parse=TRUE,size=2)+
    
    # geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "China & USA excluded" & ArticleCat2=="OAinPW")),
    #              aes(x = mean_Div, y = 0, xend = mean_Div, yend = yWO_Div-25), linetype="solid")+
    # geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "China & USA excluded" & ArticleCat2=="OAinPW")),
    #           aes(x=mean_Div+3.5, y=yWO_Div-15, label=(paste("PW-Open['mean']",as.character(mean_Div),sep=" == "))), 
    #           parse=TRUE,size=2)+
    # 
    # OA Value
    geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "China & USA excluded" & ArticleCat2=="OA-mirror")),
                 aes(x = OA_Div, y = 0, xend = OA_Div, yend = yWO_Div), 
                 colour = "red",linetype="dashed")+
    # geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "China & USA excluded" & ArticleCat2=="OA-mirror")),
    #           aes(x=OA_Div-0, y=yWO_Div+25, label=(paste("OA",as.character(OA_Div),sep=" == "))), 
    #           parse=TRUE,color="red", size=2)+
    # geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "China & USA excluded" & ArticleCat2=="OA-mirror")),
    #           aes(x=OA_Div-0, y=yWO_Div+25, label="Mirror"), 
    #           parse=TRUE,color="red", size=2)+
    # 
    
    # 
    # geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "China & USA excluded" & ArticleCat2=="Parent")),
    #              aes(x = OA_Div , y = 0, xend = OA_Div, yend = yALL_Div-100), 
    #              colour = "blue")+
    # # geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "China & USA excluded" & ArticleCat2=="Parent")),
    # #           aes(x=OA_Div+0, y=yALL_Div-100+25, label=(paste("Parent-OA",as.character(OA_Div),sep=" == "))),
    # #           parse=TRUE,color="blue", size=2)+
    # geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "China & USA excluded" & ArticleCat2=="Parent")),
    #           aes(x=OA_Div-1, y=yALL_Div-100+25, label="Parent"),
    #           parse=TRUE,color="blue", size=2)+
    # MEAN OF BOOTSTRAP
    # geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "China & USA excluded" & ArticleCat2=="PW")),
    #              aes(x = mean_Div, y = 0, xend = mean_Div, yend = yWO_Div-10), linetype="solid")+
    # geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "China & USA excluded" & ArticleCat2=="PW")),
    #           aes(x=mean_Div-0, y=yWO_Div+15, label=(paste("PW['mean']",as.character(mean_Div),sep=" == "))), 
    #           parse=TRUE,size=2)+
    # 
    # geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "China & USA excluded" & ArticleCat2=="OAinPW")),
    #              aes(x = mean_Div, y = 0, xend = mean_Div, yend = yWO_Div-10), linetype="solid")+
    # geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "China & USA excluded" & ArticleCat2=="OAinPW")),
    #           aes(x=mean_Div+1.5, y=yWO_Div+15, label=(paste("PW-Open['mean']",as.character(mean_Div),sep=" == "))), 
    #           parse=TRUE,size=2)+
    # 
    # OA Value
    geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "China & USA excluded" & ArticleCat2=="OA-mirror")),
                 aes(x = OA_Div , y = 0, xend = OA_Div, yend = ySolo_Div-10), 
                 colour = "red",linetype="dashed")
    # geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "China & USA excluded" & ArticleCat2=="OA-mirror")),
    #           aes(x=OA_Div-0, y=ySolo_Div+25, label=(paste("OA",as.character(OA_Div),sep=" == "))),
    #           parse=TRUE,color="red", size=2)+
    # geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "China & USA excluded" & ArticleCat2=="OA-mirror")),
    #           aes(x=OA_Div-0, y=ySolo_Div+25, label="Mirror"),
    #           parse=TRUE,color="red", size=2)+
    
    
    
    # geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "China & USA excluded" & ArticleCat2=="Parent")),
    #              aes(x = OA_Div , y = 0, xend = OA_Div, yend = yALL_Div-100), 
    #              colour = "blue")+
    # # geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "China & USA excluded" & ArticleCat2=="Parent")),
    # #           aes(x=OA_Div+0, y=yALL_Div-100+25, label=(paste("Parent-OA",as.character(OA_Div),sep=" == "))),
    # #           parse=TRUE,color="blue", size=2)+
    # geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "China & USA excluded" & ArticleCat2=="Parent")),
    #           aes(x=OA_Div+0, y=yALL_Div-100+25, label="Parent"),
    #           parse=TRUE,color="blue", size=2)
  
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
  
  
  # 
  # 
  # 
  # 
  # ##################################################################################
  # # DIVERISTY
  # ############################################################################
  #  n_boot<-Boot_RichDiv %>% 
  #   group_by(author,Dataset) %>% 
  #   summarize(n=n()) 
  # boot_runs<-max(as.numeric(n_boot$n))
  # 
  # figure_values<-ungroup(figure_values)
  # P_Hat<-figure_values
  # P_Hat$P_Hat<-NA
  # P_Hat$JrnlType<-NULL
  # ##########
  # # P-HAT for All countries, coauthored, PW
  # crit<-figure_values %>% 
  #   filter(Dataset=="All Countries") %>% 
  #   filter(author=="author_first") %>% 
  #   filter(ArticleCat=="PW") %>% 
  #   select(OA_Div)
  # 
  # perc<-Boot_RichDiv %>% 
  #   filter(Dataset=="All Countries") %>% 
  #   filter(author=="author_first") %>% 
  #   filter(ArticleCat=="PW") %>% 
  #   ungroup() %>% 
  #   tally(InvSimp<crit$OA_Div) %>% 
  #   mutate(perc_belowOA = n/boot_runs)
  # perc_belowOA<-perc$perc_belowOA
  # perc_belowOA
  # 
  # P_Hat$P_Hat[P_Hat$author=="author_first" & 
  #               P_Hat$Dataset=="All Countries"&
  #               P_Hat$ArticleCat=="PW"]<-perc_belowOA
  # ###########
  # 
  # 
  # ##########
  # # # P-HAT without USA CHN, coauthored, PW
  # crit<-figure_values %>% 
  #   filter(Dataset=="China & USA excluded") %>% 
  #   filter(author=="author_first") %>% 
  #   filter(ArticleCat=="PW") %>% 
  #   select(OA_Div)
  # 
  # perc<-Boot_RichDiv %>% 
  #   filter(Dataset=="China & USA excluded") %>% 
  #   filter(author=="author_first") %>% 
  #   filter(ArticleCat=="PW") %>% 
  #   ungroup() %>% 
  #   tally(InvSimp<crit$OA_Div) %>% 
  #   mutate(perc_belowOA = n/boot_runs)
  # perc_belowOA<-perc$perc_belowOA
  # perc_belowOA
  # 
  # P_Hat$P_Hat[P_Hat$author=="author_first" & 
  #               P_Hat$Dataset=="China & USA excluded" &
  #               P_Hat$ArticleCat=="PW"]<-perc_belowOA
  # ###########
  # 
  # ##########
  # # # P-HAT All countries, coauthored, PW
  # crit<-figure_values %>% 
  #   filter(Dataset=="All Countries") %>% 
  #   filter(author=="solo") %>% 
  #   filter(ArticleCat=="PW") %>% 
  #   select(OA_Div)
  # 
  # perc<-Boot_RichDiv %>% 
  #   filter(Dataset=="All Countries") %>% 
  #   filter(author=="solo") %>% 
  #   filter(ArticleCat=="PW") %>% 
  #   ungroup() %>% 
  #   tally(InvSimp<crit$OA_Div) %>% 
  #   mutate(perc_belowOA = n/boot_runs)
  # perc_belowOA<-perc$perc_belowOA
  # perc_belowOA
  # 
  # P_Hat$P_Hat[P_Hat$author=="solo" & 
  #               P_Hat$Dataset=="All Countries" & 
  #               P_Hat$ArticleCat=="PW"]<-perc_belowOA
  # ###########
  # 
  # 
  # ##########
  # # # P-HAT without USA CHN, coauthored, PW
  # crit<-figure_values %>% 
  #   filter(Dataset=="China & USA excluded") %>% 
  #   filter(author=="solo") %>% 
  #   filter(ArticleCat=="PW") %>% 
  #   select(OA_Div)
  # 
  # perc<-Boot_RichDiv %>% 
  #   filter(Dataset=="China & USA excluded") %>% 
  #   filter(author=="solo") %>% 
  #   filter(ArticleCat=="PW") %>% 
  #   ungroup() %>% 
  #   tally(InvSimp<crit$OA_Div) %>% 
  #   mutate(perc_belowOA = n/boot_runs)
  # perc_belowOA<-perc$perc_belowOA
  # perc_belowOA
  # 
  # P_Hat$P_Hat[P_Hat$author=="solo" & 
  #               P_Hat$Dataset=="China & USA excluded" &
  #               P_Hat$ArticleCat=="PW"]<-perc_belowOA
  # 
  # P_Hat<-P_Hat %>% arrange(Dataset,desc(author))
  # 
  # #########
  # # 
  # # ##########
  # # # P-HAT for All countries, coauthored, OAinPW
  # # crit<-figure_values %>% 
  # #   filter(Dataset=="All Countries") %>% 
  # #   filter(author=="author_first") %>% 
  # #   filter(ArticleCat=="OAinPW") %>% 
  # #   select(OA_Div)
  # # 
  # # perc<-Boot_RichDiv %>% 
  # #   filter(Dataset=="All Countries") %>% 
  # #   filter(author=="author_first") %>% 
  # #   filter(ArticleCat=="OAinPW") %>% 
  # #   ungroup() %>% 
  # #   tally(InvSimp<crit$OA_Div) %>% 
  # #   mutate(perc_belowOA = n/boot_runs)
  # # perc_belowOA<-perc$perc_belowOA
  # # perc_belowOA
  # # 
  # # P_Hat$P_Hat[P_Hat$author=="author_first" & 
  # #               P_Hat$Dataset=="All Countries"&
  # #               P_Hat$ArticleCat=="OAinPW"]<-perc_belowOA
  # # ###########
  # # 
  # # 
  # # ##########
  # # # # P-HAT without USA CHN, coauthored, OAinPW
  # # crit<-figure_values %>% 
  # #   filter(Dataset=="China & USA excluded") %>% 
  # #   filter(author=="author_first") %>% 
  # #   filter(ArticleCat=="OAinPW") %>% 
  # #   select(OA_Div)
  # # 
  # # perc<-Boot_RichDiv %>% 
  # #   filter(Dataset=="China & USA excluded") %>% 
  # #   filter(author=="author_first") %>% 
  # #   filter(ArticleCat=="OAinPW") %>% 
  # #   ungroup() %>% 
  # #   tally(InvSimp<crit$OA_Div) %>% 
  # #   mutate(perc_belowOA = n/boot_runs)
  # # perc_belowOA<-perc$perc_belowOA
  # # perc_belowOA
  # # 
  # # P_Hat$P_Hat[P_Hat$author=="author_first" & 
  # #               P_Hat$Dataset=="China & USA excluded" &
  # #               P_Hat$ArticleCat=="OAinPW"]<-perc_belowOA
  # # ###########
  # # 
  # # ##########
  # # # # P-HAT All countries, coauthored, OAinPW
  # # crit<-figure_values %>% 
  # #   filter(Dataset=="All Countries") %>% 
  # #   filter(author=="solo") %>% 
  # #   filter(ArticleCat=="OAinPW") %>% 
  # #   select(OA_Div)
  # # 
  # # perc<-Boot_RichDiv %>% 
  # #   filter(Dataset=="All Countries") %>% 
  # #   filter(author=="solo") %>% 
  # #   filter(ArticleCat=="OAinPW") %>% 
  # #   ungroup() %>% 
  # #   tally(InvSimp<crit$OA_Div) %>% 
  # #   mutate(perc_belowOA = n/boot_runs)
  # # perc_belowOA<-perc$perc_belowOA
  # # perc_belowOA
  # # 
  # # P_Hat$P_Hat[P_Hat$author=="solo" & 
  # #               P_Hat$Dataset=="All Countries" & 
  # #               P_Hat$ArticleCat=="OAinPW"]<-perc_belowOA
  # # ###########
  # # 
  # # 
  # # ##########
  # # # # P-HAT without USA CHN, coauthored, OAinPW
  # # crit<-figure_values %>% 
  # #   filter(Dataset=="China & USA excluded") %>% 
  # #   filter(author=="solo") %>% 
  # #   filter(ArticleCat=="OAinPW") %>% 
  # #   select(OA_Div)
  # # 
  # # perc<-Boot_RichDiv %>% 
  # #   filter(Dataset=="China & USA excluded") %>% 
  # #   filter(author=="solo") %>% 
  # #   filter(ArticleCat=="OAinPW") %>% 
  # #   ungroup() %>% 
  # #   tally(InvSimp<crit$OA_Div) %>% 
  # #   mutate(perc_belowOA = n/boot_runs)
  # # perc_belowOA<-perc$perc_belowOA
  # # perc_belowOA
  # # 
  # # P_Hat$P_Hat[P_Hat$author=="solo" & 
  # #               P_Hat$Dataset=="China & USA excluded" &
  # #               P_Hat$ArticleCat=="OAinPW"]<-perc_belowOA
  # # 
  # P_Hat<-P_Hat %>% arrange(Dataset,desc(author))
  # 
  # #########
  # 
  return(pDiv)
  # return(list(pDiv,P_Hat))
}
