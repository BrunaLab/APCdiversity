EvenFig<-function(bootstrap_results) {
  
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
  OAeven_coauthor_ALL<-DivRichCalc(coauthor_ALL,"author_first","OA")
  OAeven_coauthor_ALL<-as.numeric(OAeven_coauthor_ALL[5])
  
  OAeven_coauthor_NOCHNUSA<-DivRichCalc(coauthor_NOCHNUSA,"author_first","OA")
  OAeven_coauthor_NOCHNUSA<-as.numeric(OAeven_coauthor_NOCHNUSA[5])
  
  OAeven_sole_ALL<-DivRichCalc(sole_ALL,"author_first","OA")
  OAeven_sole_ALL<-as.numeric(OAeven_sole_ALL[5])
  
  OAeven_sole_NOCHNUSA<-DivRichCalc(sole_NOCHNUSA,"author_first","OA")
  OAeven_sole_NOCHNUSA<-as.numeric(OAeven_sole_NOCHNUSA[5])
  
  OA_Evenness<-c(OAeven_coauthor_ALL,OAeven_coauthor_NOCHNUSA,
                 OAeven_sole_ALL,OAeven_sole_NOCHNUSA)
  OA_Evenness<-as_tibble(OA_Evenness)
  OA_Evenness<-dplyr::rename(OA_Evenness,"OA_Evenness"="value")
  
  means_bootstrap_results<-bootstrap_results %>% 
    group_by(author,Dataset) %>% 
    summarize(mean(Even))
  
  figure_values<-bind_cols(means_bootstrap_results,OA_Evenness)
  figure_values
  names(figure_values)<-c("author","Dataset", "mean_Evenness", "OA_Evenness")
  figure_values$JrnlType<-"PW"
  figure_values$mean_Evenness<-round(figure_values$mean_Evenness,digits=2)
  figure_values$OA_Evenness<-round(figure_values$OA_Evenness,digits=2)
  summary(as.factor(bootstrap_results$author))
  summary(as.factor(bootstrap_results$Dataset))
  summary(as.factor(bootstrap_results$JrnlType))
  
  y_Even_bar<-385
  y_Even_text<-395
  
  author.labels <- c(author_first = "First Authors", solo= "Single Authors")
  
  bootstrap_results$Dataset<-gsub("CHN & USA excluded", "Without China & USA",bootstrap_results$Dataset)
  figure_values$Dataset<-gsub("CHN & USA excluded", "Without China & USA",figure_values$Dataset)
  
  pEven<-
    ggplot(bootstrap_results, aes(x=Even,fill=JrnlType)) +
    geom_histogram(bins=40, color="black",fill="darkgray",
                   size=0.1,alpha=0.4, position = 'identity') +
    facet_grid(cols = vars(Dataset), rows=vars(author),
               labeller=labeller(author = author.labels),
               scales="free_y")+
    geom_hline((aes(yintercept=-Inf)), color="black") +
    geom_vline((aes(xintercept=-Inf)) , color="black")+
    coord_cartesian(clip="off")+
    xlim(.6,1)+
    # ylim(0,525)+
    # scale_x_continuous(breaks = seq(0,70, by=10),expand=c(0.1,0.02))+
    scale_y_continuous(limits = c(0, 400),breaks = seq(0,400, by=50),expand=c(0,0.1))+
    guides(fill=guide_legend("Journal\nCategory"))+
    xlab("Geographic Evenness")+
    ylab("Frequency")+
  
    
    #### SOLO AUTHORS
    # MEAN OF BOOTSTRAP
    geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries")),
                 aes(x = mean_Evenness, y = 0, xend = mean_Evenness, yend = y_Even_bar), linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries")),
              aes(x=mean_Evenness+0.05, y=y_Even_text, label=(paste("PW['mean']",as.character(mean_Evenness),sep=" == "))), 
              parse=TRUE,size=2)+
    # OA Value
    geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries")),
                 aes(x = OA_Evenness , y = 0, xend = OA_Evenness, yend = y_Even_bar), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "All Countries")),
              aes(x=OA_Evenness-0.04, y=y_Even_text, label=(paste("OA",as.character(OA_Evenness),sep=" == "))),
              parse=TRUE,color="red", size=2)+
    
    # MEAN OF BOOTSTRAP
    geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "Without China & USA")),
                 aes(x = mean_Evenness, y = 0, xend = mean_Evenness, yend = y_Even_bar), linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "Without China & USA")),
              aes(x=mean_Evenness+0.05, y=y_Even_text, label=(paste("PW['mean']",as.character(mean_Evenness),sep=" == "))), 
              parse=TRUE,size=2)+
    
    # OA Value
    geom_segment(data = subset(filter(figure_values,author == "solo" & Dataset == "Without China & USA")),
                 aes(x = OA_Evenness , y = 0, xend = OA_Evenness, yend = y_Even_bar), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "solo" & Dataset == "Without China & USA")),
              aes(x=OA_Evenness-0.05, y=y_Even_text, label=(paste("OA",as.character(OA_Evenness),sep=" == "))),
              parse=TRUE,color="red", size=2)+
    
    #### FIRST AUTHORS
    # median OF BOOTSTRAP
    geom_segment(data = subset(filter(figure_values, author == "author_first" & Dataset == "All Countries")),
                 aes(x = mean_Evenness, y = 0, xend = mean_Evenness, yend = y_Even_bar), linetype="solid")+
    geom_text(data = subset(filter(figure_values, author == "author_first" & Dataset == "All Countries")),
              aes(x=mean_Evenness-0.05, y=y_Even_text, label=(paste("PW['mean']",as.character(mean_Evenness),sep=" == "))), 
              parse=TRUE,size=2)+
    
    # OA Value
    geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries")),
                 aes(x = OA_Evenness , y = 0, xend = OA_Evenness, yend = y_Even_bar), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "All Countries")),
              aes(x=OA_Evenness+0.04, y=y_Even_text, label=(paste("OA",as.character(OA_Evenness),sep=" == "))),
              parse=TRUE,color="red", size=2)+
    
    
    # MEAN OF BOOTSTRAP
    geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "Without China & USA")),
                 aes(x = mean_Evenness, y = 0, xend = mean_Evenness, yend = y_Even_bar), linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "Without China & USA")),
              aes(x=mean_Evenness+0.06, y=y_Even_text, label=(paste("PW['mean']",as.character(mean_Evenness),sep=" == "))), 
              parse=TRUE,size=2)+
    
    # OA Value
    geom_segment(data = subset(filter(figure_values,author == "author_first" & Dataset == "Without China & USA")),
                 aes(x = OA_Evenness, y = 0, xend = OA_Evenness, yend = y_Even_bar), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values,author == "author_first" & Dataset == "Without China & USA")),
              aes(x=OA_Evenness-0.04, y=y_Even_text, label=(paste("OA",as.character(OA_Evenness),sep=" == "))), 
              parse=TRUE,color="red", size=2)
  
  pEven<-pEven+
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
  pEven<-tag_facet(pEven,open="", close="", tag_pool=facet_labels,vjust=-1)
  pEven

  return(pEven)
}
