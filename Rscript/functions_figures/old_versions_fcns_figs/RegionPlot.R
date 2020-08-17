RegionPlot<-function(DataSet,Subsampled_Countries,countries) {
  # DataSet<-bootstrap_results
  # Subsampled_Countries<-bootstrap_results_countries
  # countries<-"All"
  # countries<-"No_CHN_USA"
  library(tidyverse)
  library(RColorBrewer)
  library(ggExtra)

vars<-list(DataSet,Subsampled_Countries,countries)
  
DataSet<-as.data.frame(vars[1])
Subsampled_Countries<-as.data.frame(vars[2])
countries<-vars[3]
  Subsampled_Countries$IncomeGroup<-as.factor(Subsampled_Countries$IncomeGroup) 
  Subsampled_Countries$Region<-as.factor(Subsampled_Countries$Region) 
  Subsampled_Countries$Code<-as.factor(Subsampled_Countries$Code) 
  Subsampled_Countries$Country<-as.factor(Subsampled_Countries$Country) 
  Subsampled_Countries$IncomeGroup <- ordered(Subsampled_Countries$IncomeGroup, 
                                 levels = c("Low","Lower middle","Upper middle","High"))
  
  Subsampled_Income_summary<-Subsampled_Countries %>% 
    group_by(author,Dataset,replicate,IncomeGroup) %>% 
    summarize(n=n()) %>% 
    mutate(perc=n/sum(n)*100) 
  
  PW_medians<-Subsampled_Income_summary %>% 
    group_by(author,Dataset,replicate,IncomeGroup) %>% 
    summarise(median=median(perc))
  PW_medians$JrnlType<-"PW"
  
  ###################################
  # OA 
  ###################################
  
  one_author_pubs_ALL<-read_csv(file="./data_clean/one_author_pubs_ALL.csv")
  one_author_pubs_ALL<-one_author_pubs_ALL %>% drop_na(IncomeGroup)
  
  coauthor_pubs_ALL<-read_csv(file="./data_clean/coauthor_pubs_ALL.csv")
  coauthor_pubs_ALL<-coauthor_pubs_ALL %>% drop_na(IncomeGroup)
  
  one_author_pubsNOCHNUSA<-read_csv(file="./data_clean/one_author_pubsNOCHNUSA.csv")
  one_author_pubsNOCHNUSA<-one_author_pubsNOCHNUSA %>% drop_na(IncomeGroup)
  
  coauthor_pubsNOCHNUSA<-read_csv(file="./data_clean/coauthor_pubsNOCHNUSA.csv")
  coauthor_pubsNOCHNUSA<-coauthor_pubsNOCHNUSA %>% drop_na(IncomeGroup)
  
  coauthor_pubs_ALL$Dataset<-"All Countries"
  one_author_pubs_ALL$Dataset<-"All Countries"
  coauthor_pubs_ALL$author<-"author_first"
  one_author_pubs_ALL$author<-"solo"
  one_author_pubsNOCHNUSA$Dataset<-"CHN & USA excluded"
  coauthor_pubsNOCHNUSA$Dataset<-"CHN & USA excluded"
  one_author_pubsNOCHNUSA$author<-"solo"
  coauthor_pubsNOCHNUSA$author<-"author_first"
  
  AllPubs<-bind_rows(one_author_pubs_ALL,
                     one_author_pubsNOCHNUSA,
                     coauthor_pubs_ALL,
                     coauthor_pubsNOCHNUSA)
  
  levels(as.factor(AllPubs$author))
  levels(as.factor(AllPubs$Dataset))
  levels(as.factor(AllPubs$IncomeGroup))
  
  levels(as.factor(AllPubs$IncomeGroup))
  levels(as.factor(AllPubs$author))

  OAData<-AllPubs %>% filter(JrnlType=="OA") %>% drop_na(IncomeGroup)
  
  
  levels(as.factor(OAData$IncomeGroup))
  OA_percs<-OAData %>% 
    group_by(author,Dataset,IncomeGroup) %>% 
    summarize(n=n()) %>% 
    mutate(perc=n/sum(n)*100)

  OA_percs$IncomeGroup <- ordered(OA_percs$IncomeGroup, 
                                  levels = c("Low",
                                             "Lower middle",
                                             "Upper middle",
                                             "High"))
  OA_percs$color<-NA
  
  OA_percs$color[OA_percs$IncomeGroup == "Low"] <- "'#A6CEE3'"
  OA_percs$color[OA_percs$IncomeGroup == "Lower middle"] <- "'#1F78B4'"
  OA_percs$color[OA_percs$IncomeGroup == "Upper middle"] <- "'#B2DF8A'"
  OA_percs$color[OA_percs$IncomeGroup == "High"] <- "'#33A02C'"
      
  
  
  Subsampled_Income_summary_plot<-Subsampled_Income_summary %>% 
    filter(author!="author_all")
  Subsampled_Income_summary_plot<-Subsampled_Income_summary_plot %>% drop_na(IncomeGroup)
  
  
  
  
  
  author.labels <- c(author_first = "First Authors", solo= "Single Authors")
  color.labels<-c("Low"= "#A6CEE3", 'Lower middle'="#1F78B4",'Upper middle'="#B2DF8A",'High'="#33A02C")
  Subsampled_Income_summary_plot$author<-as.factor(Subsampled_Income_summary_plot$author)
  Subsampled_Income_summary_plot$author<-ordered(Subsampled_Income_summary_plot$author, levels = c("solo", "author_first"))
  # 
  # DATA SELECTION FOR Figure

  if (((vars[3]=="All")==TRUE)) {
    fig_data<-Subsampled_Income_summary_plot  %>% filter(Dataset=="All Countries")
    label_data<-OA_percs %>% filter(Dataset=="All Countries")
  } else if (((vars[3]=="no_CHN_USA")==TRUE)) {
    fig_data<-Subsampled_Income_summary_plot  %>%filter(Dataset=="CHN & USA excluded")
    label_data<-OA_percs %>% filter(Dataset=="CHN & USA excluded")
  } else {
    stop("Please enter 'All' or 'no_CHN_USA' ")
  }

  
  RegionPlot<-ggplot(Subsampled_Income_summary_plot,aes(x=perc,fill=IncomeGroup)) +
    geom_histogram(bins=100,color="black", size=0.5, position = 'identity') +
    # scale_fill_brewer(palette = "Blues")+
    ylab("Frequency (No. of Bootstrap Runs)") + 
    xlab("Percentage of authors in each group")+
    xlim(0,110)+
    scale_fill_manual(values=c("#F7FBFF","#9ECAE1","#4292C6","#084594"),
                      name="National Income Category",
                      breaks=c("High", "Upper middle","Lower middle","Low"))+
    labs(fill = "National Income Group")+
    facet_grid(cols = vars(author), 
               rows=vars(IncomeGroup),
               labeller=labeller(author = author.labels),
               scales="free_y")+
    geom_hline((aes(yintercept=-Inf)) , color="black")+ 
    geom_vline((aes(xintercept=-Inf)) , color="black")+ 
    coord_cartesian(clip="off")+
    # scale_x_continuous(breaks = seq(0,100, by=10),expand = c(0.0, 0))+
    # brewer.pal(4, "paired") gets the hex codes from palette
    # coord_flip()+
    ## OA POINTS LINES AND LABELS
    geom_segment(data = label_data,
                 aes(x = perc,
                     y = as.numeric(IncomeGroup)-2,
                     xend = perc, 
                     yend = 400), 
                 linetype="solid", color="red")+
    
    geom_text(data = label_data,
              aes(x=perc,
                  y=420,
                  label = "OA"),color="red", size=8)
  

  RegionPlot<-RegionPlot+
    theme_classic()+
    theme(
      axis.text.x = element_text(size=25),
      axis.text.y = element_text(size=25),
      axis.title.x=element_text(colour="black", size = 35, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 35, vjust=2),
      strip.text.x = element_text(size = 35),
      strip.background.x = element_rect(fill = NA, colour = NA),
      strip.background.y = element_rect(fill = NA, colour = NA),
      strip.text.y = element_text(size = 35, angle=0),
      panel.spacing.x =unit(2, "lines"), 
      panel.spacing.y=unit(2,"lines"),
      legend.position = "none",
      legend.text = element_text(size=25),
      plot.margin =unit(c(1,1,1,1.5), "lines")   #plot margin - top, right, bottom, left
    )        
  RegionPlot   

  return(RegionPlot)
  
}