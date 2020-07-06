Fig7<-function(DataSet,Subsampled_Countries,AuPosition,countries) {
  # DataSet<-bootstrap_results
  # Subsampled_Countries<-bootstrap_results_countries
  # AuPosition<-"author_first"
  # countries<-"All"
  library(tidyverse)
  library(RColorBrewer)
  library(ggExtra)
  
vars<-list(DataSet,Subsampled_Countries,AuPosition,countries)
  
DataSet<-as.data.frame(vars[1])
Subsampled_Countries<-as.data.frame(vars[2])
AuPosition<-vars[3]
countries<-vars[4]
  Subsampled_Countries$Region<-as.factor(Subsampled_Countries$Region) 
  Subsampled_Countries$Region<-as.factor(Subsampled_Countries$Region) 
  Subsampled_Countries$Code<-as.factor(Subsampled_Countries$Code) 
  Subsampled_Countries$Country<-as.factor(Subsampled_Countries$Country) 
  Subsampled_Countries$IncomeGroup <- ordered(Subsampled_Countries$IncomeGroup, 
                                 levels = c("Low","Lower middle","Upper middle","High"))
  
  Subsampled_Countries$Region <- ordered(Subsampled_Countries$Region, 
                                         levels =c("North America",
                                                   "South Asia","Sub-Saharan Africa",
                                                   "Latin America & Caribbean",
                                                   "Middle East & North Africa",
                                                   "East Asia & Pacific",
                                                   "Europe & Central Asia")) 
  
  
  Subsampled_Region_summary<-Subsampled_Countries %>% 
    group_by(author,Dataset,replicate,Region) %>% 
    summarize(n=n()) %>% 
    mutate(perc=n/sum(n)*100) 
  
  PW_medians<-Subsampled_Region_summary %>% 
    group_by(author,Dataset,replicate,Region) %>% 
    summarise(median=median(perc))
  PW_medians$JrnlType<-"PW"
  
  ###################################
  ###################################
  # OA 
  ###################################
  ###################################
  one_author_pubs_ALL<-read_csv(file="./data_clean/one_author_pubs_ALL.csv")
  coauthor_pubs_ALL<-read_csv(file="./data_clean/coauthor_pubs_ALL.csv")
  one_author_pubsNOCHNUSA<-read_csv(file="./data_clean/one_author_pubsNOCHNUSA.csv")
  coauthor_pubsNOCHNUSA<-read_csv(file="./data_clean/coauthor_pubsNOCHNUSA.csv")
  
  coauthor_pubs_ALL_last<-coauthor_pubs_ALL %>% 
    group_by(DOI) %>% 
    filter(AuthorNum == max(AuthorNum)) %>%
    filter(AuthorNum>1)
  coauthor_pubs_ALL_last$author<-"author_last"
  
  coauthor_pubs_ALL_first<-coauthor_pubs_ALL %>% 
    group_by(DOI) %>% 
    filter(AuthorNum == 1)
  coauthor_pubs_ALL_first$author<-"author_first"
    
  coauthor_pubsNOCHNUSA_last<-coauthor_pubsNOCHNUSA %>% 
    group_by(DOI) %>% 
    filter(AuthorNum == max(AuthorNum)) %>%
    filter(AuthorNum>1)
  coauthor_pubsNOCHNUSA_last$author<-"author_last"
  
  coauthor_pubsNOCHNUSA_first<-coauthor_pubsNOCHNUSA %>% 
    group_by(DOI) %>% 
    filter(AuthorNum == 1) 
  coauthor_pubsNOCHNUSA_first$author<-"author_first"
  
  AllPubs<-bind_rows(one_author_pubs_ALL,
                     one_author_pubsNOCHNUSA,
                     coauthor_pubs_ALL_last,
                     coauthor_pubs_ALL_first,
                     coauthor_pubsNOCHNUSA_last,
                     coauthor_pubsNOCHNUSA_first)
  
  levels(as.factor(AllPubs$author))
  
  OAData<-AllPubs %>% filter(JrnlType=="OA")
  
  OA_percs<-OAData %>% 
    group_by(author,Dataset,Region) %>% 
    summarize(n=n()) %>% 
    mutate(perc=n/sum(n)*100)

  
  OA_percs$Region <- ordered(OA_percs$Region, 
                                         levels =c("North America",
                                                   "South Asia","Sub-Saharan Africa",
                                                   "Latin America & Caribbean",
                                                   "Middle East & North Africa",
                                                   "East Asia & Pacific",
                                                   "Europe & Central Asia")) 

Subsampled_Region_summary_plot<-Subsampled_Region_summary %>% 
    filter(author!="author_all")
  
  
  
  
  
  author.labels <- c(author_first = "First Authors", author_last = "Last Authors", solo= "Single Authors")
  color.labels<-c("Low"= "#A6CEE3", 'Lower middle'="#1F78B4",'Upper middle'="#B2DF8A",'High'="#33A02C")
  Subsampled_Region_summary_plot$author<-as.factor(Subsampled_Region_summary_plot$author)
  Subsampled_Region_summary_plot$author<-ordered(Subsampled_Region_summary_plot$author, levels = c("solo", "author_first", "author_last","author_all"))
  
  # DATA SELECTION FOR Figure 
  if (((vars[3]=="author_first")==TRUE)) {
    # # title_text=paste("Fig. 3a: Country in which the first",
    # "author of open access articles is based.",sep=" ")
    fig_data<-Subsampled_Region_summary_plot %>% filter(author=="author_first")
    label_data<-OA_percs %>% filter(author=="author_first")
  } else if (((vars[3]=="author_last"))==TRUE){
    # title_text=paste("Fig. 3b: Country in which the first",
    #                  "author of paywalled articles is based.",sep=" ")
    fig_data<-Subsampled_Region_summary_plot %>% filter(author=="author_last")
    label_data<-OA_percs %>% filter(author=="author_last")
  } else if (((vars[3]=="solo")==TRUE)) {
    # title_text=paste("Fig. 3c: Country in which the last",
    #                  "author of open access articles is based.",sep=" ")
    fig_data<-Subsampled_Region_summary_plot %>% filter(author=="solo")
    label_data<-OA_percs %>% filter(author=="solo")
  } else {
    stop("Please enter 'author_first' or 'author_last' or 'solo' ")
  }

  if (((vars[4]=="All")==TRUE)) {
    fig_data<-fig_data %>% filter(Dataset=="All Countries")
    label_data<-label_data %>% filter(Dataset=="All Countries")
  } else if (((vars[4]=="no_CHN_USA")==TRUE)) {
    fig_data<-fig_data %>% filter(Dataset=="CHN & USA excluded")
    label_data<-label_data %>% filter(Dataset=="CHN & USA excluded")
  } else {
    stop("Please enter 'All' or 'no_CHN_USA' ")
  }
  
  Region<-OA_percs$Region
  Region<-recode_factor(Region,"North America"="N America", 
                        "Latin America & Caribbean"="Lat Am & Carib",
                        "Middle East & North Africa"="M East & N Africa",
                        "East Asia & Pacific"="E Asia & Pacific",
                        "Europe & Central Asia"="Europe & C Asia",
                        .default = levels(Region))
  OA_percs$Region<-Region  
  
    
  Region<-fig_data$Region
  Region<-recode_factor(Region,"North America"="N America", 
                        "Latin America & Caribbean"="Lat Am & Carib",
                        "Middle East & North Africa"="M East & N Africa",
                        "East Asia & Pacific"="E Asia & Pacific",
                        "Europe & Central Asia"="Europe & C Asia",
                        .default = levels(Region))
  
  fig_data$Region<-Region  
  
  Region<-label_data$Region
  Region<-recode_factor(Region,"North America"="N America", 
                        "Latin America & Caribbean"="Lat Am & Carib",
                        "Middle East & North Africa"="M East & N Africa",
                        "East Asia & Pacific"="E Asia & Pacific",
                        "Europe & Central Asia"="Europe & C Asia",
                        .default = levels(Region))
  
  label_data$Region<-Region  
  
  levels(fig_data$Region)
  levels(OA_percs$Region)
  
  Fig7<-ggplot(fig_data,aes(x=perc,fill=Region)) +
    geom_histogram(bins=100,color="black", size=0.1,alpha=0.4, position = 'identity') +
    scale_fill_brewer(palette = "Paired")+
    ylab("Global Region") + 
    xlab("Percentage of authors in each region")+
    facet_grid(cols = vars(author), 
               rows=vars(Region),
               labeller=labeller(author = author.labels))+
    scale_x_continuous(breaks = seq(0,100, by=10),expand = c(0, 0))+
    scale_y_continuous(breaks = seq(0,650, by=150),expand = c(0.1, 0))+
    # brewer.pal(4, "paired") gets the hex codes from palette
    # coord_flip()+
    ## OA POINTS LINES AND LABELS
    geom_segment(data = label_data,
                 aes(x = perc,
                     y = as.numeric(Region)+.7,
                     xend = perc, 
                     yend = 400), 
                 linetype="solid", color="red")+
    geom_text(data = label_data,
              aes(x=perc,
                  y=450,
                  label = "OA"),color="red", size=2)
  Fig7
  
  Fig7<-Fig7+
    theme_bw()+
    theme(
      axis.text.x = element_text(size=10,angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size=10),
      axis.title.x=element_text(colour="black", size = 10, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 10, vjust=2),
      strip.text.x = element_text(size = 10),
      strip.background = element_rect(colour = "black", fill = "white"),
      strip.text.y = element_text(size = 6),
      panel.spacing.x =unit(2, "lines"), 
      panel.spacing.y=unit(0.5,"lines"),
      legend.position = "right",
      plot.margin =unit(c(1,3,1,1.5), "lines")   #plot margin - top, right, bottom, left
    )    
  Fig7   

  return(Fig7)
  
}