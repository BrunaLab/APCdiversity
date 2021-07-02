IncomePlot<-function(DataSet,Subsampled_Countries,countries,sole_ALL,sole_NOCHNUSA,first_ALL,first_NOCHNUSA) {
  # DataSet<-Boot_RichDiv
  # Subsampled_Countries<-Boot_Countries
  # countries<-"All"
  # countries<-"No_CHN_USA"
  library(tidyverse)
  library(RColorBrewer)
  library(ggExtra)
  library(egg)

  
  
  DataSet<-DataSet %>% 
    # mutate(OA_group = ifelse(OA_group == "OA", "PW", author)) %>% 
    mutate(author = ifelse(Dataset == "sole_NOCHNUSA", "solo", author)) %>% 
    mutate(author = ifelse(Dataset == "sole_ALL", "solo", author)) %>% 
    mutate(author = ifelse(Dataset == "first_NOCHNUSA", "author_first", author)) %>% 
    mutate(author = ifelse(Dataset == "first_ALL", "author_first", author)) 
    # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset)) %>%
    # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset))
  
  
  
  
  Subsampled_Countries<-Subsampled_Countries %>%
    mutate(Dataset = ifelse(Dataset == "sole_ALL", "All Countries", Dataset)) %>%
    mutate(Dataset = ifelse(Dataset == "first_ALL", "All Countries", Dataset)) %>%
    mutate(Dataset = ifelse(Dataset == "sole_NOCHNUSA","CHN & USA excluded", Dataset)) %>%
    mutate(Dataset = ifelse(Dataset == "first_NOCHNUSA","CHN & USA excluded", Dataset))
  # 
  
vars<-list(DataSet,Subsampled_Countries,countries,sole_ALL,sole_NOCHNUSA,first_ALL,first_NOCHNUSA)
  
DataSet<-as.data.frame(vars[1])
Subsampled_Countries<-as.data.frame(vars[2])
countries<-vars[3]
  Subsampled_Countries$IncomeGroup<-as.factor(Subsampled_Countries$IncomeGroup) 
  Subsampled_Countries$Region<-as.factor(Subsampled_Countries$Region) 
  Subsampled_Countries$Code<-as.factor(Subsampled_Countries$Code) 
  Subsampled_Countries$Country<-as.factor(Subsampled_Countries$Country) 
  
  levels(Subsampled_Countries$IncomeGroup)[levels(Subsampled_Countries$IncomeGroup)=="Low"] <- "Low\nIncome"
  levels(Subsampled_Countries$IncomeGroup)[levels(Subsampled_Countries$IncomeGroup)=="Lower middle"] <- "Lower-middle\nIncome"
  levels(Subsampled_Countries$IncomeGroup)[levels(Subsampled_Countries$IncomeGroup)=="Upper middle"] <- "Upper-middle\nIncome"
  levels(Subsampled_Countries$IncomeGroup)[levels(Subsampled_Countries$IncomeGroup)=="High"] <- "High\nIncome"
  
  Subsampled_Countries$IncomeGroup <- ordered(Subsampled_Countries$IncomeGroup, 
                                              levels = c("Low\nIncome","Lower-middle\nIncome","Upper-middle\nIncome","High\nIncome"))
  
  
  as.factor(Subsampled_Countries$Dataset)
  as.factor(Subsampled_Countries$ArticleCat)
  
  Subsampled_Income_summary<-Subsampled_Countries %>% 
    group_by(author,Dataset,ArticleCat,replicate,IncomeGroup) %>% 
    summarize(n=n()) %>% 
    mutate(perc=n/sum(n)*100) 
  
  PW_medians<-Subsampled_Income_summary %>% 
    filter(ArticleCat=="PW") %>% 
    group_by(author,Dataset,replicate,IncomeGroup) %>% 
    summarise(median=median(perc))
  PW_medians$ArticleCat<-"PW"
  
  
  OAinPW_medians<-Subsampled_Income_summary %>% 
    filter(ArticleCat=="OAinPW") %>% 
    group_by(author,Dataset,replicate,IncomeGroup) %>% 
    summarise(median=median(perc))
  OAinPW_medians$ArticleCat<-"OAinPW"
  
  ###################################
  # OA 
  ###################################
  
  sole_ALL<-sole_ALL %>% drop_na(IncomeGroup)
  first_ALL<-first_ALL %>% drop_na(IncomeGroup)
  sole_NOCHNUSA<-sole_NOCHNUSA %>% drop_na(IncomeGroup)
  first_NOCHNUSA<-first_NOCHNUSA %>% drop_na(IncomeGroup)
  
  first_ALL$Dataset<-"All Countries"
  sole_ALL$Dataset<-"All Countries"
  first_ALL$author<-"author_first"
  sole_ALL$author<-"solo"
  sole_NOCHNUSA$Dataset<-"CHN & USA excluded"
  first_NOCHNUSA$Dataset<-"CHN & USA excluded"
  sole_NOCHNUSA$author<-"solo"
  first_NOCHNUSA$author<-"author_first"
  
  AllPubs<-bind_rows(sole_ALL,
                     sole_NOCHNUSA,
                     first_ALL,
                     first_NOCHNUSA)
  
  levels(as.factor(AllPubs$author))
  levels(as.factor(AllPubs$Dataset))
  AllPubs$IncomeGroup<-as.factor(AllPubs$IncomeGroup)
  levels(AllPubs$IncomeGroup)
  levels(AllPubs$IncomeGroup)[levels(AllPubs$IncomeGroup)=="Low"] <- "Low\nIncome"
  levels(AllPubs$IncomeGroup)[levels(AllPubs$IncomeGroup)=="Lower middle"] <- "Lower-middle\nIncome"
  levels(AllPubs$IncomeGroup)[levels(AllPubs$IncomeGroup)=="Upper middle"] <- "Upper-middle\nIncome"
  levels(AllPubs$IncomeGroup)[levels(AllPubs$IncomeGroup)=="High"] <- "High\nIncome"
  levels(as.factor(AllPubs$IncomeGroup))
  levels(as.factor(AllPubs$author))

  OAData<-AllPubs %>% filter(JrnlType=="OA" |JrnlType=="OAinPW") %>% drop_na(IncomeGroup)
  
  
  levels(as.factor(OAData$IncomeGroup))
  OA_percs<-OAData %>% 
    group_by(author,Dataset,JrnlType,IncomeGroup) %>% 
    summarize(n=n()) %>% 
    mutate(perc=n/sum(n)*100)

  OA_percs$IncomeGroup <- ordered(OA_percs$IncomeGroup, 
                                  levels = c("Low\nIncome","Lower-middle\nIncome","Upper-middle\nIncome","High\nIncome"))
  OA_percs$color<-NA
  
  OA_percs$color[OA_percs$IncomeGroup == "Low\nIncome"] <- "'#A6CEE3'"
  OA_percs$color[OA_percs$IncomeGroup == "Lower-middle\nIncome"] <- "'#1F78B4'"
  OA_percs$color[OA_percs$IncomeGroup == "Upper-middle\nIncome"] <- "'#B2DF8A'"
  OA_percs$color[OA_percs$IncomeGroup == "High\nIncome"] <- "'#33A02C'"
      
  
  
  Subsampled_Income_summary_plot<-Subsampled_Income_summary %>% 
    filter(author!="author_all")
  Subsampled_Income_summary_plot<-Subsampled_Income_summary_plot %>% drop_na(IncomeGroup)
  
  
  
  
  
  author.labels <- c(author_first = "First Authors", solo= "Single Authors")
  color.labels<-c("Low\nIncome"= "#A6CEE3", 'Lower-middle\nIncome'="#1F78B4",'Upper-middle\nIncome'="#B2DF8A",'High\nIncome'="#33A02C")
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

  
  Subsampled_Income_summary_plot$author <- factor(Subsampled_Income_summary_plot$author,
                                     levels = c("solo","author_first"))
  
  label_data$author <- factor(label_data$author,levels = c("solo","author_first"))
  
  
  # as.factor(Subsampled_Income_summary_plot$ArticleCat)
  # as.factor(Subsampled_Income_summary_plot$author)
  
  
  IncomePlot<-ggplot(Subsampled_Income_summary_plot,aes(x=perc,fill=IncomeGroup)) +
    geom_histogram(bins=100,color="black", size=0.5, position = 'identity') +
    # scale_fill_brewer(palette = "Blues")+
    ylab("Frequency") + 
    xlab("Percentage of authors in each World Bank Lending Group")+
    # ylim(0,1000)+
    scale_y_continuous(limits = c(0, 800),breaks = seq(0,800, by=200),expand=c(0,0.1))+
    scale_x_continuous(limits = c(0, 90),breaks = seq(0,80, by=20),expand=c(0,0.1))+
    scale_fill_manual(values=c("#F7FBFF","#9ECAE1","#4292C6","#084594"),
                      name="National Income Category",
                      breaks=c("High\nIncome", "Upper-middle\nIncome","Lower-middle\nIncome","Low\nIncome"))+
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
                     yend = 350), 
                 linetype="solid", color="red")+
    
    geom_text(data = label_data,
              aes(x=perc,
                  y=360,
                  label = "OA"),color="red", size=8)+
  
    geom_segment(data = label_data,
                 aes(x = perc,
                     y = as.numeric(IncomeGroup)-2,
                     xend = perc, 
                     yend = 350), 
                 linetype="solid", color="blue")+
    
    geom_text(data = label_data,
              aes(x=perc,
                  y=400,
                  label = "OA"),color="blue", size=8)

  IncomePlot<-IncomePlot+
    theme_classic()+
    theme(
      axis.text.x = element_text(size=20),
      axis.text.y = element_text(size=20),
      axis.title.x=element_text(colour="black", size = 25, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 25, vjust=2),
      strip.text.x = element_text(size = 30,margin = margin(5,0,3,0, "lines")),
      strip.text.y = element_text(size = 30, angle=0),
      strip.background.x = element_rect(fill = NA, colour = NA),
      strip.background.y = element_rect(fill = NA, colour = NA),
      panel.spacing.x =unit(2, "lines"), 
      panel.spacing.y=unit(4,"lines"),
      legend.position = "none",
      legend.text = element_text(size=20),
      # plot.margin =unit(c(1,1,1,1.5), "lines")   #plot margin - top, right, bottom, left
    )        
  facet_labels<-c("A","B","C","D","E","F","G","H")
  IncomePlot<-tag_facet(IncomePlot,open="", close="", tag_pool=facet_labels,vjust=0.5,hjust=-1,size=10)
  IncomePlot   
 
  # P_HAT
  
  # single, all
  label_data<-ungroup(label_data)
  P_Hat<-label_data
  P_Hat$P_Hat<-NA
  P_Hat$color<-NULL
  ##########
  # All countries, coauthored, High
  crit<-label_data %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    filter(IncomeGroup=="High\nIncome") %>% 
    select(perc)
  
  perc<-Subsampled_Income_summary_plot %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    filter(IncomeGroup=="High\nIncome") %>% 
    ungroup() %>% 
    tally(perc<crit$perc) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  P_Hat$P_Hat[P_Hat$IncomeGroup=="High\nIncome" &
                P_Hat$author=="author_first" & 
                P_Hat$Dataset=="All Countries"]<-perc_belowOA
  ###########
  
  ##########
  # All countries, coauthored, Lower-middle\nIncome
  crit<-label_data %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    filter(IncomeGroup=="Lower-middle\nIncome") %>% 
    select(perc)
  
  perc<-Subsampled_Income_summary_plot %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    filter(IncomeGroup=="Lower-middle\nIncome") %>% 
    ungroup() %>% 
    tally(perc<crit$perc) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  
  P_Hat$P_Hat[P_Hat$IncomeGroup=="Lower-middle\nIncome" &
                P_Hat$author=="author_first" & 
                P_Hat$Dataset=="All Countries"]<-perc_belowOA
  
  ###########
  
  ##########
  
  # All countries, coauthored, Upper-middle\nIncome
  crit<-label_data %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    filter(IncomeGroup=="Upper-middle\nIncome") %>% 
    select(perc)
  
  perc<-Subsampled_Income_summary_plot %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    filter(IncomeGroup=="Upper-middle\nIncome") %>% 
    ungroup() %>% 
    tally(perc<crit$perc) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  
  P_Hat$P_Hat[P_Hat$IncomeGroup=="Upper-middle\nIncome" &
                P_Hat$author=="author_first" & 
                P_Hat$Dataset=="All Countries"]<-perc_belowOA
  ###########
  
  
  ##########
  # All countries, coauthored, Low\nIncome
  crit<-label_data %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    filter(IncomeGroup=="Low\nIncome") %>% 
    select(perc)
  
  perc<-Subsampled_Income_summary_plot %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="author_first") %>% 
    filter(IncomeGroup=="Low\nIncome") %>% 
    ungroup() %>% 
    tally(perc<crit$perc) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  
  
  P_Hat$P_Hat[P_Hat$IncomeGroup=="Low\nIncome" &
                P_Hat$author=="author_first" & 
                P_Hat$Dataset=="All Countries"]<-perc_belowOA
  ###########
  
  ##########
  # All countries, coauthored, High
  crit<-label_data %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="solo") %>% 
    filter(IncomeGroup=="High\nIncome") %>% 
    select(perc)
  
  perc<-Subsampled_Income_summary_plot %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="solo") %>% 
    filter(IncomeGroup=="High\nIncome") %>% 
    ungroup() %>% 
    tally(perc<crit$perc) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  P_Hat$P_Hat[P_Hat$IncomeGroup=="High\nIncome" &
                P_Hat$author=="solo" & 
                P_Hat$Dataset=="All Countries"]<-perc_belowOA
  ###########
  
  ##########
  # All countries, coauthored, Lower-middle\nIncome
  crit<-label_data %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="solo") %>% 
    filter(IncomeGroup=="Lower-middle\nIncome") %>% 
    select(perc)
  
  perc<-Subsampled_Income_summary_plot %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="solo") %>% 
    filter(IncomeGroup=="Lower-middle\nIncome") %>% 
    ungroup() %>% 
    tally(perc<crit$perc) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  
  P_Hat$P_Hat[P_Hat$IncomeGroup=="Lower-middle\nIncome" &
                P_Hat$author=="solo" & 
                P_Hat$Dataset=="All Countries"]<-perc_belowOA
  
  ###########
  
  ##########
  
  # All countries, coauthored, Upper-middle\nIncome
  crit<-label_data %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="solo") %>% 
    filter(IncomeGroup=="Upper-middle\nIncome") %>% 
    select(perc)
  
  perc<-Subsampled_Income_summary_plot %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="solo") %>% 
    filter(IncomeGroup=="Upper-middle\nIncome") %>% 
    ungroup() %>% 
    tally(perc<crit$perc) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  
  P_Hat$P_Hat[P_Hat$IncomeGroup=="Upper-middle\nIncome" &
                P_Hat$author=="solo" & 
                P_Hat$Dataset=="All Countries"]<-perc_belowOA
  ###########
  
  
  ##########
  # All countries, coauthored, Low\nIncome
  crit<-label_data %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="solo") %>% 
    filter(IncomeGroup=="Low\nIncome") %>% 
    select(perc)
  
  perc<-Subsampled_Income_summary_plot %>% 
    filter(Dataset=="All Countries") %>% 
    filter(author=="solo") %>% 
    filter(IncomeGroup=="Low\nIncome") %>% 
    ungroup() %>% 
    tally(perc<crit$perc) %>% 
    mutate(perc_belowOA = n/1000)
  perc_belowOA<-perc$perc_belowOA
  perc_belowOA
  
  
  
  P_Hat$P_Hat[P_Hat$IncomeGroup=="Low\nIncome" &
                P_Hat$author=="solo" & 
                P_Hat$Dataset=="All Countries"]<-perc_belowOA
  ###########
  P_Hat
  
  levels(P_Hat$IncomeGroup)[levels(P_Hat$IncomeGroup)=="Low\nIncome"] <-"Low" 
  levels(P_Hat$IncomeGroup)[levels(P_Hat$IncomeGroup)=="Lower-middle\nIncome"] <- "Lower middle"
  levels(P_Hat$IncomeGroup)[levels(P_Hat$IncomeGroup)=="Upper-middle\nIncome"] <- "Upper middle"
  levels(P_Hat$IncomeGroup)[levels(P_Hat$IncomeGroup)=="High\nIncome"] <-"High" 
  
  
  P_Hat <- P_Hat %>% dplyr::rename("OA_perc"="perc", 
                                   "Author"="author",
                                   "Countries"="Dataset"
  ) %>% 
    select(-n) 
  
  
  P_Hat$Author<-gsub("author_first","First",P_Hat$Author)
  P_Hat$Author<-gsub("solo","Single",P_Hat$Author)
  
  
  
   return(list(IncomePlot,P_Hat))
  
}