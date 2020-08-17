AltFig1<-function(DataSet,Subsampled_Countries) {
  # DataSet<-bootstrap_results
  # Subsampled_Countries<-bootstrap_results_countries
  library(tidyverse)
  library(RColorBrewer)
  library(ggExtra)
  library(ggridges)  
  
  Subsampled_Countries$IncomeGroup<-as.factor(Subsampled_Countries$IncomeGroup) 
  Subsampled_Countries<-Subsampled_Countries %>% drop_na("IncomeGroup")
  
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
  ###################################
  # OA 
  ###################################
  ###################################
  
  # Need to generate subsets of sole-author pubs, coauthor pubs 
  AllData<-read_csv(file="./data_clean/all_data_analysis.csv")
  # n_distinct(AllData$refID)
  # n_distinct(AllData$Title)
  # n_distinct(AllData$DOI)
  
  AllData_missing<-AllData %>% 
    filter(AuthorNum==1) %>% 
    filter(is.na(First_Author_Country)) %>% 
    select(refID) 
  AllData<-AllData %>% filter(!refID %in% AllData_missing$refID) 
  
  
  
  
  sole_author_pubs_ALL <- AllData %>%
    group_by(refID) %>%
    summarize(n=n_distinct(AuthorNum)) %>%
    filter(n==1) %>%
    select(-n)
  # n_distinct(sole_author_pubs_ALL$refID)
  sole_author_pubs_ALL <-AllData %>% 
    filter(refID %in% sole_author_pubs_ALL$refID) 
  # n_distinct(sole_author_pubs_ALL$refID)
  
  coauthor_pubs_ALL<- AllData %>%
    group_by(refID) %>%
    summarize(n=n_distinct(AuthorNum)) %>%
    filter(n>=2) %>%
    select(-n)
  # n_distinct(coauthor_pubs_ALL$refID)
  coauthor_pubs_ALL <-AllData %>% 
    filter(refID %in% coauthor_pubs_ALL$refID) 
  # n_distinct(coauthor_pubs_ALL$refID)
  
  
  
  
  # subset of data with no first authors form CHN or USA
  pubsNOCHNUSA<-AllData %>%
    select(refID,Code,AuthorNum) %>% 
    filter(AuthorNum==1) %>% 
    filter(Code!="CHN") %>% 
    filter(Code!="USA") %>% 
    select(refID) 
  
  # n_distinct(pubsNOCHNUSA$refID)
  pubsNOCHNUSA <-AllData %>% 
    filter(refID %in% pubsNOCHNUSA$refID) 
  # n_distinct(pubsNOCHNUSA$refID)
  
  
  sole_author_pubsNOCHNUSA_first_author<-pubsNOCHNUSA %>% 
    group_by(refID) %>%
    summarize(n=n_distinct(AuthorNum)) %>%
    filter(n==1) %>%
    select(-n) 
  # n_distinct(sole_author_pubsNOCHNUSA_first_author$refID)
  sole_author_pubsNOCHNUSA_first_author <-pubsNOCHNUSA %>% 
    filter(refID %in% sole_author_pubsNOCHNUSA_first_author$refID) %>% 
    mutate(Dataset="CHN & USA excluded") %>% 
    mutate(author="solo") %>% 
    drop_na("Code")
  # n_distinct(sole_author_pubsNOCHNUSA_first_author$refID)
  
  
  coauthor_pubsNOCHNUSA_first_author<- pubsNOCHNUSA %>%
    group_by(refID) %>%
    summarize(n=n_distinct(AuthorNum)) %>%
    filter(n>=2) %>%
    select(-n) 
  # n_distinct(coauthor_pubsNOCHNUSA_first_author$refID)
  coauthor_pubsNOCHNUSA_first_author <-pubsNOCHNUSA %>% 
    filter(refID %in% coauthor_pubsNOCHNUSA_first_author$refID) %>% 
    group_by(refID) %>%
    filter(AuthorNum==1) %>%
    mutate(Dataset="CHN & USA excluded")%>% 
    mutate(author="author_first") %>% 
    drop_na("Code")
  # n_distinct(coauthor_pubsNOCHNUSA_first_author$refID)
  
  # n_distinct(coauthor_pubsNOCHNUSA_first_author$refID)+
  #   n_distinct(sole_author_pubsNOCHNUSA_first_author$refID)
  # n_distinct(pubsNOCHNUSA$refID)
  
  sole_author_pubs_ALL_first_author<-sole_author_pubs_ALL %>% 
    group_by(refID) %>%
    filter(AuthorNum==1) %>%
    mutate(Dataset="All Countries") %>% 
    mutate(author="solo") %>% 
    drop_na("Code")
  
  # n_distinct(sole_author_pubs_ALL_first_author$refID)
  # n_distinct(sole_author_pubs_ALL$refID)
  
  
  coauthor_pubs_ALL_first_author<-coauthor_pubs_ALL %>% 
    group_by(refID) %>%
    filter(AuthorNum==1) %>%
    mutate(Dataset="All Countries") %>% 
    mutate(author="author_first") %>% 
    drop_na("Code")
  
  # n_distinct(coauthor_pubs_ALL_first_author$refID)
  # n_distinct(coauthor_pubs_ALL$refID)
  
  AllPubs<-bind_rows(sole_author_pubs_ALL_first_author,
                     coauthor_pubs_ALL_first_author,
                     sole_author_pubsNOCHNUSA_first_author,
                     coauthor_pubsNOCHNUSA_first_author)
  
  # AllPubs<-bind_rows(one_author_pubs_ALL,
  #                    coauthor_pubs_ALL,
  #                    sole_author_pubsNOCHNUSA,
  #                    first_author_coauthor_pubsNOCHNUSA)
  # 
  AllPubs<-AllPubs %>% drop_na("IncomeGroup")
  
  summary(as.factor(AllPubs$IncomeGroup))
  
  levels(as.factor(AllPubs$author))
  
  OAData<-AllPubs %>% filter(JrnlType=="OA")
  summary(as.factor(OAData$IncomeGroup))
  OA_percs<-OAData %>% 
    group_by(author,Dataset,IncomeGroup) %>% 
    summarize(n=n()) %>% 
    mutate(perc=n/sum(n)*100)
  summary(as.factor(OA_percs$IncomeGroup))
  
  
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
  
  author.labels <- c(author_first = "First Authors", solo= "Single Authors")
  color.labels<-c("Low"= "#A6CEE3", 'Lower middle'="#1F78B4",'Upper middle'="#B2DF8A",'High'="#33A02C")
  
  Subsampled_Income_summary_plot<-Subsampled_Income_summary_plot %>% filter(Dataset=="All Countries")
  OA_percs<-OA_percs %>% filter(Dataset=="All Countries")
  
  summary(as.factor(Subsampled_Income_summary_plot$IncomeGroup))
  Fig1<-ggplot(Subsampled_Income_summary_plot, 
                  aes(y=IncomeGroup, 
                      x=perc,
                      # height=stat(density)))+
                      fill=IncomeGroup)) +
    
    # geom_density_ridges(stat = "binline", 
    #                     bins = 40, 
    #                     scale = 0.95, 
    #                     draw_baseline = TRUE)+
    geom_density_ridges_gradient(jittered_points = FALSE,
                                 quantile_lines = TRUE,
                                 quantiles = 2,
                                 scale=2.9,
                                 color='black') +
    ylab("National Income Group") + 
    xlab("Percentage from Paywalled Journals from each National Income Category")+
    facet_grid(cols = vars(author), 
               rows=vars(Dataset),
               labeller=labeller(author = author.labels))+
    scale_x_continuous(expand=c(0.1,0.02))+
    scale_x_continuous(expand = c(0,0),limits = c(-4,100))+
    
  scale_fill_brewer(palette = "Paired")+
    # brewer.pal(4, "paired") gets the hex codes from palette
    geom_segment(data = OA_percs,
                 aes(x = perc, xend = perc,
                     y = as.numeric(IncomeGroup),
                     yend = as.numeric(IncomeGroup) + .6), 
                 color="#E41A1C")+
    ## OA POINTS LINES AND LABELS
    geom_text(data=OA_percs,aes(x=perc,
                                y=as.numeric(IncomeGroup)+.7,
                                label = "OA"),color="#E41A1C", size=2)+
    scale_y_discrete(expand = c(0.01, 0)) +
    theme_ridges(grid = TRUE, center = TRUE)+
    
    # coord_flip()+
    theme_ridges(grid=TRUE)
  
  Fig1
  
  Fig1<-Fig1+
    theme_classic()+
    theme(
      axis.text.x = element_text(size=10,angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size=10),
      axis.title.x=element_text(colour="black", size = 10, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 10, vjust=2),
      strip.text.x = element_text(size = 10),
      strip.background = element_rect(colour = "black", fill = "white"),
      strip.text.y = element_text(size = 10),
      panel.spacing.x =unit(2, "lines"), 
      panel.spacing.y=unit(2,"lines"),
      legend.position = "none",
      plot.margin =unit(c(1,3,1,1.5), "lines")   #plot margin - top, right, bottom, left
    )    
  Fig1   
  ####################################3
  return(Fig1)
  
}