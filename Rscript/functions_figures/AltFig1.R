AltFig1<-function(DataSet,AuPosition) {
  # DataSet<-bootstrap_results
  # AuPosition<-"author_first"
  Subsampled_Countries<-bootstrap_results_countries
  library(ggplot2)
  library(RColorBrewer)
  library(ggExtra)
  library(ggridges)  
  # 
  # CountryInfo<-AllData %>% 
  #   select(Country,Region,IncomeGroup,Code) %>% 
  #   group_by(Country) %>% 
  #   slice(1)
  # head(CountryInfo,10)
  
  # Subsampled_Countries<-read.csv( 'output/SubsampledPW.results_Countries_FIRST_AUTHOR.csv')
  
  # Subsampled_Countries<-Subsampled_Countries %>% 
  #   inner_join(CountryInfo,Subsampled_Countries,by="Country") 
  # head(Subsampled_Countries,10)
  Subsampled_Countries$IncomeGroup<-as.factor(Subsampled_Countries$IncomeGroup) 
  Subsampled_Countries$Region<-as.factor(Subsampled_Countries$Region) 
  Subsampled_Countries$Code<-as.factor(Subsampled_Countries$Code) 
  Subsampled_Countries$Country<-as.factor(Subsampled_Countries$Country) 
  Subsampled_Countries$IncomeGroup <- ordered(Subsampled_Countries$IncomeGroup, 
                                 levels = c("High", "Upper middle","Lower middle","Low"))
  
  Subsampled_Income_summary<-Subsampled_Countries %>% 
    group_by(author,Dataset,replicate,IncomeGroup) %>% 
    summarize(n=n()) %>% 
    mutate(perc=n/sum(n)*100) 
  
  # Subsampled_Income_summary<-SubsampledPW.results_First[1]
  
  PW_medians<-Subsampled_Income_summary %>% 
    group_by(author,Dataset,replicate,IncomeGroup) %>% 
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
  
  AllPubs<-bind_rows(one_author_pubs_ALL,
                     coauthor_pubs_ALL,
                     one_author_pubsNOCHNUSA,
                     coauthor_pubsNOCHNUSA)
  
  OAData<-AllPubs %>% filter(JrnlType=="OA")
  
  OA_percs<-OAData %>% 
    group_by(author,Dataset,IncomeGroup) %>% 
    summarize(n=n()) %>% 
    mutate(perc=n/sum(n)*100)
  
  
  # 
  # OA_percs <- data.frame(IncomeGroup = c("High", 
  #                                        "Upper middle", 
  #                                        "Lower middle",
  #                                        "Low"),
  #                        OAPerc = c(80.4, 10.6, 7.98,0.939),
  #                        OAlabel=c("OA: 80.4%","OA: 10.6%","OA: 8%","OA: 1%"),
  #                        PWPerc=c(54.4,27.5,14.9,3.12),
  #                        PWlabel=c("PW: 54.4%","PW: 27.5%","PW: 14.9%","PW: 3.12%"))
  
  OA_percs$IncomeGroup <- 
    ordered(OA_percs$IncomeGroup, levels = c("High",
                                             "Upper middle",
                                             "Lower middle",
                                             "Low"))
  # 
  # geom_density_ridges_gradient(jittered_points = FALSE, quantile_lines = 
  #                                FALSE, quantiles = 2, scale=0.9, color='white') +
  #   
  #  geom_density_ridges(stat = "binline", 
  #  bins = 100, 
  #  scale = 0.95, 
  #  draw_baseline = TRUE) +
  author.labels <- c(author_first = "First Authors", author_last = "Last Authors", solo= "Single Authors")
  fig1alt<-ggplot(Subsampled_Income_summary, 
                  aes(y=IncomeGroup, 
                      x=perc,
                      # height=stat(density)))+
                      fill=IncomeGroup)) +
    geom_density_ridges_gradient(jittered_points = FALSE, 
                                 quantile_lines = TRUE, 
                                 quantiles = 2, 
                                 scale=0.9, 
                                 color='black') +
    ylab("National Income Group") + 
    xlab("Percentage of Authors")+
    facet_grid(cols = vars(Dataset), rows=vars(author),
               labeller=labeller(author = author.labels),
               scales="free_y")+
    
    
    
    # geom_segment(data = OA_percs,
    #              aes(x = OAPerc, xend = OAPerc,
    #                  y = as.numeric(IncomeGroup),
    #                  yend = as.numeric(IncomeGroup) + .6), color="blue")+
    #ylabeltext<-"National Income Group"
    
    ## OA POINTS LINES AND LABELS
    # geom_segment(data = OA_percs,
    #              aes(x = OAPerc, xend = OAPerc,
    #                  y = as.numeric(IncomeGroup),
    #                  yend = as.numeric(IncomeGroup) + .5))+
    # geom_point(data = OA_percs,
    #              aes(x = OAPerc,
    #                  y = as.numeric(IncomeGroup) + .5))+
    # geom_text(data=OA_percs,aes(x=OAPerc+2.5,
    #                             y=as.numeric(IncomeGroup)+.6,
    #                             label = OAlabel),color="blue")+
    scale_y_discrete(expand = c(0.01, 0)) +
    theme_ridges(grid = FALSE, center = TRUE)+
    ## PW POINTS LINES AND LABELS
    # geom_segment(data = OA_percs, 
    #              aes(x = PWPerc, xend = PWPerc, 
    #                  y = as.numeric(IncomeGroup),
    #                  yend = as.numeric(IncomeGroup) + .5))+
    # geom_point(data = OA_percs, 
    #            aes(x = PWPerc, 
    #                y = as.numeric(IncomeGroup) + .5))+
  # geom_text(data=OA_percs,
  #           aes(x=PWPerc,
  #               y=as.numeric(IncomeGroup)+.65,
  #               label = PWlabel))+
  scale_x_continuous(expand = c(0,0),limits = c(0,100))+
    scale_fill_brewer(palette = "Paired")+
    coord_flip()+
    theme_ridges()
  fig1alt
  
  fig1alt<-fig1alt+
    theme_classic()+
    theme(
      axis.text.x = element_text(size=18,angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size=18),
      axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 24, vjust=2),
      strip.text.x = element_text(size = 18),
      panel.spacing.x =unit(2.5, "lines") , panel.spacing.y=unit(1,"lines"),
      legend.position = "none",
      plot.margin =unit(c(1,3,1,1.5), "lines")   #plot margin - top, right, bottom, left
    )    
  fig1alt   
  plot1<-fig1alt
  # 
  # png(file="./tables_figs/plot1A_alt2.png",width=1000, height=700)
  # fig1alt   
  # dev.off()
  
  
  # # # # # # # # # # # # # # # # # # # # # 
  
  
  # BOX PLOT
  
  plot1alt<-ggplot(Subsampled_Income_summary, aes(y=perc,x=IncomeGroup, fill=IncomeGroup)) + 
    geom_boxplot(position = 'identity') +
    scale_fill_brewer(palette = "Paired")+
    scale_y_continuous(expand = c(0,0),limits = c(0,100))+
    # geom_segment(data = OA_percs,
    #              aes(y = OAPerc, yend = OAPerc,
    #                  x = as.numeric(IncomeGroup)-0.4,
    #                  xend = as.numeric(IncomeGroup) +0.4), color="blue")+
    # geom_text(data=OA_percs,
    #           aes(y=OAPerc,
    #               x=as.numeric(IncomeGroup)+.6,
    #               label = OAlabel))+
    geom_point(data = OA_percs,
               aes(y = OAPerc,
                   x = as.numeric(IncomeGroup)),
               color="blue",
               shape=8)+
    # xlab(xlabeltext) + 
    ylab("%")
  plot1alt
  
  png(file="./tables_figs/plot1A_alt.png",width=1000, height=700)
  plot1alt
  dev.off()
  
  ###############################################
  # VIOLIN PLOT
  
  plot1alt<-ggplot(Subsampled_Income_summary, aes(y=perc,x=IncomeGroup, fill=IncomeGroup)) + 
    geom_violin(position = 'identity') +
    scale_fill_brewer(palette = "Paired")+
    scale_y_continuous(expand = c(0,0),limits = c(0,100))+
    # geom_segment(data = OA_percs,
    #              aes(y = OAPerc, yend = OAPerc,
    #                  x = as.numeric(IncomeGroup)-0.4,
    #                  xend = as.numeric(IncomeGroup) +0.4), color="blue")+
    # geom_text(data=OA_percs,
    #           aes(y=OAPerc,
    #               x=as.numeric(IncomeGroup)+.6,
    #               label = OAlabel))+
    geom_point(data = OA_percs,
               aes(y = OAPerc,
                   x = as.numeric(IncomeGroup)),
               color="blue",
               shape=8)+
    # xlab(xlabeltext) + 
    ylab("%")
  plot1alt
  #########################################
  # HISTOGRAMS ALL 1 ROW
  
  
  # 
  Subsampled_Income_summary$IncomeGroup <- ordered(Subsampled_Income_summary$IncomeGroup, levels = c("High", "Upper middle","Lower middle","Low"))
  plot1alt<-ggplot(Subsampled_Income_summary, aes(y=perc,fill=IncomeGroup)) + 
    geom_histogram(bins=100,color="black", alpha=0.8, position = 'identity') +
    scale_fill_brewer(palette = "Paired")+
    scale_y_continuous(expand = c(0,0),limits = c(0,100))+
    # xlab(xlabeltext) + 
    ylab("%")
  plot1alt
  
  ####################################3
  
#   
#   bootstrap_results$IncomeGroup <- ordered(bootstrap_results$IncomeGroup, levels = c("High", "Upper middle","Lower middle","Low"))
#   # levels(bootstrap_results$IncomeGroup)
#   # bootstrap_results<-AllData
#   # AuPosition<-"author_first"
#   if ((AuPosition=="author_first")==TRUE) {
#     first_author_income_cats<-as.data.frame(bootstrap_results) %>% 
#       filter(AuthorNum==1) %>% 
#       group_by(JrnlType,IncomeGroup) %>% 
#       tally() %>% 
#       mutate(percentage=n/sum(n)*100)
#     
#     xlabeltext="First Author National Income Category"
#     title_text=paste("Fig. 1a: Percentage of each article type with first", 
#                      "authors in different national income categories.", sep= " ")
#     
#   } else if ((AuPosition=="author_last")==TRUE) {
#     first_author_income_cats<-as.data.frame(bootstrap_results) %>% 
#       group_by(DOI) %>% 
#       filter(AuthorNum == max(AuthorNum)) %>% 
#       group_by(JrnlType,IncomeGroup) %>% 
#       tally() %>% 
#       mutate(percentage=n/sum(n)*100)
#     
#     xlabeltext="Last Author National Income Category"
#     title_text=paste("Fig. 1b: Percentage of each article type with last",
#                      "authors in different national income categories.",sep= " ")
#     
#   } else {
#     stop("Please enter 'author_first' or 'author_last'")
#     
#   }
# plot1alt2<-ggplot(Subsampled_Income_summary, aes(y=perc,x=IncomeGroup)) + 
# geom_boxplot() +
# scale_y_continuous(expand = c(0,0),limits = c(0,100))
# ggMarginal(plot1alt, type = "boxplot", fill="transparent")
#     # scale_x_discrete(limits = bar_order)+
#     # facet_grid(cols = vars(JrnlType),labeller=labeller(JrnlType = labels))+
#     # ggtitle(title_text)
# plot1alt<-plot1alt+
#     theme_classic()+
#     theme(
#       axis.text.x = element_text(size=18,angle = 45, vjust = 1, hjust = 1),
#       axis.text.y = element_text(size=18),
#       axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
#       axis.title.y=element_text(colour="black", size = 24, vjust=2),
#       strip.text.x = element_text(size = 18),
#       panel.spacing.x =unit(2.5, "lines") , panel.spacing.y=unit(1,"lines"),
#       plot.margin =unit(c(1,1,1,1.5), "lines")   #plot margin - top, right, bottom, left
#       
#       
#     )
#   plot1alt

  return(plot1)
  
}
# 
# 
# Subsampled_Income_summary$IncomeGroup <- 
#   ordered(Subsampled_Income_summary$IncomeGroup, levels = c("High", 
#                                                             "Upper middle",
#                                                             "Lower middle",
#                                                             "Low"))
# 
# fig1alt<-ggplot(Subsampled_Income_summary, 
#                 aes(y=IncomeGroup, 
#                     x=perc,
#                     height=stat(density),
#                     fill=IncomeGroup)) +
#   geom_density_ridges(stat="density") +
#   scale_fill_brewer(palette = "Paired")+
#   theme_ridges() +
#   coord_flip()
# # fig1alt
# 
# ####################################
# fig1alt<-ggplot(Subsampled_Income_summary, 
#                 aes(y=IncomeGroup, 
#                     x=perc,
#                     height=stat(density),
#                     fill=IncomeGroup)) +
#   stat_density_ridges(quantile_lines = TRUE, 
#                                           quantiles = 2, 
#                                           alpha = 0.1)+
#   scale_fill_brewer(palette = "Paired")+
#   theme_ridges() +
#   coord_flip()
# fig1alt

####################################










    