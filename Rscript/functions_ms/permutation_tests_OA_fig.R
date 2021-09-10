permutation_tests_OA_fig<-function(All_boots_wide, AllData){

  source("./Rscript/functions_ms/DivRichCalc.R")
  
  OA_paper_pool<-AllData %>% 
    filter(ArticleType=="OA") %>% 
    group_by(refID) %>% 
    slice(1)
  
  
  OA_paper_pool_NOCHNUSA<-OA_paper_pool %>% 
    filter(Code!="CHN") %>% 
    filter(Code!="USA")
  
  
  
  
  All_boots_long_fig<-All_boots_wide %>%
    select(rep,countries,DivDiff,RichDiff,EvenDiff) %>% 
    pivot_longer(DivDiff:EvenDiff,names_to= "metric")
  
  means_vars<-All_boots_long_fig %>% group_by(countries,metric) %>% 
    summarize(mean(value),var(value))
  

  #############
  # Calculate the 905%CIs

  # Alpha for percentile confidence intervals see
  # https://cran.r-project.org/web/packages/broom/vignettes/bootstrapping.html
  alpha <- .05
  rich_div_stats<-All_boots_long_fig %>% 
    group_by(countries,metric) %>% 
    summarize(avg_div=mean(value),
              sd_div=sd(value),
              count = n(),
              DiffDiv_CIlow=quantile(value, alpha/2),
              DiffDiv_CIhigh=quantile(value, 1-alpha/2))          
              
  
# OBSERVED RESULTS: ALL COUNTRIES
  subscription_stats<-DivRichCalc(OA_paper_pool,"author_first","PW","OA")
  # mirror_articles OBSERVED RESULTS
  mirror_stats<-DivRichCalc(OA_paper_pool,"author_first","OA","OA")
  # Difference between mirror and OAinPW - Diversity
  div_diff_obs_All<-as.numeric(mirror_stats[2])-as.numeric(subscription_stats[2])
  # Difference between mirror and OAinPW - Richness
  rich_diff_obs_ALL<-as.numeric(mirror_stats[1])-as.numeric(subscription_stats[1])
  # Difference between mirror and OAinPW - Evenness
  even_diff_obs_ALL<-as.numeric(mirror_stats[5])-as.numeric(subscription_stats[5])
  
  mirror_R<-as.numeric(mirror_stats[1])
  mirror_D<-round(as.numeric(mirror_stats[2]),3)
  subscription_R<-as.numeric(subscription_stats[1])
  subscription_D<-round(as.numeric(subscription_stats[2]),3)
  mirror_E<-round(as.numeric(mirror_stats[5]),3)  
  subscription_E<-round(as.numeric(subscription_stats[5]),3)
  
  
  
  
  
  
# OBSERVED RESULTS: NO CHN USA
mirror_noCHNUSA_stats<-DivRichCalc(OA_paper_pool_NOCHNUSA,"author_first","OA","OA")
subscription_noCHNUSA_stats<-DivRichCalc(OA_paper_pool_NOCHNUSA,"author_first","PW","OA")
mirror_no_R<-as.numeric(mirror_noCHNUSA_stats[1])
mirror_no_D<-round(as.numeric(mirror_noCHNUSA_stats[2]),3)  
mirror_no_E<-as.numeric(mirror_noCHNUSA_stats[5],3)
subscription_no_R<-as.numeric(subscription_noCHNUSA_stats[1])
subscription_no_D<-round(as.numeric(subscription_noCHNUSA_stats[2]),3)
subscription_no_E<-round(as.numeric(subscription_noCHNUSA_stats[5]),3)
# Bind all the observed values of R and D up in a table


mirror<-c(mirror_D,
     subscription_D,
     mirror_D-subscription_D)
names(mirror)<-c("Mirror","Subscription","ObsDiff")
mirror_no<-c(mirror_no_D,
             subscription_no_D,
        mirror_no_D-subscription_no_D)
names(mirror_no)<-c("Mirror","Subscription","ObsDiff")

subscription<-c(mirror_R,subscription_R,mirror_R-subscription_R)
names(subscription)<-c("Mirror","Subscription","ObsDiff")
subscription_no<-c(mirror_no_R,subscription_no_R,mirror_no_R-subscription_no_R)
names(subscription_no)<-c("Mirror","Subscription","ObsDiff")

evenness<-c(mirror_E,subscription_E,mirror_E-subscription_E)
names(evenness)<-c("Mirror","Subscription","ObsDiff")
evenness_no<-c(mirror_no_E,subscription_no_E,mirror_no_E-subscription_no_E)
names(evenness_no)<-c("Mirror","Subscription","ObsDiff")



obs_values<-bind_rows(mirror,subscription,evenness,mirror_no,subscription_no,evenness_no)

obs_values$metric<-rep(c("Diversity","Richness","Evenness"),2)
obs_values$countries<-c("all","all","all","no_chnusa","no_chnusa","no_chnusa")
obs_values$ObsDiff<-round(obs_values$ObsDiff,2)
obs_values$Mirror<-round(obs_values$Mirror,2)
obs_values$Subscription<-round(obs_values$Subscription,2)
obs_values

# Critical Values

# ALL COUNTRIES
# Diversity Diff
MAD_crit_all<-obs_values %>% 
  filter(metric=="Diversity") %>% 
  filter(countries=="all") %>% 
  select(ObsDiff)
MAD_crit_all

MAD_all<-All_boots_wide %>% 
  filter(countries=="all") %>% 
  select(DivDiff) %>% 
  arrange(DivDiff)

p_div_all<-sum(((MAD_crit_all$ObsDiff>MAD_all$DivDiff)==TRUE)/nrow(MAD_all)*100)
p_div_all

# Richness Diff
MAR_crit_all<-obs_values %>% 
  filter(metric=="Richness") %>% 
  filter(countries=="all") %>% 
  select(ObsDiff)
MAR_crit_all

MAR_all<-All_boots_wide %>% 
  filter(countries=="all") %>% 
  select(RichDiff) %>% 
  arrange(RichDiff)
MAR_all

p_rich_all<-sum(((MAR_crit_all$ObsDiff>MAR_all$RichDiff)==TRUE)/nrow(MAR_all)*100)
p_rich_all

# Eveness  Diff
MAE_crit_all<-obs_values %>% 
  filter(metric=="Evenness") %>% 
  filter(countries=="all") %>% 
  select(ObsDiff)
MAE_crit_all

MAE_all<-All_boots_wide %>% 
  filter(countries=="all") %>% 
  select(EvenDiff) %>% 
  arrange(EvenDiff)
MAE_all

p_even_all<-sum(((MAE_crit_all$ObsDiff>MAE_all$EvenDiff)==TRUE)/nrow(MAE_all)*100)
p_even_all

# NO CHN USA
# Diversity Diff
MAD_crit_no<-obs_values %>% 
  filter(metric=="Diversity") %>% 
  filter(countries=="no_chnusa") %>% 
  select(ObsDiff)
MAD_crit_no

MAD_no<-All_boots_wide %>% 
  filter(countries=="no_chnusa") %>% 
  select(DivDiff) %>% 
  arrange(DivDiff) 

p_div_no<-sum(((MAD_crit_no$ObsDiff>MAD_no$DivDiff)==TRUE)/nrow(MAD_no)*100)


# Richness Diff
MAR_crit_no<-obs_values %>% 
  filter(metric=="Richness") %>% 
  filter(countries=="no_chnusa") %>% 
  select(ObsDiff)
MAR_crit_no

MAR_no<-All_boots_wide %>% 
  filter(countries=="no_chnusa") %>% 
  select(RichDiff) %>% 
  arrange(RichDiff)

p_rich_no<-sum(((MAR_crit_no$ObsDiff>MAR_no$RichDiff)==TRUE)/nrow(MAR_no)*100)



# Eveness  Diff
MAE_crit_no<-obs_values %>% 
  filter(metric=="Evenness") %>% 
  filter(countries=="no_chnusa") %>% 
  select(ObsDiff)
MAE_crit_no

MAE_no<-All_boots_wide %>% 
  filter(countries=="no_chnusa") %>% 
  select(EvenDiff) %>% 
  arrange(EvenDiff)
MAE_no

p_even_no<-sum(((MAE_crit_no$ObsDiff>MAE_no$EvenDiff)==TRUE)/nrow(MAE_no)*100)
p_even_no




pvals<-data.frame(pval=c(p_div_all,p_rich_all,p_even_all,p_div_no,p_rich_no,p_even_no))
pvals$metric<-rep(c("Diversity","Richness","Evenness"),2)
pvals$countries<-c("all","all","all","no_chnusa","no_chnusa","no_chnusa")
obs_values<-left_join(obs_values,pvals)


figure_values<-obs_values
figure_values$countries<-c("all countries","all countries","all countries",
                           "China and USA excluded","China and USA excluded","China and USA excluded")
# figure_values$countries<-gsub("all", "all countries",figure_values$countries)
# figure_values$countries<-gsub("no_chnusa", "China and USA excluded",figure_values$countries)
figure_values
as.factor(All_boots_long_fig$countries)

All_boots_long_fig$metric<-gsub("DivDiff","Diversity",All_boots_long_fig$metric)
All_boots_long_fig$metric<-gsub("EvenDiff","Evenness",All_boots_long_fig$metric)
All_boots_long_fig$metric<-gsub("RichDiff","Richness",All_boots_long_fig$metric)
All_boots_long_fig$countries<-gsub("no_chnusa","China and USA excluded",All_boots_long_fig$countries)
All_boots_long_fig$countries<-gsub("all","all countries",All_boots_long_fig$countries)
# All_boots_long_fig %>% group_by(countries,metric) %>% summarize(n())


p1<-
    ggplot(All_boots_long_fig, aes(x=value),fill=metric) +
    geom_histogram(bins=30, color="black",
                   fill="darkgray",
                   size=0.1,alpha=0.6, position = 'identity') +
    facet_grid(rows= vars(countries), cols = vars(metric),
               # labeller=labeller(author = author.labels),
               scales="free") +
    geom_hline((aes(yintercept=-Inf)), color="black") +
    geom_vline((aes(xintercept=-Inf)) , color="black") +
    guides(fill=guide_legend("Metric"))+
    # scale_x_continuous(limits = c(-40, 20),breaks = seq(-40,20, by=10),expand = c(0.01,0))+
    scale_y_continuous(limits = c(-10, 1200),breaks = seq(0, 1200, by=200),expand = c(0.01,0))+
    labs(x = "Difference in Metrics between 'Mirror' - 'Subscription OA' sample", 
         y = "Frequency") +
  #   
    geom_segment(data = subset(filter(figure_values, metric == "Diversity" & countries=="all countries")),
                 aes(x = ObsDiff , y = 0, xend = ObsDiff, yend = 1000), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values, metric == "Diversity", countries=="all countries")),
              aes(x=ObsDiff+1.2, y=1040, label=(paste("Obs",as.character(ObsDiff),sep=" == "))),
              parse=TRUE,size=2)+
    geom_segment(data = subset(filter(figure_values, metric == "Richness", countries=="all countries")),
                 aes(x = ObsDiff , y = 0, xend = ObsDiff, yend = 1000),
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values, metric == "Richness", countries=="all countries")),
              aes(x=ObsDiff, y=1040, label=(paste("Obs",as.character(ObsDiff),sep=" == "))),
              parse=TRUE,size=2)+
  geom_segment(data = subset(filter(figure_values, metric == "Evenness", countries=="all countries")),
               aes(x = ObsDiff , y = 0, xend = ObsDiff, yend = 1000),
               colour = "red",linetype="solid")+
  geom_text(data = subset(filter(figure_values, metric == "Evenness", countries=="all countries")),
            aes(x=ObsDiff, y=1040, label=(paste("Obs",as.character(ObsDiff),sep=" == "))),
            parse=TRUE,size=2)+
  
    # WITHOUT CHN USA
    
    geom_segment(data = subset(filter(figure_values, metric == "Diversity", countries=="China and USA excluded")),
                 aes(x = ObsDiff , y = 0, xend = ObsDiff, yend = 1000), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values, metric == "Diversity", countries=="China and USA excluded")),
              aes(x=ObsDiff, y=1040, label=(paste("Obs",as.character(ObsDiff),sep=" == "))),
              parse=TRUE,size=2)+
    geom_segment(data = subset(filter(figure_values, metric == "Richness", countries=="China and USA excluded")),
                 aes(x = ObsDiff , y = 0, xend = ObsDiff, yend = 1000),
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values, metric == "Richness", countries=="China and USA excluded")),
              aes(x=ObsDiff, y=1040, label=(paste("Obs",as.character(ObsDiff),sep=" == "))),
              parse=TRUE,size=2)+
  geom_segment(data = subset(filter(figure_values, metric == "Evenness", countries=="China and USA excluded")),
               aes(x = ObsDiff , y = 0, xend = ObsDiff, yend = 1000),
               colour = "red",linetype="solid")+
  geom_text(data = subset(filter(figure_values, metric == "Evenness", countries=="China and USA excluded")),
            aes(x=ObsDiff, y=1040, label=(paste("Obs",as.character(ObsDiff),sep=" == "))),
            parse=TRUE,size=2)+
    
    
    coord_cartesian(clip="off")
  

  p1<-p1+
    theme_classic()+ 
    theme(
      axis.text.x = element_text(size=8),
      axis.text.y = element_text(size=8),
      axis.title.x=element_text(colour="black", size = 10, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 10, hjust=0.5,),
      strip.text.x = element_text(size = 10,margin = margin(0,0,3,0, "lines")),
      strip.text.y = element_text(size = 10, angle=0),
      strip.text = element_blank(),
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
  
  facet_labels<-c("A","B","C","D","E","F")
  p1<-tag_facet(p1,open="", close="", tag_pool=facet_labels,vjust=-1,hjust=-0.1)
  p1
  
  ##########
  # Stats
  #########
  
  
  
  
  ##################################################################################
  # DIVERISTY
  ############################################################################
  
  figure_values<-ungroup(figure_values)
  perm_results<-list(p1,figure_values)
  return(perm_results)
}