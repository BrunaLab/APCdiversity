# This does permutation tests to see to determine if the difference
# beween the observed diversity of Mirror Journal OA articles and 
# OA articles in subscription journals is sognificanl;ty different 
# from what would be expected if the articles in these two groups were 
# assigned at random from the pool of all OA articles.  

# Note that there are enough to do all articles together and the coauthored 
# articles, but not the solo authored articles. Here we will do all articles 
# pooled
library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(egg)

AllData$Journal<-as.factor(AllData$Journal)

# ALL THE OA PAPERS (Mirror + PW)
OApapers<-AllData %>% 
  filter(ArticleType=="OA") %>% 
  # filter(author=="coauthored") %>%
  # filter(author=="solo") %>%
  filter(pair_key!=5) %>% 
  filter(pair_key!=6)
OApapers$pair_key<-droplevels(OApapers$pair_key)
levels(OApapers$pair_key)

# How many by Pair_key
OApapers_byPK<-OApapers %>%
  group_by(refID) %>% 
  slice(1) %>% 
  group_by(pair_key) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  select(pair_key,n)
n_allOApapers<-OApapers_byPK$n
sum(n_allOApapers)

# All the OA articles in subscription journals
OAinPW_articles <- OApapers %>%
  filter(JrnlType == "PW") %>% 
  filter(ArticleType=="OA") %>% 
  group_by(refID) %>% 
  slice(1) 

# All the OA articles in mirror journals
mirror_articles <- OApapers %>%
  filter(JrnlType == "OA") %>% 
  filter(ArticleType=="OA") %>% 
  group_by(refID) %>% 
  slice(1) 

# How many articles in each of the subscription journals
OAinPW_count <- OAinPW_articles %>%
  group_by(pair_key) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  select(pair_key,n)
n_OAinPW<-OAinPW_count$n
sum(n_OAinPW)

# How many articles in each of the mirror journals
mirror_count <- mirror_articles %>%
  group_by(pair_key) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  select(pair_key,n)
n_mirror<-mirror_count$n
sum(n_mirror)

### WITHOUT CHN USA
OA_papers_NOCHNUSA<-OApapers %>% 
  filter(Code!="CHN") %>% 
  filter(Code!="USA")

n_OAinPW_noCHNUSA <- OA_paper_pool_NOCHNUSA %>%
  group_by(pair_key) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  select(pair_key,n)
n_OAinPW_noCHNUSA<-n_OAinPW_noCHNUSA$n
# n_OAinPW_noCHNUSA<-n_OAinPW_noCHNUSA$n
sum(n_OAinPW_noCHNUSA)

mirror_noCHNUSA <- OA_papers_NOCHNUSA %>%
  filter(JrnlType=="OA")

n_mirror_noCHNUSA <- mirror_noCHNUSA %>%
  group_by(pair_key) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  select(pair_key,n)
n_mirror_noCHNUSA<-n_mirror_noCHNUSA$n
sum(n_mirror_noCHNUSA)
# n_OAinPW_noCHNUSA>n_mirror_noCHNUSA
# n_OAinPW_noCHNUSA-n_mirror_noCHNUSA









# Are there enough articles to draw from the pool?
n_allOApapers>n_OAinPW
n_allOApapers>n_mirror

# The pool of OA papers from which we will be drawing
  OA_paper_pool<- OApapers %>%
    group_by(refID) %>% 
    slice(1) 
  
  # It has been suggested that permutation tests use 2*n replicates
  nrow(OA_paper_pool)*2
  ############################################
  # now sample and iterate!
  # THIS IS FOR MATCHED SUBSAMPLING - FIRST AUTHOR
  # 1<-Richness  2<-InvSimpsons 3<-Countries
  ############################################
  
  
  OA_paper_pool_solo<- OApapers %>%
    group_by(refID) %>% 
    slice(1) %>% 
    filter(author=="solo")
 
 n_solo_mirror<- OA_paper_pool_solo %>%
   filter(JrnlType=="OA") %>% 
    group_by(pair_key) %>% 
    summarize(n=n()) %>% 
    ungroup() %>% 
    select(pair_key,n)
 pair_keys_OAsolo<-as_tibble(levels(as.factor(OA_paper_pool_solo$pair_key))) %>% dplyr::rename("pair_key"="value")
 n_solo_mirror<-full_join(n_solo_mirror,pair_keys_OAsolo) %>% replace_na(list(n=0))
 
  n_solo_mirror<-n_solo_mirror$n
  sum(n_solo_mirror)
  n_solo_mirror<-n_solo_mirror
  n_solo_mirror<-unlist(n_solo_mirror)
  n_solo_mirror<-as.data.frame(n_solo_mirror)
  n_solo_mirror<-n_solo_mirror$n
  sum(n_solo_mirror)
  sum(n_solo_mirror)*2
  
  
  OA_paper_pool_first<- OApapers %>%
    group_by(refID) %>% 
    slice(1) %>% 
    filter(author=="coauthored")
  n_first_mirror <- OA_paper_pool_first %>%
    group_by(pair_key) %>% 
    summarize(n=n()) %>% 
    ungroup() %>% 
    select(pair_key,n)
  
  pair_keys_OAfirst<-as_tibble(levels(as.factor(OA_paper_pool_first$pair_key))) %>% dplyr::rename("pair_key"="value")
  n_first_mirror<-full_join(n_first_mirror,pair_keys_OAfirst) %>% replace_na(list(n=0))
  
  n_first_mirror<-n_first_mirror$n
  sum(n_first_mirror)
  n_first_mirror<-n_first_mirror
  n_first_mirror<-unlist(n_first_mirror)
  n_first_mirror<-as.data.frame(n_first_mirror)
  n_first_mirror<-n_first_mirror$n
  sum(n_first_mirror)*2
  sum(n_first_mirror)
  
  source("./Rscript/functions/DivRichCalc.R")
  # OA_paper_pool_first
  # OA_paper_pool_solo
 n<-n_first_mirror
  # n<-n_solo_mirror
  # sum(n)
  OA_paper_pool<-as.data.frame(OA_paper_pool_first)
  # n<-n_OAinPW
  # This is the number in each of the mirror journals

  
  nboot <-500 #number of permutations
  InvSimp <-rep(NA, nboot)
  Richness<-rep(NA, nboot)
  Shannon<-rep(NA, nboot)
  Even<-rep(NA, nboot)
  n_papers<-rep(NA, nboot)
  replicate<-data.frame(replicate=as.numeric())
  Countries_mirror<-rep(NA, nboot)
  Countries_OAinPW<-rep(NA, nboot)
 
  # These are where you will store the results - one for the randomly selected in a way
  # that mimics the "mirror" populations and one for the randomly assigned to the 
  # subscription journal populations.
  permutations_mirror <- data.frame(Richness,InvSimp,Shannon,Even,n_papers)
  permutations_OAinPW <- data.frame(Richness,InvSimp,Shannon,Even,n_papers)
  # replicate<-data.frame(replicate)
  
  
  
  rm(InvSimp,Richness,Shannon,Even,n_papers)
  
  set.seed(1)
  
  for(i in 1:nboot){
    
    # BE SURE PERMUTE OA IS SET TO SAMPlE *WITHOUT* REPLACEMENT
    
    source("./Rscript/functions/permute_OA.R")
    
    # Sample at random without replacement from the pooled OA articles in a way
    # that mimics the number of articles in each mirror journal. 
    mirror_sample<-permute_OA(OA_paper_pool,n,"perm")
    mirror_sample<-as.data.frame(mirror_sample[1])
    
    # Now use the refIDs of the ones sample to figure out which ones 
    # *weren't* selected and assign these to the "subscription journal group 
    mirror_sample_refID<-mirror_sample$refID
    OAinPW_sample<-anti_join(OA_paper_pool,mirror_sample)
    
    AuPosition<-"author_first"
    ArticleType<-"OA"
    # JrnlType<-"OA"
    JrnlType<-"both"
    
    # do the stats for the "mirror" group
    results_mirror<-DivRichCalc(mirror_sample,AuPosition,JrnlType,ArticleType)
    permutations_mirror[i,1]<-(results_mirror)[1]
    permutations_mirror[i,2]<-(results_mirror)[2]
    permutations_mirror[i,3]<-(results_mirror)[4]
    permutations_mirror[i,4]<-(results_mirror)[5]
    permutations_mirror[i,5]<-nrow(mirror_sample)
    Countries_mirror[i]<-(results_mirror)[3]

    count<-data.frame(replicate=rep(i, each=permutations_mirror[i,1]))
    replicate<- bind_rows(replicate,count)
  
    # Do the stats for the "subscription" group
    results_OAinPW<-DivRichCalc(OAinPW_sample,AuPosition,JrnlType,ArticleType)
    permutations_OAinPW[i,1]<-(results_OAinPW)[1]
    permutations_OAinPW[i,2]<-(results_OAinPW)[2]
    permutations_OAinPW[i,3]<-(results_OAinPW)[4]
    permutations_OAinPW[i,4]<-(results_OAinPW)[5]
    permutations_OAinPW[i,5]<-nrow(OAinPW_sample)
    Countries_OAinPW[i]<-(OAinPW_sample)[3]
    
    count<-data.frame(replicate=rep(i, each=permutations_OAinPW[i,1]))
    replicate<- bind_rows(replicate,count)
    
  }
  
  # Label the resulting permutations
  permutations_mirror$ArticleCat<-"Mirror"
  permutations_mirror$countries<-"all"
  permutations_mirror$rep<-seq(1:nboot)
  # permutations_mirror$comparison<-"first of coauthored"
  # permutations_mirror$comparison<-"solo"
  
  permutations_OAinPW$ArticleCat<-"OAinPW"
  permutations_OAinPW$countries<-"all"
  permutations_OAinPW$rep<-seq(1:nboot)
  # permutations_OAinPW$comparison<-"first of coauthored"
  # permutations_OAinPW$comparison<-"solo"
  
  # Bind the results together and rename/cleanup
  # binding in long format
  All_boots_long<-bind_rows(permutations_mirror, permutations_OAinPW) 
  
  ##########################
  # THIS IS FOR NO CHN USA
  ##########################
  
  OA_paper_pool_NOCHNUSA<-OA_paper_pool %>% 
    filter(Code!="CHN") %>% 
    filter(Code!="USA")
  
  n_mirror_solo_noCHNUSA <- OA_paper_pool_NOCHNUSA %>%
    filter(JrnlType=="OA") %>% 
    filter(author=="solo") %>% 
    group_by(pair_key) %>% 
    summarize(n=n()) %>% 
    ungroup() %>% 
    select(pair_key,n)
  n_mirror_solo_noCHNUSA<-n_mirror_solo_noCHNUSA$n
  sum(n_mirror_solo_noCHNUSA)
  
  
  n2<-n_mirror_solo_noCHNUSA
  n2<-unlist(n2)
  n2<-as.data.frame(n2)
  n_solo<-n_solo$n
  OA_paper_pool_NOCHNUSA<-as.data.frame(OA_paper_pool_NOCHNUSA)
  
  # n<-n_OAinPW
  # This is the number in each of the mirror journals
  
  n_OAinPW_noCHNUSA<-unlist(n_OAinPW_noCHNUSA)
  n_OAinPW_noCHNUSA<-as.data.frame(n_OAinPW_noCHNUSA)
  
  # nboot <-6 #number of permutations
  InvSimp <-rep(NA, nboot)
  Richness<-rep(NA, nboot)
  Shannon<-rep(NA, nboot)
  Even<-rep(NA, nboot)
  n_papers<-rep(NA, nboot)
  replicate_no<-data.frame(replicate=as.numeric())
  Countries_no_mirror<-rep(NA, nboot)
  Countries_no_OAinPW<-rep(NA, nboot)
  
  # These are where you will store the results - one for the randomly selected in a way
  # that mimics the "mirror" populations and one for the randomly assigned to the 
  # subscription journal populations.
  permutations_mirror_no <- data.frame(Richness,InvSimp,Shannon,Even,n_papers)
  permutations_OAinPW_no <- data.frame(Richness,InvSimp,Shannon,Even,n_papers)
  # replicate<-data.frame(replicate)
  
  rm(InvSimp,Richness,Shannon,Even,n_papers)
  
  set.seed(1)
  
  for(i in 1:nboot){
    
    # BE SURE PERMUTE OA IS SET TO SAMPlE *WITHOUT* REPLACEMENT
    # OA_pool_noCHNUSA<-OA_paper_pool %>% filter(Code!="CHN") %>% filter(Code!="USA")
    source("./Rscript/functions/permute_OA.R")
    # Sample at random without replacement from the pooled OA articles in a way
    # that mimics the number of articles in each mirror journal. 
    mirror_no_sample<-permute_OA(OA_paper_pool_NOCHNUSA,n2,"perm")
    mirror_no_sample<-as.data.frame(mirror_no_sample[1])
    
    # Now use the refIDs of the ones sample to figure out which ones 
    # *weren't* selected and assign these to the "subscription journal group 
    mirror_no_sample_refID<-mirror_no_sample$refID
    OAinPW_no_sample<-anti_join(OA_paper_pool_NOCHNUSA,mirror_no_sample)
    
    AuPosition<-"author_first"
    ArticleType<-"OA"
    # JrnlType<-"OA"
    JrnlType<-"both"
    
    # do the stats for the "mirror" group
    results_mirror_no<-DivRichCalc(mirror_no_sample,AuPosition,JrnlType,ArticleType)
    permutations_mirror_no[i,1]<-(results_mirror_no)[1]
    permutations_mirror_no[i,2]<-(results_mirror_no)[2]
    permutations_mirror_no[i,3]<-(results_mirror_no)[4]
    permutations_mirror_no[i,4]<-(results_mirror_no)[5]
    permutations_mirror_no[i,5]<-nrow(mirror_no_sample)
    Countries_no_mirror[i]<-(results_mirror_no)[3]
    
    count_no<-data.frame(replicate_no=rep(i, each=permutations_mirror_no[i,1]))
    replicate_no<- bind_rows(replicate_no,count_no)
    
    # Do the stats for the "subscription" group
    results_OAinPW_no<-DivRichCalc(OAinPW_no_sample,AuPosition,JrnlType,ArticleType)
    permutations_OAinPW_no[i,1]<-(results_OAinPW_no)[1]
    permutations_OAinPW_no[i,2]<-(results_OAinPW_no)[2]
    permutations_OAinPW_no[i,3]<-(results_OAinPW_no)[4]
    permutations_OAinPW_no[i,4]<-(results_OAinPW_no)[5]
    permutations_OAinPW_no[i,5]<-nrow(OAinPW_no_sample)
    Countries_no_OAinPW[i]<-(results_OAinPW_no)[3]
    
    count_no<-data.frame(replicate_no=rep(i, each=permutations_OAinPW_no[i,1]))
    replicate_no<- bind_rows(replicate_no,count_no)
    
  }
  
  # Label the resulting permutations
  permutations_mirror_no$ArticleCat<-"Mirror"
  permutations_mirror_no$countries<-"no_chnusa"
  permutations_mirror_no$rep<-seq(1:nboot)
  # permutations_mirror$comparison<-"first of coauthored"
  # permutations_mirror$comparison<-"solo"
  
  permutations_OAinPW_no$ArticleCat<-"OAinPW"
  permutations_OAinPW_no$countries<-"no_chnusa"
  permutations_OAinPW_no$rep<-seq(1:nboot)
  # permutations_OAinPW$comparison<-"first of coauthored"
  # permutations_OAinPW$comparison<-"solo"
  
  # Bind the results together and rename/cleanup
  # binding in long format
  All_boots_long_no<-bind_rows(permutations_mirror_no, permutations_OAinPW_no) 
  All_boots_long<-bind_rows(All_boots_long,All_boots_long_no)
  
  
  # This flips it to wide and adds a new column: the diff between the values of 
  # diversity and richness for the permuted articles assigned to "mirror" 
  # and "subscription" journals. The mean and CIs of this will be compared to the 
  # observed values to determine if observed diff is sig. diff
  colnames(All_boots_long)
  colnames(All_boots_wide)
  # subscription and mirror journals. 
  All_boots_wide<-All_boots_long %>%  
    select(-n_papers) %>% 
    pivot_wider(names_from = ArticleCat, values_from = c(Richness,InvSimp,Shannon,Even)) %>% 
    mutate(DivDiff=InvSimp_Mirror-InvSimp_OAinPW) %>% 
    mutate(RichDiff=Richness_Mirror-Richness_OAinPW) 
  colnames(All_boots_wide)
  # foo<-All_boots_wide %>% filter(countries=="all")
  # foo<-All_boots_wide %>% filter(countries=="no_chnusa")
  # hist(foo$DivDiff)
  # hist(foo$RichDiff)
  
  DivDiff<-All_boots_wide %>%
    select(rep,countries,DivDiff) %>% 
    dplyr::rename("value"="DivDiff")
  DivDiff$metric<-"DivDiff"
  
  RichDiff<-All_boots_wide %>%
    select(rep,countries,RichDiff) %>% 
    dplyr::rename("value"="RichDiff")
  RichDiff$metric<-"RichDiff"
  
  All_boots_long_fig<-bind_rows(DivDiff,RichDiff)
  # foo<-All_boots_long_fig %>% filter(countries=="all") %>% filter(metric=="DivDiff")
  # foo<-All_boots_long_fig %>% filter(countries=="no_chnusa")%>% filter(metric=="DivDiff")
  # hist(foo$value)
  # hist(foo$RichDiff)
  
  All_boots_long_fig<-All_boots_wide %>%
    select(rep,countries,DivDiff,RichDiff) %>% 
    pivot_longer(DivDiff:RichDiff,names_to= "metric")
  #   separate(metric, c("metric", "cat"))
  # as.factor(All_boots_long$metric)
  
  # %>% 
  #   mutate(DivDiff_no_CHNUSA=InvSimp_no_chnusa-InvSimp_no_chnusa) %>% 
  #   mutate(RichDiff_no_CHNUSAl=Richness_no_chnusa-Richness_no_chnusa)
  # Quick visualization 
  
  # plot(All_boots_wide$InvSimp_Mirror,All_boots_wide$InvSimp_OAinPW)
  
  # viz_raw_permutations<-
  #   ggplot(All_boots_long, aes(x=value,fill=cat)) +
  #   geom_histogram(bins=40, color="black",
  #                  size=0.1,alpha=0.6, position = 'identity')+
  #   facet_grid(rows="countries")
  
  means_vars<-All_boots_long_fig %>% group_by(countries,metric) %>% 
    summarize(mean(value),var(value))
  
  
  # diff_Div<-All_boots_wide$InvSimp_Mirror-All_boots_wide$InvSimp_OAinPW
  # diff_Rich<-All_boots_wide$Richness_Mirror-All_boots_wide$Richness_OAinPW
  # hist(All_boots_wide$DivDiff_MO)
  # hist(All_boots_wide$RichDiff_MO)
  
  # OAinPW_articles OBSERVED RESULTS
  
  OAinPW_stats<-DivRichCalc(OAinPW_articles,"author_first","PW","OA")
  # mirror_articles OBSERVED RESULTS
  mirror_stats<-DivRichCalc(mirror_articles,"author_first","OA","OA")
  # Difference between the two
  div_diff_obs<-as.numeric(mirror_stats[2])-as.numeric(OAinPW_stats[2])
  
  # This tells how how many of differences in diversity permutations are 
  # above and below the observed one
  summary(div_diff_obs>sort(All_boots_wide$DivDiff))
  
  # Now do it for Richness
  rich_diff_obs<-as.numeric(mirror_stats[1])-as.numeric(OAinPW_stats[1])
  summary(rich_diff_obs>sort(All_boots_wide$RichDiff_MO))
  
  # Look at the plot of the permutation values. looks like what 
  # would be expected.
  plot(All_boots_wide$InvSimp_Mirror,All_boots_wide$InvSimp_OAinPW)
  diffs_rich<-as.data.frame(All_boots_wide$RichDiff-All_boots_wide$RichDiff)
  
  write_csv(All_boots_wide,"./output/MirrorvOAinPW_permutations.csv")
  
  
  
  #############
  # Calculate the 905%CIs

  # Alpha for percentile confidence intervals see
  # https://cran.r-project.org/web/packages/broom/vignettes/bootstrapping.html
  alpha <- .05
  BootInfo_Div<-All_boots_long_fig %>% 
    group_by(countries,metric) %>% 
    summarize(avg_div=mean(value),
              sd_div=sd(value),
              count = n(),
              DiffDiv_CIlow=quantile(value, alpha/2),
              DiffDiv_CIhigh=quantile(value, 1-alpha/2))          
              
# THIS SUMMARY TABLE IS NEEDED TO MAKE THE FIGURES

M_R<-as.numeric(mirror_stats[1])
M_D<-round(as.numeric(mirror_stats[2],3))  
PW_R<-as.numeric(OAinPW_stats[1])
PW_D<-round(as.numeric(OAinPW_stats[2],3))

MR_noCHNUSA_stats<-DivRichCalc(mirror_noCHNUSA,"author_first","OA","OA")
PW_noCHNUSA_stats<-DivRichCalc(OA_paper_pool_NOCHNUSA,"author_first","PW","OA")
M_no_R<-as.numeric(MR_noCHNUSA_stats[1])
M_no_D<-round(as.numeric(MR_noCHNUSA_stats[2],3))  
PW_no_R<-as.numeric(PW_noCHNUSA_stats[1])
PW_no_D<-round(as.numeric(PW_noCHNUSA_stats[2],3))


M<-c(M_D,
     PW_D,
     M_D-PW_D)
names(M)<-c("Mirror","Subscription","ObsDiff")
M_no<-c(M_no_D,
        PW_no_D,
        M_no_D-PW_no_D)
names(M_no)<-c("Mirror","Subscription","ObsDiff")

PW<-c(M_R,PW_R,M_R-PW_R)
names(PW)<-c("Mirror","Subscription","ObsDiff")
PW_no<-c(M_no_R,PW_no_R,M_no_R-PW_no_R)
names(PW_no)<-c("Mirror","Subscription","ObsDiff")
figure_values<-bind_rows(M,PW,M_no,PW_no)
figure_values$metric<-rep(c("Diversity","Richness"),2)
figure_values$countries<-c("all countries","all countries",
                           "China and USA excluded","China and USA excluded")

figure_values
as.factor(All_boots_long_fig$countries)

All_boots_long_fig$metric<-gsub("DivDiff","Diversity",All_boots_long_fig$metric)
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
    # scale_y_continuous(limits = c(-10, 4000),breaks = seq(0, 4000, by=500),expand = c(0.01,0))+
    labs(x = "Difference in Permutations ('Mirror' - 'Subscription OA')", 
         y = "Frequency") +
  #   
    geom_segment(data = subset(filter(figure_values, metric == "Diversity" & countries=="all countries")),
                 aes(x = ObsDiff , y = 0, xend = ObsDiff, yend = 1200), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values, metric == "Diversity", countries=="all countries")),
              aes(x=ObsDiff, y=1210, label=(paste("Mirror",as.character(ObsDiff),sep=" == "))),
              parse=TRUE,size=2)+
    geom_segment(data = subset(filter(figure_values, metric == "Richness", countries=="all countries")),
                 aes(x = ObsDiff , y = 0, xend = ObsDiff, yend = 1200),
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values, metric == "Richness", countries=="all countries")),
              aes(x=ObsDiff, y=1210, label=(paste("Mirror",as.character(ObsDiff),sep=" == "))),
              parse=TRUE,size=2)+
    
    # WITHOUT CHN USA
    
    geom_segment(data = subset(filter(figure_values, metric == "Diversity", countries=="China and USA excluded")),
                 aes(x = ObsDiff , y = 0, xend = ObsDiff, yend = 1200), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values, metric == "Diversity", countries=="China and USA excluded")),
              aes(x=ObsDiff, y=1210, label=(paste("Mirror",as.character(ObsDiff),sep=" == "))),
              parse=TRUE,size=2)+
    geom_segment(data = subset(filter(figure_values, metric == "Richness", countries=="China and USA excluded")),
                 aes(x = ObsDiff , y = 0, xend = ObsDiff, yend = 1200),
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values, metric == "Richness", countries=="China and USA excluded")),
              aes(x=ObsDiff, y=1210, label=(paste("Mirror",as.character(ObsDiff),sep=" == "))),
              parse=TRUE,size=2)+
    
    
    coord_cartesian(clip="off")
  
  #   coord_cartesian(clip="off")+
  # geom_segment(data = subset(filter(figure_values, Difference == "Diversity")),
  #              aes(x = ObsDiff , y = 0, xend = ObsDiff, yend = 1100), 
  #              colour = "red",linetype="solid")+
  #   geom_text(data = subset(filter(figure_values, Difference == "Diversity")),
  #             aes(x=ObsDiff, y=1150, label=(paste("Observed",as.character(ObsDiff),sep=" == "))), 
  #             parse=TRUE,size=2)+
  #   geom_segment(data = subset(filter(figure_values, Difference == "Richness")),
  #                aes(x = ObsDiff , y = 0, xend = ObsDiff, yend = 1100), 
  #                colour = "red",linetype="solid")+
  #   geom_text(data = subset(filter(figure_values, Difference == "Richness")),
  #             aes(x=ObsDiff, y=1150, label=(paste("Observed",as.character(ObsDiff),sep=" == "))), 
  #             parse=TRUE,size=2)
  
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
  
  facet_labels<-c("A","B","C","D")
  p1<-tag_facet(p1,open="", close="", tag_pool=facet_labels,vjust=-1,hjust=-0.1)
  p1
  
  
  
  
  
  #################################################################
  # Now do the figure of the Bootsrapped OAinPW based on Mirror n_papers
  #################################################################
  
  n<-n_mirror
  n<-unlist(n)
  n<-as.data.frame(n)
  n<-n$n
  
  n3<-n_mirror_noCHNUSA
  n3<-unlist(n3)
  n3<-as.data.frame(n3)
  n3<-n3$n3
  
  
  nboot <-2000 #number of permutations
  InvSimp <-rep(NA, nboot)
  Richness<-rep(NA, nboot)
  Shannon<-rep(NA, nboot)
  Even<-rep(NA, nboot)
  n_papers<-rep(NA, nboot)
  n_distinct_papers<-rep(NA, nboot)
  replicate<-data.frame(replicate=as.numeric())
  Countries_OAinPW<-rep(NA, nboot)
  
  # These are where you will store the results - one for the randomly selected in a way
  # that mimics the "mirror" populations and one for the randomly assigned to the 
  # subscription journal populations.
  boot_OAinPW <- data.frame(Richness,InvSimp,Shannon,Even,n_papers,n_distinct_papers)
  replicate<-data.frame(replicate)
  boot_noCHNUSA <- boot_OAinPW
  replicate_noCHNUSA<-replicate
  Countries_noCHNUSA<-rep(NA, nboot)
  rm(InvSimp,Richness,Shannon,Even,n_papers)
  
  set.seed(1)
  
  for(i in 1:nboot){
    
    # BE SURE PERMUTE OA IS SET TO SAMPlE *WITHOUT* REPLACEMENT 
    # AS PER B INOUYE SUGGESTION
    
    source("./Rscript/functions/permute_OA.R")
    # Sample at random without replacement from the pooled OA articles in a way
    # that mimics the number of articles in each mirror journal. 
    boot_pool<-permute_OA(OA_paper_pool,n,"boot")[1]
    boot_pool<-as.data.frame(boot_pool)
    
    # do the stats for the "mirror" group
    AuPosition<-"author_first"
    ArticleType<-"OA"
    JrnlType<-"PW"
    results_boot<-DivRichCalc(boot_pool,AuPosition,JrnlType,ArticleType)
    boot_OAinPW[i,1]<-(results_boot)[1]
    boot_OAinPW[i,2]<-(results_boot)[2]
    boot_OAinPW[i,3]<-(results_boot)[4]
    boot_OAinPW[i,4]<-(results_boot)[5]
    boot_OAinPW[i,5]<-nrow(boot_pool)
    boot_OAinPW[i,6]<-boot_pool %>% summarize(n_distinct(refID))
    Countries_OAinPW[i]<-(results_mirror)[3]
    
    count<-data.frame(replicate=rep(i, each=boot_OAinPW[i,1]))
    replicate<- bind_rows(replicate,count)
    
    ##########################
    # REPEAT FOR NO_CHNUSA
    ########################
    boot_pool_2<-permute_OA(OA_paper_pool_NOCHNUSA,n3,"boot")[1]
    boot_pool_2<-as.data.frame(boot_pool_2)
    
    # do the stats for the "mirror" group
    AuPosition<-"author_first"
    ArticleType<-"OA"
    JrnlType<-"PW"
    results_boot_2<-DivRichCalc(boot_pool_2,AuPosition,JrnlType,ArticleType)
    boot_noCHNUSA[i,1]<-(results_boot_2)[1]
    boot_noCHNUSA[i,2]<-(results_boot_2)[2]
    boot_noCHNUSA[i,3]<-(results_boot_2)[4]
    boot_noCHNUSA[i,4]<-(results_boot)[5]
    boot_noCHNUSA[i,5]<-nrow(boot_pool_2)
    boot_noCHNUSA[i,6]<-boot_pool_2 %>% summarize(n_distinct(refID))
    Countries_noCHNUSA[i]<-(results_boot)[3]
    
    count2<-data.frame(replicate_noCHNUSA=rep(i, each=boot_noCHNUSA[i,1]))
    replicate_noCHNUSA<- bind_rows(replicate_noCHNUSA,count2)
    
    
  }
  
  # Label the resulting permutations
  boot_OAinPW$ArticleCat<-"OA"
  boot_OAinPW$comparison<-"Mirror v BS_OAinPW"
  boot_OAinPW$rep<-seq(1:nboot)
  boot_OAinPW$countries<-"all countries"
  # permutations_mirror$comparison<-"first of coauthored"
  # permutations_mirror$comparison<-"solo"
  boot_noCHNUSA$ArticleCat<-"OA"
  boot_noCHNUSA$comparison<-"Mirror v BS_OAinPW"
  boot_noCHNUSA$rep<-seq(1:nboot)
  boot_noCHNUSA$countries<-"China and USA excluded"
  
  all_boot<-bind_rows(boot_OAinPW,boot_noCHNUSA)
  
  boot_OAinPW_long<- pivot_longer(all_boot,c(Richness,InvSimp,Shannon,Even), names_to = "metric")
  # figure_values <- figure_values %>% dplyr::rename("metric"="Difference")
  
  
  
  fig2_data<-boot_OAinPW_long %>% filter(metric=="Richness"|metric=="InvSimp")
  fig2_data$metric<-gsub("InvSimp","Diversity",fig2_data$metric)
  
  p2<-
    ggplot(fig2_data, aes(x=value)) +
    geom_histogram(bins=30, color="black",
                   fill="darkgray",
                   size=0.1,alpha=0.6, position = 'identity') +
    facet_grid(cols = vars(metric),rows=vars(countries),
               # labeller=labeller(author = author.labels),
               scales="free") +
    geom_hline((aes(yintercept=-Inf)), color="black") +
    geom_vline((aes(xintercept=-Inf)) , color="black") +
    guides(fill=guide_legend("Metric"))+
    # scale_x_continuous(limits = c(-40, 20),breaks = seq(-40,20, by=10),expand = c(0.01,0))+
    # scale_y_continuous(limits = c(-10, 4000),breaks = seq(0, 4000, by=500),expand = c(0.01,0))+
    labs(x = "Value of Metric for Bootstraped Collections of OA Articles in Subscription Journals", 
         y = "Frequency") +
    
    geom_segment(data = subset(filter(figure_values, metric == "Diversity", countries=="all countries")),
                  aes(x = Mirror , y = 0, xend = Mirror, yend = 200), 
                  colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values, metric == "Diversity", countries=="all countries")),
              aes(x=Mirror, y=210, label=(paste("Mirror",as.character(Mirror),sep=" == "))),
              parse=TRUE,size=2)+
    geom_segment(data = subset(filter(figure_values, metric == "Richness", countries=="all countries")),
                 aes(x = Mirror , y = 0, xend = Mirror, yend = 200),
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values, metric == "Richness", countries=="all countries")),
              aes(x=Mirror, y=210, label=(paste("Mirror",as.character(Mirror),sep=" == "))),
              parse=TRUE,size=2)+
    
    # WITHOUT CHN USA
    
    geom_segment(data = subset(filter(figure_values, metric == "Diversity", countries=="China and USA excluded")),
                 aes(x = Mirror , y = 0, xend = Mirror, yend = 200), 
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values, metric == "Diversity", countries=="China and USA excluded")),
              aes(x=Mirror, y=210, label=(paste("Mirror",as.character(Mirror),sep=" == "))),
              parse=TRUE,size=2)+
    geom_segment(data = subset(filter(figure_values, metric == "Richness", countries=="China and USA excluded")),
                 aes(x = Mirror , y = 0, xend = Mirror, yend = 200),
                 colour = "red",linetype="solid")+
    geom_text(data = subset(filter(figure_values, metric == "Richness", countries=="China and USA excluded")),
              aes(x=Mirror, y=210, label=(paste("Mirror",as.character(Mirror),sep=" == "))),
              parse=TRUE,size=2)+
    
    
    coord_cartesian(clip="off")
  
  p2<-p2+
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
  
  facet_labels<-c("A","B","C","D")
  p2<-tag_facet(p2,open="", close="", tag_pool=facet_labels,vjust=-1,hjust=-0.1)
  p2
  
  foo<-fig2_data %>% filter(metric=="Diversity") %>% filter(countries=="all countries")
  summary(foo$value>12)
  
  foo<-fig2_data %>% filter(metric=="Diversity") %>% filter(countries=="China and USA excluded")
  summary(foo$value>18)
  
  foo<-fig2_data %>% filter(metric=="Richness") %>% filter(countries=="all countries")
  summary(foo$value>58)
  
  foo<-fig2_data %>% filter(metric=="Richness") %>% filter(countries=="China and USA excluded")
  summary(foo$value>56)
  