# This does permutation tests to see to determine if the difference
# beween the observed diversity of Mirror Journal OA articles and 
# OA articles in subscription journals is sognificanl;ty different 
# from what would be expected if the articles in these two groups were 
# assigned at random from the pool of all OA articles.  

# STEP 1: calculate the richness and diversity of "OA in Mirror" and "PA in PW"
# STEP 2: calculate the diff in richness and diversity between "OA in Mirror" and "PA in PW"
# STEP 3: create 5000 "Mirror" and "OAinPW" collections identical in size/structure to 
#         the original ones using sampling without replacement.
# STEP 4: calc the rich and diversity of each of these, then the difference in rich and diversity
# STEP 5: 

# STEP X: Repeat with CHINA USA excluded

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
OA_paper_pool<-AllData %>% 
  filter(ArticleType=="OA") %>% 
  group_by(refID) %>% 
  slice(1) %>% 
  filter(author=="coauthored")


# Number of OA papers in all journals
nOA <- OA_paper_pool %>% 
  group_by(pair_key, JrnlType) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = JrnlType, values_from=n) %>%
  replace_na(list(PW = 0))  %>% 
  mutate(diff = PW-OA) %>% 
  dplyr::rename("mirror"="OA", "OAinPW"="PW") %>% 
  arrange(mirror)

nOA$mirror


source("./Rscript/functions/DivRichCalc.R")
# OA_paper_pool_first
# OA_paper_pool_solo
# n<-n_first_mirror
# n<-n_solo_mirror
# This is the number in each of the mirror journals
# n <- n_mirror
OA_paper_pool <- as.data.frame(OA_paper_pool)



nperm <-500 #number of permutations
InvSimp <-rep(NA, nperm)
Richness<-rep(NA, nperm)
Shannon<-rep(NA, nperm)
Even<-rep(NA, nperm)
n_papers<-rep(NA, nperm)
replicate<-data.frame(replicate=as.numeric())
Countries_mirror<-rep(NA, nperm)
Countries_OAinPW<-rep(NA, nperm)

# These are where you will store the results - one for the randomly selected in a way
# that mimics the "mirror" populations and one for the randomly assigned to the 
# subscription journal populations.
permutations_mirror <- data.frame(Richness, InvSimp, Shannon, Even, n_papers)
permutations_OAinPW <- data.frame(Richness, InvSimp, Shannon, Even, n_papers)
# replicate<-data.frame(replicate)



rm(InvSimp, Richness, Shannon, Even, n_papers)

set.seed(1)

nOA<-nOA %>% 
  arrange(pair_key) 
n_mirror_sum <- sum(nOA$mirror)
n_mirror_sum
# n_mirror <- as.data.frame(n_mirror)
# n_mirror <- n_mirror$n
sum(n_mirror_sum)*2
n <- nOA$mirror




for(i in 1:nperm){
  
  # BE SURE PERMUTE OA IS SET TO SAMPlE *WITHOUT* REPLACEMENT
  
  source("./Rscript/functions/permute_OA.R")
  
  # Sample at random without replacement from the pooled OA articles in a way
  # that mimics the number of articles in each mirror journal. 
  mirror_sample<-permute_OA(OA_paper_pool,n,"perm")
  mirror_sample<-as.data.frame(mirror_sample[1])
  
  # Now use the refIDs of the ones sample to figure out which ones 
  # *weren't* selected and assign these to the "subscription journal group 
  mirror_sample_refID<-mirror_sample$refID
  suppressMessages(OAinPW_sample<-anti_join(OA_paper_pool,mirror_sample))
  
  AuPosition<-"author_first"
  ArticleType<-"OA"
  # JrnlType<-"OA"
  JrnlType<-"both"
  
  # do the stats for the "mirror" group
  results_mirror<-DivRichCalc(mirror_sample, AuPosition, JrnlType, ArticleType)
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
  
  #this is the counter to let you know where it is in the loop
  cat("\r", i, "of", nperm) 
  flush.console()
}

# Label the resulting permutations
permutations_mirror$ArticleCat<-"Mirror"
permutations_mirror$countries<-"all"
permutations_mirror$rep<-seq(1:nperm)
# permutations_mirror$comparison<-"first of coauthored"
# permutations_mirror$comparison<-"solo"

permutations_OAinPW$ArticleCat<-"OAinPW"
permutations_OAinPW$countries<-"all"
permutations_OAinPW$rep<-seq(1:nperm)
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

nOA_noCHNUSA <- OA_paper_pool_NOCHNUSA %>% 
  group_by(pair_key, JrnlType) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = JrnlType, values_from=n) %>%
  replace_na(list(PW = 0))  %>% 
  mutate(diff = PW-OA) %>% 
  dplyr::rename("mirror"="OA", "OAinPW"="PW") %>% 
  arrange(pair_key)



n2<-nOA_noCHNUSA$mirror
OA_paper_pool_NOCHNUSA<-as.data.frame(OA_paper_pool_NOCHNUSA)

# nperm <-6 #number of permutations
InvSimp <-rep(NA, nperm)
Richness<-rep(NA, nperm)
Shannon<-rep(NA, nperm)
Even<-rep(NA, nperm)
n_papers<-rep(NA, nperm)
replicate_no<-data.frame(replicate=as.numeric())
Countries_no_mirror<-rep(NA, nperm)
Countries_no_OAinPW<-rep(NA, nperm)

# These are where you will store the results - one for the randomly selected in a way
# that mimics the "mirror" populations and one for the randomly assigned to the 
# subscription journal populations.
permutations_mirror_no <- data.frame(Richness,InvSimp,Shannon,Even,n_papers)
permutations_OAinPW_no <- data.frame(Richness,InvSimp,Shannon,Even,n_papers)
# replicate<-data.frame(replicate)

rm(InvSimp,Richness,Shannon,Even,n_papers)

set.seed(1)

for(i in 1:nperm){
  
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
  suppressMessages(OAinPW_no_sample<-anti_join(OA_paper_pool_NOCHNUSA,mirror_no_sample))
  
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
  #this is the counter to let you know where it is in the loop
  cat("\r", i, "of", nperm) 
  flush.console()
}

# Label the resulting permutations
permutations_mirror_no$ArticleCat<-"Mirror"
permutations_mirror_no$countries<-"no_chnusa"
permutations_mirror_no$rep<-seq(1:nperm)
# permutations_mirror$comparison<-"first of coauthored"
# permutations_mirror$comparison<-"solo"

permutations_OAinPW_no$ArticleCat<-"OAinPW"
permutations_OAinPW_no$countries<-"no_chnusa"
permutations_OAinPW_no$rep<-seq(1:nperm)
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

write_csv(All_boots_wide,"./output/MirrorvOAinPW_first_permutations.csv")

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


mirror_R<-as.numeric(mirror_stats[1])
mirror_D<-round(as.numeric(mirror_stats[2],3))  
subscription_R<-as.numeric(subscription_stats[1])
subscription_D<-round(as.numeric(subscription_stats[2],3))

# OBSERVED RESULTS: NO CHN USA
mirror_noCHNUSA_stats<-DivRichCalc(OA_paper_pool_NOCHNUSA,"author_first","OA","OA")
subscription_noCHNUSA_stats<-DivRichCalc(OA_paper_pool_NOCHNUSA,"author_first","PW","OA")
mirror_no_R<-as.numeric(mirror_noCHNUSA_stats[1])
mirror_no_D<-round(as.numeric(mirror_noCHNUSA_stats[2],3))  
subscription_no_R<-as.numeric(subscription_noCHNUSA_stats[1])
subscription_no_D<-round(as.numeric(subscription_noCHNUSA_stats[2],3))

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

obs_values<-bind_rows(mirror,subscription,mirror_no,subscription_no)

obs_values$metric<-rep(c("Diversity","Richness"),2)
obs_values$countries<-c("all","all","no_chnusa","no_chnusa")

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


pvals<-data.frame(pval=c(p_div_all,p_rich_all,p_div_no,p_rich_no))
pvals$metric<-rep(c("Diversity","Richness"),2)
pvals$countries<-c("all","all","no_chnusa","no_chnusa")
obs_values<-left_join(obs_values,pvals)

rich_div_stats
figure_values<-obs_values
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




# # How many by Pair_key
# OApapers_byPK<-OApapers %>%
#   group_by(refID) %>% 
#   slice(1) %>% 
#   group_by(pair_key) %>% 
#   summarize(n=n()) %>% 
#   ungroup() %>% 
#   select(pair_key,n)
# n_allOApapers<-OApapers_byPK$n
# sum(n_allOApapers)
# 
# # All the OA articles in subscription journals
# OAinPW_articles <- OApapers %>%
#   filter(JrnlType == "PW") %>% 
#   filter(ArticleType=="OA") %>% 
#   group_by(refID) %>% 
#   slice(1) 
# 
# # All the OA articles in mirror journals
# mirror_articles <- OApapers %>%
#   filter(JrnlType == "OA") %>% 
#   filter(ArticleType=="OA") %>% 
#   group_by(refID) %>% 
#   slice(1) 
# 
# # How many articles in each of the subscription journals
# OAinPW_count <- OAinPW_articles %>%
#   group_by(pair_key) %>% 
#   summarize(n=n()) %>% 
#   ungroup() %>% 
#   select(pair_key,n)
# n_OAinPW<-OAinPW_count$n
# sum(n_OAinPW)
# 
# # How many articles in each of the mirror journals
# mirror_count <- mirror_articles %>%
#   group_by(pair_key) %>% 
#   summarize(n=n()) %>% 
#   ungroup() %>% 
#   select(pair_key,n)
# n_mirror<-mirror_count$n
# sum(n_mirror)
# 
# ### WITHOUT CHN USA
# OA_papers_NOCHNUSA<-OApapers %>% 
#   filter(Code!="CHN") %>% 
#   filter(Code!="USA")
# 
# n_OAinPW_noCHNUSA <- OA_paper_pool_NOCHNUSA %>%
#   group_by(pair_key) %>% 
#   summarize(n=n()) %>% 
#   ungroup() %>% 
#   select(pair_key,n)
# n_OAinPW_noCHNUSA<-n_OAinPW_noCHNUSA$n
# # n_OAinPW_noCHNUSA<-n_OAinPW_noCHNUSA$n
# sum(n_OAinPW_noCHNUSA)
# 
# mirror_noCHNUSA <- OA_papers_NOCHNUSA %>%
#   filter(JrnlType=="OA")
# 
# n_mirror_noCHNUSA <- mirror_noCHNUSA %>%
#   group_by(pair_key) %>% 
#   summarize(n=n()) %>% 
#   ungroup() %>% 
#   select(pair_key,n)
# n_mirror_noCHNUSA<-n_mirror_noCHNUSA$n
# sum(n_mirror_noCHNUSA)
# # n_OAinPW_noCHNUSA>n_mirror_noCHNUSA
# # n_OAinPW_noCHNUSA-n_mirror_noCHNUSA
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Are there enough articles to draw from the pool?
# n_allOApapers>n_OAinPW
# n_allOApapers>n_mirror
# 
# # The pool of OA papers from which we will be drawing
#   OA_paper_pool<- OApapers %>%
#     group_by(refID) %>% 
#     slice(1) 
#   
#   # It has been suggested that permutation tests use 2*n replicates
#   nrow(OA_paper_pool)*2
############################################
# now sample and iterate!
# THIS IS FOR MATCHED SUBSAMPLING - FIRST AUTHOR
# 1<-Richness  2<-InvSimpsons 3<-Countries
############################################
# OA_paper_pool_solo<-OA_paper_pool %>% 
#     filter(author=="solo")
#  n_solo_mirror<- OA_paper_pool_solo %>%
#    filter(JrnlType=="OA") %>% 
#     group_by(pair_key) %>% 
#     summarize(n=n()) %>% 
#     ungroup() %>% 
#     select(pair_key,n)
#   n_solo_mirror<-n_solo_mirror$n
#   sum(n_solo_mirror)
#   n_solo_mirror<-n_solo_mirror
#   n_solo_mirror<-unlist(n_solo_mirror)
#   n_solo_mirror<-as.data.frame(n_solo_mirror)
#   n_solo_mirror<-n_solo_mirror$n
#   sum(n_solo_mirror)
#   sum(n_solo_mirror)*2
#   
#   
#   OA_paper_pool_first<-OA_paper_pool %>% 
#     filter(author=="coauthored")
#   n_first_mirror <- OA_paper_pool_first %>%
#     filter(JrnlType=="OA") %>% 
#     group_by(pair_key) %>% 
#     summarize(n=n()) %>% 
#     ungroup() %>% 
#     select(pair_key,n)
#   n_first_mirror<-n_first_mirror$n
#   sum(n_first_mirror)
#   n_first_mirror<-n_first_mirror
#   n_first_mirror<-unlist(n_first_mirror)
#   n_first_mirror<-as.data.frame(n_first_mirror)
#   n_first_mirror<-n_first_mirror$n
#   sum(n_first_mirror)*2
#   sum(n_first_mirror)
#   
# #   
#   all_OA_papers<-OApapers %>% 
#     group_by(refID) %>% 
#     slice(1) 

