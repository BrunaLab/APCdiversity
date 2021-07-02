

# ALL THE OA PAPERS (Mirror + PW)
OA_paper_pool<-AllData %>% 
  filter(ArticleType=="OA") %>% 
  group_by(refID) %>% 
  slice(1)


# Number of OA papers in all journals
nOA <- OA_paper_pool %>% 
  group_by(pair_key, JrnlType) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = JrnlType, values_from=n) %>%
  replace_na(list(PW = 0))  %>% 
  mutate(diff = PW-OA) %>% 
  dplyr::rename("mirror"="OA", "OAinPW"="PW") %>% 
  arrange(mirror)

n<-nOA$mirror

n3<-nOA$OAinPW

nperm <-20 #number of permutations
InvSimp <-rep(NA, nperm)
Richness<-rep(NA, nperm)
Shannon<-rep(NA, nperm)
Even<-rep(NA, nperm)
n_papers<-rep(NA, nperm)
n_distinct_papers<-rep(NA, nperm)
replicate<-data.frame(replicate=as.numeric())
Countries_OAinPW<-rep(NA, nperm)

# These are where you will store the results - one for the randomly selected in a way
# that mimics the "mirror" populations and one for the randomly assigned to the 
# subscription journal populations.
boot_OAinPW <- data.frame(Richness,InvSimp,Shannon,Even,n_papers,n_distinct_papers)
replicate<-data.frame(replicate)
boot_noCHNUSA <- boot_OAinPW
replicate_noCHNUSA<-replicate
Countries_noCHNUSA<-rep(NA, nperm)
rm(InvSimp,Richness,Shannon,Even,n_papers)

set.seed(1)

for(i in 1:nperm){
  
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
boot_OAinPW$rep<-seq(1:nperm)
boot_OAinPW$countries<-"all countries"
# permutations_mirror$comparison<-"first of coauthored"
# permutations_mirror$comparison<-"solo"
boot_noCHNUSA$ArticleCat<-"OA"
boot_noCHNUSA$comparison<-"Mirror v BS_OAinPW"
boot_noCHNUSA$rep<-seq(1:nperm)
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

