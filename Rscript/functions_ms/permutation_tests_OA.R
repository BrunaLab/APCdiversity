permutation_tests_OA<-function(AllData){
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

nOA$mirror


  source("./Rscript/functions_ms/DivRichCalc.R")
 

  OA_paper_pool <- as.data.frame(OA_paper_pool)
  

  
  nperm <-5000 #number of permutations
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
    
    source("./Rscript/functions_ms/permute_OA.R")
    
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
  
  
  permutations_OAinPW$ArticleCat<-"OAinPW"
  permutations_OAinPW$countries<-"all"
  permutations_OAinPW$rep<-seq(1:nperm)
  
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
    source("./Rscript/functions_ms/permute_OA.R")
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
    mutate(RichDiff=Richness_Mirror-Richness_OAinPW) %>% 
    mutate(EvenDiff=Even_Mirror-Even_OAinPW) 
  colnames(All_boots_wide)
  
  write_csv(All_boots_wide,"./data_clean/MirrorvOAinPW_permutations.csv")
  
  
}