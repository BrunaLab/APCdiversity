AllPapers<-AllData %>% 
  group_by(refID) %>% 
  slice(1)
PW_papers<-AllPapers %>% 
  filter(ArticleType=="PW") %>% 
  group_by(refID) %>% 
  slice(1)


OA_papers<-AllPapers %>% 
  filter(ArticleType=="OA")%>% 
  group_by(refID) %>% 
  slice(1)

nrow(OA_papers)

n_OA<- AllPapers %>%
  filter(ArticleType=="OA") %>% 
  group_by(pair_key) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  select(pair_key,n)
n_OA<-n_OA$n
sum(n_OA)
  

nboot <-5000 #number of permutations
InvSimp <-rep(NA, nboot)
Richness<-rep(NA, nboot)
Shannon<-rep(NA, nboot)
Even<-rep(NA, nboot)
n_papers<-rep(NA, nboot)
replicate<-data.frame(replicate=as.numeric())
Countries_OA<-rep(NA, nboot)
Countries_PW<-rep(NA, nboot)

# These are where you will store the results - one for the randomly selected in a way
# that mimics the "OA" populations and one for the randomly assigned to the 
# subscription journal populations.
permutations_OA <- data.frame(Richness,InvSimp,Shannon,Even,n_papers)
permutations_PW <- data.frame(Richness,InvSimp,Shannon,Even,n_papers)
# replicate<-data.frame(replicate)



rm(InvSimp,Richness,Shannon,Even,n_papers)


set.seed(1)

for(i in 1:nboot){
  
  # BE SURE PERMUTE OA IS SET TO SAMPlE *WITHOUT* REPLACEMENT
  
  # Sample at random without replacement from the pooled OA articles in a way
  # that mimics the number of articles in each OA journal. 
  OA_sample<-AllPapers %>% 
    arrange(pair_key) %>% 
    group_by(pair_key) %>% 
    nest() %>%            
    ungroup() %>% 
    mutate(n) %>%
    mutate(samp = map2(data, n_OA, sample_n, replace=FALSE)) %>% # This is sampling WITHOUT replacement
    select(-data) %>%
    unnest(samp)
  # OA_sample<-as.data.frame(OA_sample[1])
  
  # Now use the refIDs of the ones sample to figure out which ones 
  # *weren't* selected and assign these to the "subscription journal group 
  OA_sample_refID<-OA_sample$refID
  PW_sample<-anti_join(AllData,OA_sample)
  
  AuPosition<-"author_first"
  ArticleType<-"OA"
  # JrnlType<-"OA"
  JrnlType<-"both"
  
  # do the stats for the "OA" group
  results_OA<-DivRichCalc(OA_sample,AuPosition,JrnlType,ArticleType)
  permutations_OA[i,1]<-(results_OA)[1]
  permutations_OA[i,2]<-(results_OA)[2]
  permutations_OA[i,3]<-(results_OA)[4]
  permutations_OA[i,4]<-(results_OA)[5]
  permutations_OA[i,5]<-nrow(OA_sample)
  Countries_OA[i]<-(results_OA)[3]
  
  count<-data.frame(replicate=rep(i, each=permutations_OA[i,1]))
  replicate<- bind_rows(replicate,count)
  
  # Do the stats for the "subscription" group
  results_PW<-DivRichCalc(PW_sample,AuPosition,JrnlType,ArticleType)
  permutations_PW[i,1]<-(results_PW)[1]
  permutations_PW[i,2]<-(results_PW)[2]
  permutations_PW[i,3]<-(results_PW)[4]
  permutations_PW[i,4]<-(results_PW)[5]
  permutations_PW[i,5]<-nrow(PW_sample)
  Countries_PW[i]<-(PW_sample)[3]
  
  count<-data.frame(replicate=rep(i, each=permutations_PW[i,1]))
  replicate<- bind_rows(replicate,count)
  
}

# Label the resulting permutations
permutations_OA$ArticleCat<-"OA"
permutations_OA$countries<-"all"
permutations_OA$rep<-seq(1:nboot)
# permutations_OA$comparison<-"first of coauthored"
# permutations_OA$comparison<-"solo"

permutations_PW$ArticleCat<-"PW"
permutations_PW$countries<-"all"
permutations_PW$rep<-seq(1:nboot)
# permutations_PW$comparison<-"first of coauthored"
# permutations_PW$comparison<-"solo"
diff_OA_minus_PW<-permutations_OA$InvSimp-permutations_PW$InvSimp
hist(diff_OA_minus_PW)

OAdiv<-as.numeric(DivRichCalc(OA_papers,"author_first","both","OA")[2])
PWdiv<-as.numeric(DivRichCalc(PW_papers,"author_first","PW","PW")[2])
obs_diff<-OAdiv-PWdiv
summary(obs_diff>diff_OA_minus_PW)
# Bind the results together and rename/cleanup
# binding in long format
All_boots_long<-bind_rows(permutations_OA, permutations_PW) 
write_csv(All_boots_long,"./output/permutation_OAvsPW_all.csv")