Prep_for_samplePW<-function(Dataset) {
# How to sample different amounts from different groups is very nicely laid out in this post:
# https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html

# create a df telling you how many articles published in each OA journal
# will sample in this amount from the df of PW articles
# Dataset<-coauthor_ALL_first
  # Dataset<-AllData %>% filter(author=="coauthored") %>% 
  #   group_by(refID) %>%   
  #   slice(1)
  
OA_sample <- Dataset %>%
  filter(ArticleType == "OA") %>% 
  filter(JrnlType == "OA") %>% 
  group_by(pair_key) %>% 
  summarize(n=n_distinct(refID))%>% 
  arrange(pair_key) %>% 
  ungroup() %>% 
  select(pair_key,n)
# sum(OA_sample$n)
OA_sample$pair_key<-as.factor(OA_sample$pair_key)
nlevels(OA_sample$pair_key)
OA_sample$pair_key<-droplevels(OA_sample$pair_key)
nlevels(OA_sample$pair_key)

PW_papers<- Dataset %>%
  filter(ArticleType == "PW") %>% 
  group_by(pair_key,Journal,refID) %>% 
  slice(1) %>% 
  select(refID,Journal,JrnlType,pair_key)

# This is to check if there are more OA than PW (there need to be)
PW_sample<-as.data.frame(PW_papers) %>%
  group_by(pair_key) %>%
  summarize(PWn=n()) %>%
  ungroup()
PW_sample$pair_key<-as.factor(PW_sample$pair_key)
N_check<-left_join(PW_sample,OA_sample) %>%
  mutate(PWminusOA=PWn-n) %>%
  dplyr::rename("OAn"="n") %>% 
  mutate(perc_of_PW=OAn/(PWn+OAn)*100) %>% 
  # arrange(desc(perc_of_PW))
  arrange(PWminusOA)
# N_check
# PW_papers$Journal<-as.factor(PW_papers$pair_key)
# nlevels(PW_papers$pair_key)
# PW_papers$pair_key<-droplevels(PW_papers$pair_key)
# nlevels(PW_papers$pair_key)
# STEP 1
# Are you sure all the journals in PW df are represented 
# in the OA df and vice versa? FALSES would indicate "no"
summary(levels(as.factor(OA_sample$pair_key))==levels(as.factor(PW_papers$pair_key)))

# You could have the same number of journals in both, but they might not 
# be the same journals. This will ensure the journals being used are in both
# the OA and PW lists (using intersection of pair_key in OA and PW).
# The intersection command gets the ones in both, and then uses that to confirm
# the list of OA journals to be using.
OA_pk<-OA_sample$pair_key
PW_papers_pk<-PW_papers$pair_key
Ipk<-intersect(PW_papers_pk,OA_pk)
Ipk<-as_factor(Ipk)
OA_sample$pair_key
OA_sample<-OA_sample %>% filter(pair_key%in%Ipk)
OA_sample<-droplevels(OA_sample)
# PW_papers$pair_key<-as.numeric(PW_papers$pair_key)
# OA_sample$pair_key<-as.numeric(OA_sample$pair_key)
#this next line is what makes sure you are only sampling from PW journals for which there is OA
PW_papers$pair_key<-as.factor(PW_papers$pair_key)
PW_papers<-semi_join(PW_papers,OA_sample,by="pair_key") 
PW_papers$Journal<-as.factor(PW_papers$Journal)
nlevels(PW_papers$Journal)
PW_papers$Journal<-droplevels(PW_papers$Journal)
nlevels(PW_papers$Journal)
nlevels(PW_papers$pair_key)
PW_papers<-droplevels(PW_papers)
nlevels(PW_papers$Journal)
PW_papers<-arrange(PW_papers,pair_key,Journal)
# This is how many you are going to be sampling from each PW Journal.
# n is no. of articles in each OA journal 
n<-as.integer(OA_sample$n)
OA_sample$pair_key<-as.factor(OA_sample$pair_key)
Prep_for_samplePW<-list(PW_papers,n,N_check)
return(Prep_for_samplePW)

}