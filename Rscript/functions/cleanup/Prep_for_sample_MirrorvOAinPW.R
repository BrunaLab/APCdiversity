Prep_for_sample_MirrorvOAinPW<-function(Dataset) {

# How to sample different amounts from different groups is very nicely laid out in this post:
# https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html

# create a df telling you how many articles published in each OAinPW journal
# will sample in this amount from the df of PW articles
# Dataset<-AllData
# Dataset<-sole_ALL
# Dataset<-first_ALL
  
  mirrorOA_papers <- Dataset %>%
  filter(JrnlType=="OA") %>% 
  group_by(pair_key,Journal) %>%
  summarize(n=n_distinct(DOI)) %>% 
  arrange(Journal) %>%
  ungroup() 
  # select(pair_key,n)
  # mirrorOA_papers$pair_key<-as.factor(mirrorOA_papers$pair_key)
nlevels(mirrorOA_papers$pair_key)
mirrorOA_papers$pair_key<-droplevels(mirrorOA_papers$pair_key)
nlevels(mirrorOA_papers$pair_key)
mirrorOA_sample_n<-sum(mirrorOA_papers$n)

OAinPW_papers<- Dataset %>%
  filter(JrnlType == "PW" & ArticleType == "OA") 

# %>% 
#   group_by(pair_key,Journal,refID) %>%
#   slice(1) %>%
#   ungroup() %>% 
#   group_by(pair_key,Journal) %>%
#   summarize(n=n_distinct(refID))
#   
# OAinPW_n<-sum(OAinPW_papers$n)
#   # select(pair_key,n)
# OAinPW_papers$pair_key<-as.factor(OAinPW_papers$pair_key)
# nlevels(OAinPW_papers$pair_key)
# OAinPW_papers$pair_key<-droplevels(OAinPW_papers$pair_key)
# nlevels(PW_papers$pair_key)
# STEP 1
# Are you sure all the journals in PW df are represented 
# in the OAinPW df and vice versa? FALSES would indicate "no"

# summary(levels(mirrorOA_papers$pair_key)==levels(OAinPW_papers$pair_key))

# You could have the same number of journals in both, but they might not 
# be the same journals. This will ensure the journals being used are in both
# the OAinPW and PW lists (using intersection of pair_key in OAinPW and PW).
# The intersection command gets the ones in both, and then uses that to confirm
# the list of OAinPW journals to be using.
# mirrorOA_papers_pk<-mirrorOA_papers$pair_key
# OAinPW_papers_pk<-OAinPW_papers$pair_key
# Ipk<-intersect(OAinPW_papers_pk,mirrorOA_papers_pk)
# Ipk<-as_factor(Ipk)
# OAinPW_papers<-mirrorOA_papers %>% filter(pair_key%in%Ipk)
# mirrorOA_papers<-droplevels(mirrorOA_papers)
# 
# PW_papers<-semi_join(PW_papers,OAinPW_sample,by="pair_key") #this is what makes sure you are only sampling from PW journals for which there is OAinPW
# # PW_papers$pair_key<-as.factor(PW_papers$pair_key)
# nlevels(PW_papers$pair_key)
# PW_papers<-droplevels(PW_papers)
# nlevels(PW_papers$pair_key)
# PW_papers<-arrange(PW_papers,pair_key)
# # This is how many you are going to be sampling from each PW Journal.
# # n is no. of articles in each OAinPW journal 
# n<-as.integer(OAinPW_sample$n)

Prep_for_sample_MirrorvOAinPW<-list(OAinPW_papers,mirrorOA_sample_n)
return(Prep_for_sample_MirrorvOAinPW)

}