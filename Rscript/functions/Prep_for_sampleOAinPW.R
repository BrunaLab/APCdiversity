Prep_for_sampleOAinPW<-function(Dataset,OAtype) {

# How to sample different amounts from different groups is very nicely laid out in this post:
# https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html

# create a df telling you how many articles published in each OAinPW journal
# will sample in this amount from the df of PW articles
# Dataset<-AllData
# Dataset<-sole_ALL  
  
  if ((OAtype=="mirror")==TRUE) {
    DatasetOA<-Dataset %>% 
      filter(JrnlType == "OA") %>% 
      filter(ArticleType == "OA") %>% 
      filter(AuthorNum==1)
    
  } else if ((OAtype=="OAinPW")==TRUE) {
    DatasetOA<-Dataset %>% 
      filter(JrnlType == "PW") %>% 
      filter(ArticleType == "OA") %>% 
      filter(AuthorNum==1)
  } else {
    stop("error in type selection")
  }
  
OAinPW_sample <- DatasetOA %>%
  # filter(JrnlType == "PW") %>%
  # filter(ArticleType == "OA") %>% 
  group_by(pair_key,Journal) %>% 
  summarize(n=n_distinct(refID))%>% 
  arrange(Journal) %>% 
  ungroup() %>% 
  select(pair_key,n)
OAinPW_sample$pair_key<-as.factor(OAinPW_sample$pair_key)
nlevels(OAinPW_sample$pair_key)
OAinPW_sample$pair_key<-droplevels(OAinPW_sample$pair_key)
nlevels(OAinPW_sample$pair_key)

PW_papers<- Dataset %>%
  filter(JrnlType == "PW") %>%
  filter(ArticleType == "PW") %>%
  group_by(pair_key,Journal,refID) %>% 
  slice(1) %>% 
  select(refID,Journal,JrnlType,pair_key)
  # summarize(n=n_distinct(DOI)) %>% 
  # arrange(Journal) %>% 
  # ungroup() %>% 
# select(pair_key,n)
PW_papers$pair_key<-as.factor(PW_papers$pair_key)
nlevels(PW_papers$pair_key)
PW_papers$pair_key<-droplevels(PW_papers$pair_key)
nlevels(PW_papers$pair_key)
# STEP 1
# Are you sure all the journals in PW df are represented 
# in the OAinPW df and vice versa? FALSES would indicate "no"

# summary(levels(PW_papers$pair_key)==levels(OAinPW_sample$pair_key))

# You could have the same number of journals in both, but they might not 
# be the same journals. This will ensure the journals being used are in both
# the OAinPW and PW lists (using intersection of pair_key in OAinPW and PW).
# The intersection command gets the ones in both, and then uses that to confirm
# the list of OAinPW journals to be using.
OAinPW_pk<-OAinPW_sample$pair_key
PW_papers_pk<-PW_papers$pair_key
Ipk<-intersect(PW_papers_pk,OAinPW_pk)
Ipk<-as_factor(Ipk)
OAinPW_sample$pair_key
OAinPW_sample<-OAinPW_sample %>% filter(pair_key%in%Ipk)
OAinPW_sample<-droplevels(OAinPW_sample)

PW_papers<-semi_join(PW_papers,OAinPW_sample,by="pair_key") #this is what makes sure you are only sampling from PW journals for which there is OAinPW
# PW_papers$pair_key<-as.factor(PW_papers$pair_key)
nlevels(PW_papers$pair_key)
PW_papers<-droplevels(PW_papers)
nlevels(PW_papers$pair_key)
PW_papers<-arrange(PW_papers,pair_key)
# This is how many you are going to be sampling from each PW Journal.
# n is no. of articles in each OAinPW journal 
n<-as.integer(OAinPW_sample$n)

Prep_for_sampleOAinPW<-list(PW_papers,n)
return(Prep_for_sampleOAinPW)

}