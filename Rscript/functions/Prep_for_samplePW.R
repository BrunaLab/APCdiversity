Prep_for_samplePW<-function(AllData) {
# # df of all articles in PW journals
# FirstAuthPW<-AllData %>% 
#   filter(Country != "NA" & Code != "NA") %>%
#   filter(JrnlType=="PW") %>% 
#   filter(AuthorNum==1)
# FirstAuthPW$Journal<-as.factor(FirstAuthPW$Journal)
# FirstAuthPW$Journal<-droplevels(FirstAuthPW$Journal)
# 
# # df of all articles in OA journals
# FirstAuthOA<-AllData %>% 
#   filter(Country != "NA" & Code != "NA") %>%
#   filter(JrnlType=="OA") %>% 
#   filter(AuthorNum==1)
# FirstAuthOA$Journal<-as.factor(FirstAuthOA$Journal)
# FirstAuthOA$Journal<-droplevels(FirstAuthOA$Journal)
# 
# # number of journals, make sure they are the same!
# nlevels(FirstAuthPW$Journal)==nlevels(FirstAuthOA$Journal)

# How to sample different amounts from different groups is very nicely laid out in this post:
# https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html

# create a df telling you how many articles published in each OA journal
# will sample in this amount from the df of PW articles
OA_sample <- AllData %>%
  filter(JrnlType == "OA") %>% 
  group_by(pair_key,Journal) %>% 
  summarize(n=n_distinct(DOI))%>% 
  arrange(Journal) %>% 
  ungroup() %>% 
  select(pair_key,n)
OA_sample$pair_key<-as.factor(OA_sample$pair_key)
nlevels(OA_sample$pair_key)
OA_sample$pair_key<-droplevels(OA_sample$pair_key)
nlevels(OA_sample$pair_key)

PW_papers<- AllData %>%
  filter(JrnlType == "PW") %>% 
  group_by(pair_key,Journal,DOI) %>% 
  slice(1) %>% 
  select(DOI,Journal,JrnlType,pair_key)
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
# in the OA df and vice versa? FALSES would indicate "no"
summary(levels(PW_papers$pair_key)==levels(OA_sample$pair_key))

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

PW_papers<-semi_join(PW_papers,OA_sample,by="pair_key") #this is what makes sure you are only sampling from PW journals for which there is OA
# PW_papers$pair_key<-as.factor(PW_papers$pair_key)
nlevels(PW_papers$pair_key)
PW_papers<-droplevels(PW_papers)
nlevels(PW_papers$pair_key)
PW_papers<-arrange(PW_papers,pair_key)
# This is how many you are going to be sampling from each PW Journal.
# n is no. of articles in each OA journal 
n<-as.integer(OA_sample$n)

Prep_for_samplePW<-list(PW_papers,n)
return(Prep_for_samplePW)

}