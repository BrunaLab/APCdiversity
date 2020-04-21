samplePW<-function(OA_sample,PW_papers) {
  
  # Are you sure all the journals in PW df are represented 
  # in the OA df and vice versa?
  
  # FALSES would indicate "no"
  levels(PW_papers$pair_key)==levels(OA_sample$pair_key)
  
  # This will insure the journals are in both (using intersection
  # of pair_key in OA and PW)
  OA_pk<-OA_sample$pair_key
  PW_papers_pk<-PW_papers$pair_key
  Ipk<-intersect(PW_papers_pk,OA_pk)
  Ipk<-as_factor(Ipk)
  OA_sample$pair_key
  OA_sample<-OA_sample %>% filter(pair_key%in%Ipk)
  OA_sample<-droplevels(OA_sample)
  
  PW_papers<-semi_join(PW_papers,OA_sample,buy="pair_key") #this is what makes sure you are only sampling from PW journals for which there is OA
  PW_papers$pair_key<-as.factor(PW_papers$pair_key)
  nlevels(PW_papers$pair_key)
  PW_papers<-droplevels(PW_papers)
  nlevels(PW_papers$pair_key)
  PW_papers<-arrange(PW_papers,pair_key)
  
  # This is how many you are going to be sampling from each PW Journal.
  # It is the number of articles n in each OA journal 
  n = OA_sample$n
  
  # Now sample
  PW_sample<-PW_papers %>% 
    arrange(pair_key) %>% 
    nest() %>%            
    ungroup() %>% 
    mutate(n) %>%
    mutate(samp = map2(data, n, sample_n)) %>% 
    select(-data) %>%
    unnest(samp)
  summary(PW_sample)
  return(PW_sample)
  
  # To double check
  PW_sample_check<-PW_sample %>% 
    group_by(pair_key) %>% 
    summarize(sample_n=n())
  
  PW_sample_check<-full_join(PW_sample_check,OA_sample, by="pair_key")
  summary(PW_sample_check$sample_n==PW_sample_check$n)

}