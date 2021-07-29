sampleOAinPW<-function(PW_papers,n) {
  # PW_papers<-OAinPW_papers
  # n<-mirrorOA_sample_n
  PW_papers<-PW_papers %>% 
    ungroup() %>% 
    as_tibble(PW_papers)
  # 
  # PW_papers_refID<-PW_papers %>% 
  #   ungroup() %>% 
  #   select(refID) %>% 
  #   group_by(refID) %>% 
  #   slice(1)
  # n<-unlist(n)
  # n<-as.data.frame(n)
  # n<-n$n
    # PW_sample<-PW_papers %>% 
    # group_by(pair_key) %>% 
    # nest() %>%            
    # ungroup() %>% 
    # mutate(n) %>%
    # # mutate(samp = map2(data, n, sample_n, replace=FALSE)) %>% # This is sampling WITHOUT replacement
    # mutate(samp = map2(data, n, sample_n, replace=TRUE)) %>% # This is sampling WITH replacement
    # select(-data) %>%
    # unnest(samp)
  PW_sample<-PW_papers %>% 
    ungroup() %>% 
    sample_n(n,replace=TRUE)
  PW_sample_n_refID<-PW_sample %>% summarize(n_distinct(refID))
  PW_sample_n<-nrow(PW_sample)
  PW_sample_check<- (PW_sample_n==n)
  PWsampling<-list(PW_sample,PW_sample_check)
  return(PWsampling)

}