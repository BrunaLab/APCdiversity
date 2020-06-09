samplePW<-function(PW_papers,n) {
  PW_papers<-as.data.frame(PW_papers)
  n<-unlist(n)
  n<-as.data.frame(n)
  n<-n$n
  PW_sample<-PW_papers %>% 
    group_by(pair_key) %>% 
    nest() %>%            
    ungroup() %>% 
    mutate(n) %>%
    # mutate(samp = map2(data, n, sample_n, replace=FALSE)) %>% # This is sampling WITHOUT replacement
    mutate(samp = map2(data, n, sample_n, replace=TRUE)) %>% # This is sampling WITH replacement
    select(-data) %>%
    unnest(samp)
  
  PW_sample_check<-PW_sample %>% 
    group_by(pair_key) %>% 
    summarize(sample_n=n())
  PW_sample_check<- (PW_sample_check$sample_n==n)
  
  PWsampling<-list(PW_sample,PW_sample_check)
  return(PWsampling)

}