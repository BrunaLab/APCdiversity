samplePW<-function(PW_papers,n) {
  
  # n
  n<-unlist(n)
  n<-as.data.frame(n)
  # n1$pair_key<-as.factor(n1$pair_key)
  n<-n$n
  
 


  # Sample your PW papers
  PW_papers<-as.data.frame(PW_papers)
  PW_sample<-PW_papers %>% 
    group_by(pair_key) %>%
    arrange(pair_key) %>% 
    nest() %>%            
    ungroup() %>% 
    mutate(n) %>%
    # mutate(samp = map2(data, n, sample_n, replace=FALSE)) %>% # This is sampling WITHOUT replacement
    mutate(samp = map2(data, n, sample_n, replace=TRUE)) %>% # This is sampling WITH replacement
    select(-data) %>%
    unnest(samp)
  # n_distinct(PW_sample)
  
  PW_sample_check<-PW_sample %>% 
    group_by(pair_key) %>% 
    summarize(sample_n=n()) %>% 
    mutate(diff=sample_n-n) %>% 
    arrange(desc(diff))
  
  PWsampling<-list(PW_sample,PW_sample_check)
  return(PWsampling)

}
