permute_OA<-function(OA_paper_pool,n,sampling_type) {
  # sampling_type<-"perm"
  
  OA_paper_pool<-as.data.frame(OA_paper_pool)
 
  
  #TODO: INSERT OSMEWTHING HERE TO CHECK IF THE pair_keys are in both and if enough n if not tell you
  
  if ((sampling_type=="perm")==TRUE) {
    OA_sample<-OA_paper_pool %>% 
      group_by(pair_key) %>% 
      arrange(pair_key) %>% 
      nest() %>%            
      ungroup() %>% 
      mutate(n) %>%
      mutate(samp = map2(data, n, sample_n, replace=FALSE)) %>% # This is sampling WITHOUT replacement
      select(-data) %>%
      unnest(samp)
  } else if ((sampling_type=="boot")==TRUE) {
    OA_sample<-OA_paper_pool %>% 
      arrange(pair_key) %>% 
      group_by(pair_key) %>% 
      nest() %>%            
      ungroup() %>% 
      mutate(n) %>%
      mutate(samp = map2(data, n, sample_n, replace=TRUE)) %>% # This is sampling WITH replacement
      select(-data) %>%
      unnest(samp)
  } else {
    stop("error in bootstrap/permutation type selection")
  }
  
 
  
  suppressMessages(OA_sample_check<-OA_sample %>% 
    group_by(pair_key) %>% 
    summarize(sample_n=n()))
  OA_sample_check<- (OA_sample_check$sample_n==n)
  
  OA_sampling<-list(OA_sample,OA_sample_check)
  return(OA_sampling)

}