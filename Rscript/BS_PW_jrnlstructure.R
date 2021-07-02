


  OA_sample <- Dataset %>%
    filter(ArticleType == "PW") %>% 
  group_by(pair_key) %>% 
    summarize(n=n_distinct(refID))%>% 
    arrange(pair_key) %>% 
    ungroup() %>% 
    select(pair_key,n)
  # sum(OA_sample$n)
  # OA_sample$Journal<-as.factor(OA_sample$Journal)
  OA_sample$pair_key<-as.factor(OA_sample$pair_key)
  nlevels(OA_sample$pair_key)
  # OA_sample$Journal<-droplevels(OA_sample$Journal)
  OA_sample$pair_key<-droplevels(OA_sample$pair_key)
  nlevels(OA_sample$pair_key)
  
  PW_papers<- Dataset %>%
    filter(ArticleType == "PW") %>% 
    group_by(pair_key,Journal,refID) %>% 
    slice(1) %>% 
    select(refID,Journal,JrnlType,pair_key)
  # summarize(n=n_distinct(DOI)) %>% 
  # arrange(Journal) %>% 
  # ungroup() %>% 
  # select(pair_key,n)
  PW_papers$Journal<-as.factor(PW_papers$pair_key)
  nlevels(PW_papers$pair_key)
  PW_papers$pair_key<-droplevels(PW_papers$pair_key)
  nlevels(PW_papers$pair_key)
  # STEP 1
  # Are you sure all the journals in PW df are represented 
  # in the OA df and vice versa? FALSES would indicate "no"
  summary(levels(OA_sample$pair_key)==levels(PW_papers$pair_key))
  
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
  nlevels(PW_papers$Journal)
  nlevels(PW_papers$pair_key)
  PW_papers<-droplevels(PW_papers)
  nlevels(PW_papers$Journal)
  PW_papers<-arrange(PW_papers,pair_key,Journal)
  # This is how many you are going to be sampling from each PW Journal.
  # n is no. of articles in each OA journal 
  n<-as.integer(OA_sample$n)
  
  
  n
  PW_papers
  source("./Rscript/functions/samplePW.R")
  PW_sampling<-samplePW(PW_papers,n)
  PW_sample<-PW_sampling[1]
  PW_sample_check<-PW_sampling[2]
  ################
  
  ############################################
  # now sample and iterate!
  # THIS IS FOR MATCHED SUBSAMPLING - FIRST AUTHOR
  # 1<-Richness  2<-InvSimpsons 3<-Countries
  ############################################
  source("./Rscript/functions/samplePW.R")
  source("./Rscript/functions/DivRichCalc.R")
  # source("./Rscript/functions/DivCalcPooledFirst.R")
  PW_papers<-as.data.frame(PW_papers)
  n<-unlist(n)
  n<-as.data.frame(n)
  n<-n$n
  
  
  nboot <-2000 #number of bootstrap samples
  InvSimp <-rep(NA, nboot)
  Richness<-rep(NA, nboot)
  Countries<-rep(NA, nboot)
  Shannon<-rep(NA, nboot)
  Even<-rep(NA, nboot)
  replicate<-data.frame(replicate=as.numeric())
  
  SubsampledPW.results_First <- data.frame(Richness,InvSimp,Shannon,Even)
  # replicate<-data.frame(replicate)
  
  rm(InvSimp,Richness,Shannon,Even)
  set.seed(1)
  for(i in 1:nboot){
    
    
    
    
    PW_sample<-samplePW(PW_papers,n)
    # The first element of the list is the list of papers. 
    # Need to take that list and Dataset to get back all the 
    # papers with all their authors.
    PW_sample<-as.data.frame(PW_sample[1])
    PW_sample_refID<-PW_sample$refID
    PW_sample<-Dataset %>% filter(refID%in%PW_sample_refID)
    # PW_sample %>% summarize(n_distinct(DOI)) #confirm the number of articles
    PW_sample<-as.data.frame(PW_sample)
    AuPosition<-AuPosition
    JrnlType<-"PW"
    ArticleType<-"PW"
    results<-DivRichCalc(PW_sample,AuPosition,JrnlType,ArticleType)
    SubsampledPW.results_First[i,1]<-(results)[1]
    SubsampledPW.results_First[i,2]<-(results)[2]
    SubsampledPW.results_First[i,3]<-(results)[4]
    SubsampledPW.results_First[i,4]<-(results)[5]
    Countries[i]<-(results)[3]
    
    # nCountries[i,1]<-i
    # nCountries[i,2]<-(nrow(Countries[[i]]))
    # count<-data.frame(replicate=rep(nCountries[i,1], each=nCountries[i,2]))
    count<-data.frame(replicate=rep(i, each=SubsampledPW.results_First[i,1]))
    replicate<- bind_rows(replicate,count)
    # replicate<-bind_rows(replicate,count)
    # bind_rows(counter,counter=rep(i, each=nCountries))
    # nCountries[i]<-nrow(Countries[[i]])
    
  }
  
  SubsampledPW.results_First$author<-AuPosition
  STRUCTURED_BS<-SubsampledPW.results_First
  STRUCTURED_BS
  ##############################################
  ##############################################
  ##############################################
  ##############################################
  ############################################
  # now sample and iterate!
  # THIS IS FOR MATCHED SUBSAMPLING - FIRST AUTHOR
  # 1<-Richness  2<-InvSimpsons 3<-Countries
  ############################################
  source("./Rscript/functions/samplePW.R")
  source("./Rscript/functions/DivRichCalc.R")
  # source("./Rscript/functions/DivCalcPooledFirst.R")
  PW_papers<-as.data.frame(PW_papers)
  n<-unlist(n)
  n<-as.data.frame(n)
  n<-n$n
  
  
  # nboot <-10 #number of bootstrap samples
  InvSimp <-rep(NA, nboot)
  Richness<-rep(NA, nboot)
  Countries<-rep(NA, nboot)
  Shannon<-rep(NA, nboot)
  Even<-rep(NA, nboot)
  replicate<-data.frame(replicate=as.numeric())
  
  SubsampledPW.results_First <- data.frame(Richness,InvSimp,Shannon,Even)
  # replicate<-data.frame(replicate)
  
  rm(InvSimp,Richness,Shannon,Even)
  set.seed(1)
  for(i in 1:nboot){
    
    
    
    
    # PW_sample<-samplePW(PW_papers,n)
    PW_sample<-sample(PW_papers$refID,sum(n),replace = TRUE)
    
    # The first element of the list is the list of papers. 
    # Need to take that list and Dataset to get back all the 
    # papers with all their authors.
    PW_sample<-as.data.frame(PW_sample)
    names(PW_sample)<-"refID"
    head(PW_sample)
    PW_sample_refID<-PW_sample$refID
    PW_sample<-Dataset %>% filter(refID%in%PW_sample_refID)
    # PW_sample %>% summarize(n_distinct(DOI)) #confirm the number of articles
    PW_sample<-as.data.frame(PW_sample)
    AuPosition<-AuPosition
    JrnlType<-"PW"
    ArticleType<-"PW"
    results<-DivRichCalc(PW_sample,AuPosition,JrnlType,ArticleType)
    SubsampledPW.results_First[i,1]<-(results)[1]
    SubsampledPW.results_First[i,2]<-(results)[2]
    SubsampledPW.results_First[i,3]<-(results)[4]
    SubsampledPW.results_First[i,4]<-(results)[5]
    Countries[i]<-(results)[3]
    
    # nCountries[i,1]<-i
    # nCountries[i,2]<-(nrow(Countries[[i]]))
    # count<-data.frame(replicate=rep(nCountries[i,1], each=nCountries[i,2]))
    count<-data.frame(replicate=rep(i, each=SubsampledPW.results_First[i,1]))
    replicate<- bind_rows(replicate,count)
    # replicate<-bind_rows(replicate,count)
    # bind_rows(counter,counter=rep(i, each=nCountries))
    # nCountries[i]<-nrow(Countries[[i]])
    
  }
  
  SubsampledPW.results_First$author<-AuPosition
  NO_STRUCTURE_BS<-SubsampledPW.results_First
  NO_STRUCTURE_BS
  ##############################################
  ##############################################
  ##############################################
  ##############################################
  hist(STRUCTURED_BS$InvSimp)
  hist(NO_STRUCTURE_BS$InvSimp)
  mean(STRUCTURED_BS$InvSimp)
  mean(NO_STRUCTURE_BS$InvSimp)
  # 
  # ########################################
  # # CALC AND SAVE A DF OF THE COUNTRIES SAMPLED
  # # EACH RUN
  # ########################################
  # 
  # 
  # # # Use the AllData df to get the basic info on each country (code, region, etc)
  # # CountryInfo<-AllData %>% 
  # #   select(First_Author_Country,Region,IncomeGroup,Code) %>% 
  # #   group_by(Code) %>% 
  # #   slice(1)
  # # # head(CountryInfo,10)
  # # 
  # # 
  # # Countries<-bind_rows(Countries)
  # # Countries<-bind_cols(Countries,replicate)
  # # Subsampled_Countries<-as.data.frame(Countries)
  # # # str(Subsampled_Countries)
  # # Subsampled_Countries<-Subsampled_Countries %>% 
  # #   inner_join(CountryInfo,Subsampled_Countries,by="Code") 
  # # # head(Subsampled_Countries,10)
  # # Subsampled_Countries$IncomeGroup<-as.factor(Subsampled_Countries$IncomeGroup) 
  # # Subsampled_Countries$Region<-as.factor(Subsampled_Countries$Region) 
  # # Subsampled_Countries$Code<-as.factor(Subsampled_Countries$Code) 
  # # Subsampled_Countries$Country<-as.factor(Subsampled_Countries$First_Author_Country) 
  # # Subsampled_Countries$author<-AuPosition
  # 
  # 
  # CountryInfo<-read_csv("./data_clean/CountryData.csv")
  # Countries<-bind_rows(Countries)
  # Countries<-bind_cols(Countries,replicate)
  # Subsampled_Countries<-as.data.frame(Countries)
  # # str(Subsampled_Countries)
  # Subsampled_Countries<-Subsampled_Countries %>% 
  #   inner_join(CountryInfo,Subsampled_Countries,by="Code") 
  # # head(Subsampled_Countries,10)
  # Subsampled_Countries$IncomeGroup<-as.factor(Subsampled_Countries$IncomeGroup) 
  # Subsampled_Countries$Region<-as.factor(Subsampled_Countries$Region) 
  # Subsampled_Countries$Code<-as.factor(Subsampled_Countries$Code)
  # library(countrycode)
  # Subsampled_Countries$Country<-countrycode(Subsampled_Countries$Code,origin = 'iso3c',destination ="country.name")
  # Subsampled_Countries$author<-AuPosition
  # 
  # 
  # 
  # 
  # SubsampledPW.results_First$Dataset<-deparse(substitute(Dataset)) #adds the name of the dataset (All Data or No China USA as chr)
  # SubsampledPW.results_First$Dataset<-gsub("AllData_noUSAorCHN","Without CHN & USA",SubsampledPW.results_First$Dataset)
  # SubsampledPW.results_First$Dataset<-gsub("AllData","All Countries",SubsampledPW.results_First$Dataset)
  # Subsampled_Countries$Dataset<-deparse(substitute(Dataset)) #adds the name of the dataset (All Data or No China USA as chr)
  # Subsampled_Countries$Dataset<-gsub("AllData_noUSAorCHN","Without CHN & USA",Subsampled_Countries$Dataset)
  # Subsampled_Countries$Dataset<-gsub("AllData","All Countries",Subsampled_Countries$Dataset)
  # SubsampledPW.results<-list(SubsampledPW.results_First,Subsampled_Countries)
  # 
  # 
  # 
