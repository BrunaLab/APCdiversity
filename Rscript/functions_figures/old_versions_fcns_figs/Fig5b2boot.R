Fig5b2boot<-function(SampledData,OriginalData) {
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(RColorBrewer)
  
  # SampledData<-SubsampledPW.results_Last
  # OriginalData<-AllData
  # summary(as.factor(OriginalData$JrnlType))
  vars<-list(SampledData[1],OriginalData)
  SampledData<-as.data.frame(vars[1])
  OriginalData<-as.data.frame(vars[2])
  
  OA_papers <- OriginalData %>% 
    group_by(DOI) %>%
    filter(AuthorNum == max(AuthorNum))
  

  
  source("./Rscript/functions/DivRichCalc.R") 
  nboot <-1000 #number of bootstrap samples
  Richness <-rep(NA, nboot)
  InvSimp <-rep(NA, nboot)
  bootstrap.OA.last<-data.frame(Richness,InvSimp)
  rm(Richness,InvSimp)
  set.seed(10)
  for(i in 1:nboot){
    bootOA<-DivRichCalc(sample_n(OA_papers, nrow(OA_papers), replace = TRUE),"author_last","OA")
    bootstrap.OA.last[i,1]<-bootOA[1]
    bootstrap.OA.last[i,2]<-bootOA[2]
  }
  bootstrap.OA.last<-arrange(bootstrap.OA.last)
  bootstrap.OA.last$author<-"author_last"
  bootstrap.OA.last$JrnlType<-"OA"
  
  source("./Rscript/functions/DivRichCalc.R")
  OAdiv<-DivRichCalc(OriginalData,"author_last","OA")
  OADiv_Last<-as.numeric(OAdiv[2])
  OADiv_Last
  problast<-sum(SampledData$InvSimp>OADiv_Last)/1000*100
  problast<-round(problast,1)
  problast
  
  SampledData$JrnlType<-"PW"
  SampledData<-bind_rows(SampledData,bootstrap.OA.last)
  
  
  
  pDiv_last<-ggplot(SampledData, aes(x=InvSimp,fill=JrnlType)) + 
    geom_histogram(bins=100,color="black", alpha=0.8, position = 'identity') +
    scale_fill_brewer(palette = "Paired")+
    geom_vline(aes(xintercept=OADiv_Last),
               color="darkblue", linetype="dashed", size=1)+
    annotate("text", x = 6.5, y = 150,label =(paste(problast,"%",sep="")))+
    # geom_label(label="96% bootstrap PW values >\nObserved OA Diversity",
    #            x=8.5,y=135,label.padding = unit(0.55, "lines"), # Rectangle size around label
    #            label.size = 0.5,color = "darkblue", fill="white")+
    xlab("Author Geographic Diversity:\nLast authors")+
    ylab("Frequency")+
    scale_x_continuous(breaks = c(6:19),limits=c(6,19))+
    scale_y_continuous(expand = c(0,0),limits = c(0,115))
  pDiv_last<-pDiv_last+
    theme_classic()+ 
    theme(
      axis.text.x = element_text(size=18),
      axis.text.y = element_text(size=18),
      axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 24, hjust=0.5),
      plot.margin =unit(c(1,1,1,1.5), "lines")  
    )
  pDiv_last
  
  
  
  
  return(pDiv_last)
  
}