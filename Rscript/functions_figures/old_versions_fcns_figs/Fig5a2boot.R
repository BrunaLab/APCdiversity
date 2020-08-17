Fig5a2boot<-function(SampledData,OriginalData) {
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(RColorBrewer)

  # SampledData<-SubsampledPW.results_First
  # OriginalData<-AllData
  # summary(as.factor(OriginalData$JrnlType))
  vars<-list(SampledData[1],OriginalData)
  SampledData<-as.data.frame(vars[1])
  OriginalData<-as.data.frame(vars[2])
  
  OA_papers <- OriginalData %>% 
    filter(JrnlType == "OA",AuthorNum==1) 
  
  source("./Rscript/functions/DivRichCalc.R") 
  nboot <-1000 #number of bootstrap samples
  Richness <-rep(NA, nboot)
  InvSimp <-rep(NA, nboot)
  bootstrap.OA.1st<-data.frame(Richness,InvSimp)
  rm(Richness,InvSimp)
  set.seed(10)
  for(i in 1:nboot){
    bootOA<-DivRichCalc(sample_n(OA_papers, nrow(OA_papers), replace = TRUE),"author_first","OA")
    bootstrap.OA.1st[i,1]<-bootOA[1]
    bootstrap.OA.1st[i,2]<-bootOA[2]
  }
  bootstrap.OA.1st<-arrange(bootstrap.OA.1st)
  bootstrap.OA.1st$author<-"author_first"
  bootstrap.OA.1st$JrnlType<-"OA"
  
  source("./Rscript/functions/DivRichCalc.R")
  OAdiv<-DivRichCalc(OriginalData,"author_first","OA")
  OADiv_First<-as.numeric(OAdiv[2])
  OADiv_First
  probFirst<-sum(SampledData$InvSimp>OADiv_First)/1000*100
  probFirst
  
  SampledData$JrnlType<-"PW"
  SampledData<-bind_rows(SampledData,bootstrap.OA.1st)
  
  
  
  pDiv_first<-
    ggplot(SampledData, aes(x=InvSimp,fill=JrnlType)) + 
    geom_histogram(bins=100,color="black", alpha=0.8, position = 'identity') +
    scale_fill_brewer(palette = "Paired")+
    geom_vline(aes(xintercept=OADiv_First),
               color="darkblue", linetype="dashed", size=1)+
    annotate("text", x = 14.2, y = 105,label =(paste(probFirst,"%",sep="")))+
    # geom_label(label="Observed OA Diversity (0%)", x=13,y=275,
    #            label.padding = unit(0.55, "lines"), # Rectangle size around label
    #            label.size = 0.5,color = "darkblue", fill="white")+
    xlab("Author Geographic Diversity:\nfirst authors")+
    ylab("Frequency")+
    # scale_x_continuous(breaks = c(6:16),limits=c(6:16))+
    scale_y_continuous(expand = c(0,0),limits = c(0,125))
  pDiv_first<-pDiv_first+
    theme_classic()+ 
    theme(
      axis.text.x = element_text(size=18),
      axis.text.y = element_text(size=18),
      axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 24, hjust=0.5,),
      plot.margin =unit(c(1,1,1,1.5), "lines")  
    )
  pDiv_first
  
  return(pDiv_first)
  
}