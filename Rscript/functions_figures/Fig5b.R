Fig5b<-function(SampledData,OriginalData) {
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  # SampledData<-SubsampledPW.results_First
  # OriginalData<-AllData
  vars<-list(SampledData,OriginalData)
  SampledData<-as.data.frame(vars[1])
  OriginalData<-as.data.frame(vars[2])
  
  source("./Rscript/functions/DivRichCalc.R")
  OAdiv<-DivRichCalc(AllData,"author_last","OA")
  OAdiv_Last<-as.numeric(OAdiv[2])
  
  
  div_Last<-sum(SampledData$InvSimp>OAdiv_Last)/1000*100
  div_Last
  
  
  pDiv_last<-ggplot(SubsampledPW.results_Last, aes(x=InvSimp)) + 
    geom_histogram(bins=30, colour="black", fill="white")+
    geom_vline(aes(xintercept=OAdiv_Last),
               color="darkblue", linetype="dashed", size=1)+
    annotate("text", x = 6.5, y = 175,label =(paste(div_Last,"%",sep="")))+
    # geom_label(label="96% bootstrap PW values >\nObserved OA Diversity",
    #            x=8.5,y=135,label.padding = unit(0.55, "lines"), # Rectangle size around label
    #            label.size = 0.5,color = "darkblue", fill="white")+
    xlab("Resampled national diversity:\nLast authors of paywalled articles")+
    ylab("Frequency")+
    scale_x_continuous(breaks = c(6:11),limits=c(6,11))+
    scale_y_continuous(expand = c(0,0),limits = c(0,200))
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