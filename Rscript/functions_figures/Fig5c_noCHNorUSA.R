Fig5c_noCHNorUSA<-function(SampledData,OriginalData) {
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  # SampledData<-SubsampledPW.results_All_NOUSACHN
  # OriginalData<-AllData_noUSAorCHN
  vars<-list(SampledData,OriginalData)
  SampledData<-as.data.frame(vars[1])
  OriginalData<-as.data.frame(vars[2])
  
  source("./Rscript/functions/DivRichCalc.R")
  OAdiv<-DivRichCalc(OriginalData,"author_all","OA")
  OAdiv_All<-as.numeric(OAdiv[2])
  OAdiv_All
  probAll<-sum(SampledData$InvSimp>OAdiv_All)/1000*100
  probAll
  
  pDiv_all<-ggplot(SampledData, aes(x=InvSimp)) + 
    geom_histogram(bins=30, colour="black", fill="white")+
    geom_vline(aes(xintercept=OAdiv_All),
               color="darkblue", linetype="dashed", size=1)+
    annotate("text", x = 15, y = 140,label =(paste(probAll,"%",sep="")))+
    # geom_label(label="0% bootstrap PW values >\nObserved OA Diversity",
    #            x=11.5,y=135,label.padding = unit(0.55, "lines"), # Rectangle size around label
    #            label.size = 0.5,color = "darkblue", fill="white")+
    xlab("Resampled national diversity:\nAll authors of paywalled articles")+
    ylab("Frequency")+
    scale_x_continuous(breaks = c(10:30),limits=c(10,30))+
    scale_y_continuous(expand = c(0,0),limits = c(0,200))
  pDiv_all<-pDiv_all+
    theme_classic()+ 
    theme(
      axis.text.x = element_text(size=18),
      axis.text.y = element_text(size=18),
      axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 24, hjust=0.5),
      plot.margin =unit(c(1,1,1,1.5), "lines")  
    )
  pDiv_all
  return(pDiv_all)
  
}