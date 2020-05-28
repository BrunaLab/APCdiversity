Fig5e<-function(SampledData,OriginalData) {
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  # SampledData<-SubsampledPW.results_First
  # OriginalData<-AllData
  vars<-list(SampledData[1],OriginalData)
  SampledData<-as.data.frame(vars[1])
  OriginalData<-as.data.frame(vars[2])
  source("./Rscript/functions/DivRichCalc.R")
  OAdiv<-DivRichCalc(AllData,"author_last","OA")
  OArich_Last<-as.numeric(OAdiv[1])
  
  problast<-sum(SampledData$Richness>OArich_Last)/1000*100
  problast
  
  prich_last<-ggplot(SampledData, aes(x=Richness)) +
    geom_histogram(bins=30, colour="black", fill="white")+
    geom_vline(aes(xintercept=OArich_Last),
               color="darkblue", linetype="dashed", size=1)+
    annotate("text", x = 52, y = 130,label =(paste(problast,"%",sep="")))+
    # geom_label(label="Observed OA Richness(--%)", x=58,y=230,
    #            label.padding = unit(0.55, "lines"), # Rectangle size around label
    #            label.size = 0.5,color = "darkblue", fill="white")+
    xlab("Resampled national richness:\nLast authors of paywalled articles")+
    ylab("Frequency")+
    scale_y_continuous(expand = c(0,0),limits = c(0,150))+
    scale_x_continuous(breaks = seq(50,80, by=10),limits=c(50,80))
  prich_last<-prich_last+
    theme_classic()+ 
    theme(
      axis.text.x = element_text(size=18),
      axis.text.y = element_text(size=18),
      axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 24, hjust=0.5,),
      plot.margin =unit(c(1,1,1,1.5), "lines")  
    )
  prich_last
  return(prich_last)
  
}