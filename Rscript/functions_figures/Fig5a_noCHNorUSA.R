Fig5a_noCHNorUSA<-function(SampledData,OriginalData) {
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  # SampledData<-SubsampledPW.results_First_NOUSACHN
  # AllData_noUSAorCHN
  vars<-list(SampledData,OriginalData)
  SampledData<-as.data.frame(vars[1])
  OriginalData<-as.data.frame(vars[2])
  
  source("./Rscript/functions/DivRichCalc.R")
  OAdiv<-DivRichCalc(OriginalData,"author_first","OA")
  OAdiv_First<-as.numeric(OAdiv[2])
  OAdiv_First
  probFirst<-sum(SampledData$InvSimp>OAdiv_First)/1000*100
  probFirst
  
  pDiv_first<-ggplot(SampledData, aes(x=InvSimp)) + 
    geom_histogram(bins=30, colour="black", fill="white")+
    geom_vline(aes(xintercept=OAdiv_First),
               color="darkblue", linetype="dashed", size=1)+
    annotate("text", x = 17, y = 275,label =(paste(probFirst,"%",sep="")))+
    # geom_label(label="Observed OA Diversity (0%)", x=13,y=275,
    #            label.padding = unit(0.55, "lines"), # Rectangle size around label
    #            label.size = 0.5,color = "darkblue", fill="white")+
    xlab("Resampled national diversity:\nfirst authors of paywalled articles")+
    ylab("Frequency")+
    scale_y_continuous(expand = c(0,0),limits = c(0,300))+
    scale_x_continuous(breaks = c(10:30),limits=c(10,30))
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