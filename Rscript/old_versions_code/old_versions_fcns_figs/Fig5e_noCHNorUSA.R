Fig5e_noCHNorUSA<-function(SampledData,OriginalData) {
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
    annotate("text", x = 55, y = 160,label =(paste(problast,"%",sep="")))+
    # geom_label(label="Observed OA Richness(--%)", x=58,y=230,
    #            label.padding = unit(0.55, "lines"), # Rectangle size around label
    #            label.size = 0.5,color = "darkblue", fill="white")+
    xlab("Author Geographic Richness")+
    ylab("Frequency")+
    ggtitle("E) last authors")+
    scale_y_continuous(expand = c(0,0),limits = c(0,170))+
    scale_x_continuous(breaks = seq(50,75, by=5),limits=c(50,75))
  prich_last<-prich_last+
    theme_classic()+ 
    theme(
      axis.text.x = element_text(size=18),
      axis.text.y = element_text(size=18),
      axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 24, hjust=0.5,),
      plot.title = element_text(colour="black", size = 24, vjust=3),
      plot.margin =unit(c(1,1,1,1.5), "lines")  
    )
  prich_last
  return(prich_last)
  
}