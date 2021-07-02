Fig5d_noCHNorUSA<-function(SampledData,OriginalData) {
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  # SampledData<-SubsampledPW.results_First
  # OriginalData<-AllData
  vars<-list(SampledData[1],OriginalData)
  SampledData<-as.data.frame(vars[1])
  OriginalData<-as.data.frame(vars[2])
  
  source("./Rscript/functions/DivRichCalc.R")
  OAdiv<-DivRichCalc(AllData,"author_first","OA")
  OArich_First<-as.numeric(OAdiv[1])
  
  probFirst<-sum(SampledData$Richness>OArich_First)/1000*100
  
  prich_first<-ggplot(SampledData, aes(x=Richness)) +
    geom_histogram(bins=30, colour="black", fill="white")+
    annotate("text", x = 67, y = 190,label =(paste(probFirst,"%",sep="")))+
    geom_vline(aes(xintercept=OArich_First),
               color="darkblue", linetype="dashed", size=1)+
    # geom_label(label="Observed OA Richness (--%)", x=65,y=240,
    #            label.padding = unit(0.55, "lines"), # Rectangle size around label
    #            label.size = 0.5,color = "darkblue", fill="white")+
    xlab("Author Geographic Richness")+
    ylab("Frequency")+
    ggtitle("D) first authors")+
    # scale_x_continuous(breaks = seq(40,80, by=10),limits=c(40,80))
    scale_y_continuous(expand = c(0,0),limits = c(0,200))+
    scale_x_continuous(breaks = seq(50,75, by=5),limits=c(50,75))
  prich_first<-prich_first+
    theme_classic()+ 
    theme(
      axis.text.x = element_text(size=18),
      axis.text.y = element_text(size=18),
      axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 24, hjust=0.5,),
      plot.title = element_text(colour="black", size = 24, vjust=3),
      plot.margin =unit(c(1,1,1,1.5), "lines")  
    )
  prich_first
  
  return(prich_first)
  
}