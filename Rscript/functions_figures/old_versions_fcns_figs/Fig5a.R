Fig5a<-function(SampledData,OriginalData) {
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  # SampledData<-SubsampledPW.results_First
  # OriginalData<-AllData
  vars<-list(SampledData[1],OriginalData)
  SampledData<-as.data.frame(vars[1])
  OriginalData<-as.data.frame(vars[2])
  
  source("./Rscript/functions/DivRichCalc.R")
  OAdiv<-DivRichCalc(OriginalData,"author_first","OA")
  OADiv_First<-as.numeric(OAdiv[2])
  OADiv_First
  probFirst<-sum(SampledData$InvSimp>OADiv_First)/1000*100
  probFirst
  
  pDiv_first<-ggplot(SampledData, aes(x=InvSimp)) + 
    geom_histogram(bins=30, colour="black", fill="white")+
    geom_vline(aes(xintercept=OADiv_First),
               color="darkblue", linetype="dashed", size=1)+
    annotate("text", x = 14.5, y = 240,label =(paste(probFirst,"%",sep="")))+
    # geom_label(label="Observed OA Diversity (0%)", x=13,y=275,
    #            label.padding = unit(0.55, "lines"), # Rectangle size around label
    #            label.size = 0.5,color = "darkblue", fill="white")+
    xlab("Author Geographic Diversity")+
    ylab("Frequency")+
    ggtitle("A) first authors")+
    scale_x_continuous(breaks = seq(6,16, by=1),limits=c(6,16))+
    scale_y_continuous(expand = c(0,0),limits = c(0,250))
  pDiv_first<-pDiv_first+
    theme_classic()+ 
    theme(
      axis.text.x = element_text(size=18),
      axis.text.y = element_text(size=18),
      axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
      axis.title.y=element_text(colour="black", size = 24, hjust=0.5,),
      plot.title = element_text(colour="black", size = 24, vjust=3),
      plot.margin =unit(c(1,1,1,1.5), "lines")  
    )
  pDiv_first
  
  return(pDiv_first)
  
}