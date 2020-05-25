Fig4<-function(bootstrap.OA.1st,OAdiv,OARich) {
    list(bootstrap.OA.1st,OAdiv,OARich)
    bootstrap.OA.1st<-as.data.frame(bootstrap.OA.1st)
    OAdiv<-as.numeric(OAdiv)
    OARich<-as.numeric(OARich)
    
    
    
    OAbootPlot_div<-ggplot(bootstrap.OA.1st, aes(x=InvSimp)) + 
      geom_histogram(bins=25, colour="black", fill="white")+
      geom_vline(aes(xintercept=OAdiv),
                 color="darkblue", linetype="dashed", size=1)+
      geom_label(label="Observed OA Diversity", x=17.5,y=185,
                 label.padding = unit(0.55, "lines"), # Rectangle size around label
                 label.size = 0.5,color = "darkblue", fill="white")+
      xlab("National Diversity: First Authors") + ylab("Frequency")+
      # xlab("Richness (No. of Countries)") + ylab("Frequency")+
      scale_x_continuous(expand = c(0,0),limits = c(10,20))
      scale_y_continuous(expand = c(0,0),limits = c(0,250))
    OAbootPlot_div<-OAbootPlot_div+
      theme_classic()+
      theme(
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
        axis.title.y=element_text(colour="black", size = 24, hjust=0.5,vjust=0.5,angle = 0),
        plot.margin =unit(c(1,1,1,1.5), "lines")  
      )
    
    

    OAbootPlot_rich<-ggplot(bootstrap.OA.1st, aes(x=Richness)) + 
      geom_histogram(bins=25, colour="black", fill="white")+
      geom_vline(aes(xintercept=OARich),
                 color="darkblue", linetype="dashed", size=1)+
      geom_label(label="Observed\nOA Richness", x=65,y=180,
                 label.padding = unit(0.55, "lines"), # Rectangle size around label
                 label.size = 0.5,color = "darkblue", fill="white")+
      xlab("No. of Countries: First Authors") + ylab("Frequency")+
      scale_x_continuous(expand = c(0,0),limits = c(50,70))
      scale_y_continuous(expand = c(0,0),limits = c(0,200))
    
    OAbootPlot_rich<-OAbootPlot_rich+
      theme_classic()+
      theme(
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        axis.title.x=element_text(colour="black", size = 24, vjust=-0.5),
        axis.title.y=element_text(colour="black", size = 24, hjust=0.5,vjust=0.5,angle = 90),
        plot.margin =unit(c(1,1,1,1.5), "lines")  
      )
    
    OAbootPlot<-list(OAbootPlot_div,OAbootPlot_rich)
    
    
  return(OAbootPlot)
  
}