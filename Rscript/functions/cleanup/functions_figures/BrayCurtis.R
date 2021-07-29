BrayCurtis<-function(data) {
library(tidyverse)
library(vegan)
library(egg)



#######
#Fig
#######

data$authors <- ordered(data$authors,levels = c("Single Authors","First Authors"))

# data$dataset <- factor(label_data$author,levels = c("solo","author_first"))

Fig<-ggplot(data, aes(x=cat,y=bray)) +
  geom_boxplot(outlier.colour="black", 
               fill="white",
               position="dodge",
               notch=FALSE)+
  ylab("Bray-Curtis Dissimilarity") + 
  xlab("Comparison")+
  facet_grid(cols = vars(dataset), 
             rows=vars(authors))



Fig<-Fig+theme_classic()+
  theme(
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=20),
    axis.title.x=element_text(colour="black", size = 25, vjust=-0.5),
    axis.title.y=element_text(colour="black", size = 25, vjust=2),
    strip.text.x = element_text(size = 30,margin = margin(5,0,3,0, "lines")),
    strip.text.y = element_text(size = 30, angle=0),
    strip.background.x = element_rect(fill = NA, colour = NA),
    strip.background.y = element_rect(fill = NA, colour = NA),
    panel.spacing.x =unit(4, "lines"), 
    panel.spacing.y=unit(4,"lines"),
    legend.position = "none",
    legend.text = element_text(size=20)
    # plot.margin =unit(c(3,1,1,1.5), "lines")   #plot margin - top, right, bottom, left
  )    
Fig


facet_labels<-c("A","B","C","D")
# Fig<-tag_facet(Fig,open="", close="", tag_pool=facet_labels,vjust=0.5,hjust=-1,size=10)
Fig<-tag_facet(Fig,open="", close="", tag_pool=facet_labels,vjust=0,hjust=-1,size=10)
Fig<-tag_facet(Fig)

return(Fig)




}



