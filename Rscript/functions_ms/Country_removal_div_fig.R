Country_removal_div_fig<-function(){


Plot_PW_div<-read_csv("./data_clean/div_remove_one_country.csv")
plot_PW_div_noCHN<-read_csv("./data_clean/div_remove_second_country.csv")

# effect of removing each country
p1<-ggplot(Plot_PW_div, 
           aes(x=Code, 
               y=Change_in_Div,
               color=`Effect on Diversity`,
               fill=`Effect on Diversity`)) +
  geom_bar(stat="identity")+
  scale_fill_manual("Effect on Diversity", 
                    values = c("None" = "white", 
                               "Significant Decrease" = "gray", 
                               "Significant Increase" = "black"))+
  scale_color_manual("Effect on Diversity", 
                     values = c("None" = "black", 
                                "Significant Decrease" = "black", 
                                "Significant Increase" = "black"))


textcol_first <- 
  ifelse(Plot_PW_div$`Effect on Diversity` == "None", 'white', 'black')

p1<-p1+
  labs(y = "Change in Geographic Diversity",
       x = "Country Removed from the Dataset")+
  theme_classic()+
  theme(
    axis.text.x = element_text(size=16,angle = 90, color=textcol_first),
    axis.text.y = element_text(size=16),
    axis.title.x=element_text(colour="black", size = 20, vjust=-0.5),
    axis.title.y=element_text(colour="black", size = 20, hjust=0.5,),
    legend.title = element_text(colour="black", size = 20),
    legend.text = element_text(colour="black", size = 14),
    legend.position = ("top"),
    plot.margin =unit(c(1,1,1,1.5), "lines")
  )
p1


# effect of removing a country in addition to China


p2<-ggplot(plot_PW_div_noCHN, 
           aes(x=Code, 
               y=Change_in_Div,
               color=`Effect on Diversity`,
               fill=`Effect on Diversity`)) +
  geom_bar(stat="identity")+
  
  scale_fill_manual("Effect on Diversity", 
                    values = c("None" = "white", 
                               "Significant Decrease" = "gray", 
                               "Significant Increase" = "black"))+
  scale_color_manual("Effect on Diversity", 
                     values = c("None" = "black", 
                                "Significant Decrease" = "black", 
                                "Significant Increase" = "black"))


textcol_second <- 
  ifelse(plot_PW_div_noCHN$`Effect on Diversity` == "None", 'white', 'black')

p2<-p2+
  labs(y = "Change in Geographic Diversity",
       x = "Second Country Removed from the Dataset (after China)")+
  theme_classic()+
  theme(
    axis.text.x = element_text(size=16,angle = 90, color=textcol_second),
    axis.text.y = element_text(color = "black", size=16),
    axis.title.x=element_text(color = "black", size = 18, vjust=-0.5),
    legend.title = element_text(color = "black", size=20),
    legend.text = element_text(color = "black", size = 16),
    legend.position = ("none"),
    plot.margin =unit(c(1,1,1,1.5), "lines")  
    )
p2

return(list(p1,p2))

}