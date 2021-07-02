GeoFig<-function(DataSet,AuPosition) {
  # DataSet<-AllData
  # AuPosition<-"author_first"
  # AuPosition<-"solo"
  vars<-list(DataSet,AuPosition)
  #   
    if ((vars[2]=="author_first")==TRUE) {
    
      coauthored_pubs<-AllData %>%
        group_by(refID) %>%
        summarize(n=n_distinct(AuthorNum)) %>%
        filter(n>=2) %>%
        select(-n)
      AllGeo <-AllData %>% 
        filter(refID %in% coauthored_pubs$refID) 
      
  } else if ((vars[2]=="solo")==TRUE) {
    
    solo_pubs<-AllData %>%
      group_by(refID) %>%
      summarize(n=n_distinct(AuthorNum)) %>%
      filter(n==1) %>%
      select(-n)
    AllGeo <-AllData %>% 
      filter(refID %in% solo_pubs$refID) 
    
  } else {
    stop("Please enter 'author_first' or 'solo' for b")
    
  }

# This takes the codes and converts them to names    
library(countrycode)
  AllGeo$Code<-
    countrycode(AllGeo$Code, origin = 'iso3c', 
                destination = 'country.name')
  
  
    AllGeo<-AllGeo %>% 
      filter(First_Author_Country != "NA" & Code != "NA") %>%
      ungroup() %>% 
      select(First_Author_Country, Code,Region,IncomeGroup, JrnlType) %>%
      group_by(JrnlType) %>% 
      add_count(Code) %>% 
      group_by(JrnlType,Code) %>% 
      slice(1) %>%
      arrange(desc(n)) %>% 
      filter(IncomeGroup != "NA") %>%
      ungroup() %>%
      group_by(JrnlType) %>% 
      mutate(perc=n/sum(n)*100)
    AllGeo %>% group_by(JrnlType) %>% summarize(sum(perc))

    
    AllGeo$perc<-round(AllGeo$perc,2)
    AllGeo<-AllGeo %>% 
      arrange(JrnlType,desc(perc)) %>% 
      group_by(JrnlType) %>% 
      transform(Code = reorder(Code,perc))
    AllGeo$IncomeGroup<-as.factor(AllGeo$IncomeGroup)
    AllGeo$IncomeGroup<-droplevels(AllGeo$IncomeGroup)
    levels(AllGeo$IncomeGroup)
    
    cutoff = 25 # This is how many countries you want on the chart, all the rest will be in "OTHER"
    AllGeo2<-AllGeo %>% 
      arrange(JrnlType,desc(perc)) %>%
      select(Code,n,perc, IncomeGroup,JrnlType)
    most.common.authors<-AllGeo2 %>%
      group_by(JrnlType) %>% 
      slice(1:cutoff)
    lst.common.authors<-AllGeo2 %>% 
      group_by(JrnlType) %>% 
      slice((cutoff+1):nrow(AllGeo)) 
    # lst.common.authors$Code<-"all others"
    # lst.common.authors$IncomeGroup<-"all others"
    lst.common.authors<-lst.common.authors %>% 
      group_by(JrnlType,IncomeGroup) %>% 
      summarize(n=sum(n),perc=sum(perc),n_countries=n_distinct(Code)) 
    lst.common.authors$Code<-paste(lst.common.authors$n_countries,lst.common.authors$IncomeGroup,sep = " ", collapse = NULL)
    # lst.common.authors$Code<-paste(lst.common.authors$n_countries,lst.common.authors$IncomeGroup,"income countries", sep = " ", collapse = NULL)
    # lst.common.authors$Code<-gsub("income countries","",lst.common.authors$Code)
    # 
    lst.common.authors$IncomeGroup<- ordered(lst.common.authors$IncomeGroup, 
                                  levels = c("Low", 
                                             "Lower middle",
                                             "Upper middle",
                                             "High"))
    lst.common.authors <-lst.common.authors %>% 
      arrange(JrnlType,desc(IncomeGroup))
    
    most.common.authors$IncomeGroup<- ordered(most.common.authors$IncomeGroup, 
                                             levels = c("Low", 
                                                        "Lower middle",
                                                        "Upper middle",
                                                        "High"))
    
    most.common.authors$Code<-as.factor(most.common.authors$Code)
    
    
    most.common.authors<-bind_rows(most.common.authors, lst.common.authors)
    
    
    
    most.common.authors<-most.common.authors %>% 
      arrange(JrnlType) %>% 
      group_by(JrnlType) %>% 
      mutate(plot_order = row_number()) %>%
      arrange(JrnlType,plot_order)
    
    most.common.authors$Country<-most.common.authors$Code
  
    most.common.authors$Code<-paste(as.character(most.common.authors$Code),
                                       most.common.authors$JrnlType,
                                       sep="")
    
#     
#     most.common.authorsOA<-most.common.authors %>% 
#       filter(JrnlType=="OA")  
#     most.common.authorsOA$Code<-fct_inorder(most.common.authorsOA$Code,ordered = NA)
#       
#     most.common.authorsPW<-most.common.authors %>% 
#       filter(JrnlType=="PW")  
#     most.common.authorsPW$Code<-fct_inorder(most.common.authorsPW$Code,ordered = NA)
#     
#     most.common.authors<-bind_rows(most.common.authorsOA,most.common.authorsPW)
#     
#     # This is needed to put them in order in the plot with OTHER at the end of the graph
#     order<-rev(seq(1:nrow(most.common.authors))) #REV is what makes it go tyop to bottom if flipped coordinates
#     most.common.authors$Code <- factor(most.common.authors$Code,most.common.authors$Code[levels = order])
#     
#     most.common.authors$Code <-as.factor(most.common.authors$Code)
#     most.common.authors$order <-as.factor(most.common.authors$order)
#     most.common.authors$Code <- most.common.authors$Code %>% 
#       group_by(JrnlType) %>% 
#       fct_inorder(Code,ordered = NA)
#       
#       factor(Code,Code[levels = order$order])
#       most.common.authors$Code<-fct_inorder(most.common.authors$Code,ordered = NA)
#     
#     # rm(order,AllGeo,lst.common.authors)
#     most.common.authors
#     
#     most.common.authors
#     
#     =))
# most.common.authors3<-most.common.authors2 %>% filter(JrnlType=="OA")
# reorder(most.common.authors2$Code,most.common.authors2$plot_order)
    country_names<-most.common.authors %>% select(JrnlType,Country,plot_order) %>% arrange(JrnlType,desc(plot_order))
    most.common.authors$JrnlType<-as.factor(most.common.authors$JrnlType)
    levels(most.common.authors$JrnlType)[levels(most.common.authors$JrnlType)=="OA"] <- "Open Access"
    levels(most.common.authors$JrnlType)[levels(most.common.authors$JrnlType)=="PW"] <- "Paywalled"
    
    plot<-ggplot(most.common.authors, aes(x=reorder(Code,desc(plot_order)),
                                           y=perc, 
                                           fill=IncomeGroup))+
      geom_bar(stat = "identity",colour="black", size=0.1)+
      facet_grid(rows=vars(JrnlType),
                 # cols=vars(Code),
                 scales="free_y")+
      # scale_y_continuous(breaks = c(5,10,15,20,25,30,35,40),expand = c(0.0, 0))+
      # geom_text(size = 3, position = position_stack(vjust = 0.5))+
      xlab("Country") +
      ylab("Percentage of Articles")+
      # scale_fill_brewer(palette = "Greys")+
      # "#F7FBFF" "#DEEBF7" "#C6DBEF" "#9ECAE1" "#6BAED6" "#4292C6" "#2171B5" "#084594"
      # scale_fill_manual(values=c("#2171B5","#6BAED6","#BDD7E7","#EFF3FF"),
      # scale_fill_manual(values=c("#F7FBFF","#C6DBEF","#6BAED6","#084594"),
      # scale_fill_manual(values=c("#222C61","#7E82AC","#AFB8D9","#C7C5D5"),
      scale_fill_manual(values=c("#19245C","#626FA3","#98A2D9","#26378F"),
                               name="National Income Category",
                         breaks=c("High", "Upper middle","Lower middle","Low"))+
      # # 
      coord_flip()
    plot<-plot+
      theme_classic()+
      # labs(title = “Main title”, subtitle = “My subtitle”, caption = title_text)+
      # labs(title = title_text,size=10)+
      theme(
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.title.x=element_text(colour="black", size = 12, vjust=-0.5),
        axis.title.y=element_text(colour="black", size = 12, hjust=0.5,vjust=0.5,angle = 90),
        plot.title = element_text(size=8),
        legend.text = element_text(colour="black", size = 8, vjust=0.5),
        legend.title = element_text(size=8),
        legend.position="right",
        # legend.key.height = unit(1,"lines"),
        plot.margin =unit(c(1,1,1,1.5), "lines")  
      )
    plot
    
  

  return(plot)
  
}