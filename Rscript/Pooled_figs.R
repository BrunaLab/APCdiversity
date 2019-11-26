
library(tidyverse)
##############################################################
# bar chart of % of authors from different countries, 
# pooling the least common ones
##############################################################

PayWallAllGeo
OAAllGeo

# "cutoff" is how many countries you want on the chart, 
# all the rest will be in "OTHER"
cutoff = 10
# ungroup to be able to add %
PW<-ungroup(PayWallAllGeo)
# Need to add percentages since unequal number of pubis in OA and PW journals
PW<-PW %>% 
  mutate(perc = n / sum(n)*100) %>% 
  arrange(desc(perc))
PW

# Rearrnage to that they are in order from high to low, cut off the 
# ones above the cutoff, put the rest in a new df and rename the country "other"
# then reattach the dataframes
PW.author.Geo<-arrange(PW, desc(perc)) %>% select(Code,perc,IncomeGroup)
PW.most.common.authors<-slice(PW.author.Geo, 1:cutoff)
PW.least.common.authors<-slice(PW.author.Geo, (cutoff+1):nrow(PW.author.Geo)) 
PW.least.common.authors$Code<-"OTHER"
PW.least.common.authors<-PW.least.common.authors %>% 
  mutate(perc=sum(perc)) %>%
  slice(1:1)
PW.most.common.authors<-bind_rows(PW.most.common.authors, PW.least.common.authors)
PW.most.common.authors$Code<-as.factor(PW.most.common.authors$Code)
# Add a label as PW
PW.most.common.authors$jrnl_type<-"PW" 

# This is needed to put them in order in the plot with OTHER at the end of the graph
order<-seq(1:nrow(PW.most.common.authors))
PW.most.common.authors$Code <- factor(PW.most.common.authors$Code,PW.most.common.authors$Code[levels = order])
# levels(most.common.editors$geo.code)
PW.most.common.authors


###########################
OA<-ungroup(OAAllGeo)

OA<-OA %>% 
  mutate(perc = n / sum(n)*100) %>% 
  arrange(desc(perc))
OA


# cutoff = 9 # This is how many countries you want on the chart, all the rest will be in "OTHER"
OA.author.Geo<-arrange(OA, desc(perc)) %>% select(Code,perc,IncomeGroup)
OA.most.common.authors<-slice(OA.author.Geo, 1:cutoff)
OA.least.common.authors<-slice(OA.author.Geo, (cutoff+1):nrow(OA.author.Geo)) 
OA.least.common.authors$Code<-"OTHER"
OA.least.common.authors<-OA.least.common.authors %>% 
  mutate(perc=sum(perc)) %>%
  slice(1:1)
OA.most.common.authors<-bind_rows(OA.most.common.authors, OA.least.common.authors)
OA.most.common.authors$Code<-as.factor(OA.most.common.authors$Code)
OA.most.common.authors$jrnl_type<-"OA"


# This is needed to put them in order in the plot with OTHER at the end of the graph
order<-seq(1:nrow(OA.most.common.authors))
OA.most.common.authors$Code <- factor(OA.most.common.authors$Code,OA.most.common.authors$Code[levels = order])
# levels(most.common.editors$geo.code)
OA.most.common.authors




ALL_summary<-full_join(PW.most.common.authors,OA.most.common.authors)


# Country Representation All authors OA
MostCommon_plot<-ggplot(ALL_summary, aes(Code, perc, fill = jrnl_type))+
  geom_bar(stat = "identity", position="dodge")+
  coord_flip()+
  ggtitle("Country Representation by Journal Type")+
  facet_grid(rows = vars(jrnl_type))

MostCommon_plot




# Country Representation All authors PW
PW.MostCommon_plot<-ggplot(PW.most.common.authors, aes(Code, perc,fill=IncomeGroup))+
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("Country Representation - PW")
PW.MostCommon_plot



# Country Representation All authors OA
OA.MostCommon_plot<-ggplot(OA.most.common.authors, aes(Code, perc,fill=IncomeGroup))+
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("Country Representation - OA")
  OA.MostCommon_plot
  
  
  ###########
  # plotting as a "reaction norm" plot
  ############
  
  
all2<-full_join(PW,OA,by=c("Country","Code","Region","IncomeGroup")) %>% 
    select(Country, Code, Region, IncomeGroup, n.x, n.y, perc.x, perc.y) %>% 
    replace_na(list(n.x = 0, n.y = 0,perc.y=0,perc.x=0)) %>% 
    dplyr::rename(nPW=n.x,nOA=n.y,percPW=perc.x,percOA=perc.y) %>% 
  gather(cat,perc,nPW:percOA) %>% filter(cat=="percOA" | cat=="percPW")

  all2$cat<-as.factor(all2$cat)
  all2$Code<-as.factor(all2$Code)
  

rxn.norm.plot<-ggplot(data=all2, aes(x=cat, y=perc,group=Country)) +
  geom_line(size=0.5) + geom_point(size=4, aes(colour=IncomeGroup, shape=IncomeGroup))+
  ylab("1st Authors (%)")+
  xlab("Journal Category")+ 
  # scale_y_continuous(limit=c(0, 100))+
  scale_colour_manual(values=c("#000066","#0072B2","lightgray","red"))
  
rxn.norm.plot<-rxn.norm.plot + theme_classic() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                                 axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
                                                                 axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
                                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                                                 plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=22),        #Sets title size, style, location
                                                                 axis.title.x=element_text(colour="black", size = 20, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                                 axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                                 legend.position = "none",
                                                                 axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                                                 plot.margin = unit(c(0,2,2,1), "cm"))
rxn.norm.plot


#############
# Is there an OA penalty?

all3<-full_join(PW,OA,by=c("Country","Code","Region","IncomeGroup")) %>% 
  select(Country, Code, Region, IncomeGroup, n.x, n.y, perc.x, perc.y) %>% 
  replace_na(list(n.x = 0, n.y = 0,perc.y=0,perc.x=0)) %>% 
  dplyr::rename(nPW=n.x,nOA=n.y,percPW=perc.x,percOA=perc.y) 

all3$PWminusOA<-all3$percPW-all3$percOA
str(all3$PWminusOA)

all3<-all3 %>% 
  select(Country,PWminusOA,IncomeGroup) %>%
  arrange(desc(PWminusOA)) %>% 
  filter(PWminusOA < -1 |PWminusOA > 1) 
# This is needed to put them in order in the plot with OTHER at the end of the graph
order<-seq(1:nrow(all3))
all3$Country<- factor(all3$Country,all3$Country[levels = order])
# levels(most.common.editors$geo.code)
all3



OApenalty.plot<-ggplot(all3, aes(Country, PWminusOA,fill=IncomeGroup))+
  geom_bar(stat = "identity")+
  ggtitle("OA Penalty (negative values indicate less pub in OA)")
OApenalty.plot<-OApenalty.plot+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45,hjust=1))
OApenalty.plot


#############
# low income countries only
all4<-full_join(PW,OA,by=c("Country","Code","Region","IncomeGroup")) %>% 
  select(Country, Code, Region, IncomeGroup, n.x, n.y, perc.x, perc.y) %>% 
  replace_na(list(n.x = 0, n.y = 0,perc.y=0,perc.x=0)) %>% 
  dplyr::rename(nPW=n.x,nOA=n.y,percPW=perc.x,percOA=perc.y) 

all4$PWminusOA<-all4$percPW-all4$percOA
str(all3$PWminusOA)

levels(all4$IncomeGroup)

all4<-all4 %>% 
  select(Country,IncomeGroup,nPW,nOA ) %>%
  filter(IncomeGroup == "Low income") %>% 
  gather(category,count,nPW:nOA) 

all4 %>% group_by(category) %>% summarize(sum(count))
