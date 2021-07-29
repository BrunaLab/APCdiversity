BrayCurtis<-function(bootstrap_results_countries) {
library(tidyverse)
library(vegan)
library(egg)


# dune2<-slice(dune,1:2)
# simpson<-diversity(dune2,index = "simpson")
# bray = vegdist(dune2, "bray") 
# gower = vegdist(dune2, "gower")

##################
# BC SOLO ALL
##################
bootstrap_results_countries<-read_csv("./output/bootstrap_results_countries.csv")
bootstrap_results_countries
foo<-bootstrap_results_countries %>% 
  filter(author=="solo"& Dataset=="All Countries") %>% 
  select(n,Code,replicate) %>% 
  arrange(n) %>% 
  spread(Code,n) 
foo[is.na(foo)] = 0
foo$cat<-"PW"
foo$cat<-NULL
foo$replicate<-NULL
bray = vegdist(foo, "bray")
str(bray)
bray<-as.matrix(bray)
bray[lower.tri(bray)] <- 0
bray[lower.tri(bray,diag=TRUE)] <- NA
bray<-as.vector(bray)
bray<-as.vector(na.omit(bray))
hist(bray)



# sole_NOCHNUSA
# coauthor_NOCHNUSA
# sole_ALL
# coauthor_ALL

source("./Rscript/functions/DivRichCalc.R") 
OA_data<-DivRichCalc(sole_ALL,'author_first','OA')
OA_countries<-as.data.frame(OA_data[3])
colnames(OA_countries)
names(OA_countries)<-c("Code","n")
OA_countries<-OA_countries %>%spread(Code,n)
OA_countries[is.na(OA_countries)] = 0
OA_countries$cat<-"OA"

data<-bind_rows(OA_countries,foo)
data[is.na(data)] = 0
# first row is oa
data$cat<-NULL
data$replicate<-NULL

bray2 = vegdist(data, "bray") 
hist(bray2)
str(bray2)

bray2<-as.matrix(bray2)
bray2[lower.tri(bray2)] <- 0
bray2[lower.tri(bray2,diag=TRUE)] <- NA

bray2<-as_tibble(bray2)
OA vs. PW<-slice(bray2,1)
OA vs. PW<-as.vector(OA vs. PW)
OA vs. PW<-as.numeric(OA vs. PW)
PW vs. PW<-slice(bray2,2:1001)

PW vs. PW<-c(PW vs. PW)
PW vs. PW<-unlist(PW vs. PW)
PW vs. PW<-as.vector(PW vs. PW)
PW vs. PW<-as_tibble(PW vs. PW)
PW vs. PW<-na.omit(PW vs. PW)
summary(PW vs. PW)

OA vs. PW<-na.omit(OA vs. PW)
OA vs. PW<-unlist(OA vs. PW)
OA vs. PW<-as_tibble(OA vs. PW)
summary(OA vs. PW)
summary(PW vs. PW)

names(OA vs. PW)<-c("bray")
names(PW vs. PW)<-c("bray")
hist(PW vs. PW$bray)
hist(OA vs. PW$bray)
OA vs. PW$cat<-"OA vs. PW"
PW vs. PW$cat<-"PW vs. PW"
data2<-bind_rows(OA vs. PW,PW vs. PW)
data2.1<-data2
data2.1$authors<-"Single Authors"
data2.1$dataset<-"All Countries"

t.test(bray,bray2)




##################
# BC 1st of CO ALL
##################

bootstrap_results_countries<-read_csv("./output/bootstrap_results_countries.csv")
foo<-bootstrap_results_countries %>% 
  filter(author=="author_first"& Dataset=="All Countries") %>% 
  select(n,Code,replicate) %>% 
  arrange(n) %>% 
  spread(Code,n) 
foo[is.na(foo)] = 0
foo$cat<-"PW"
foo$cat<-NULL
foo$replicate<-NULL
bray = vegdist(foo, "bray")
str(bray)
bray<-as.matrix(bray)
bray[lower.tri(bray)] <- 0
bray[lower.tri(bray,diag=TRUE)] <- NA
bray<-as.vector(bray)
bray<-as.vector(na.omit(bray))
hist(bray)



# sole_NOCHNUSA
# coauthor_NOCHNUSA
# sole_ALL
# coauthor_ALL

source("./Rscript/functions/DivRichCalc.R") 
OA_data<-DivRichCalc(coauthor_ALL,'author_first','OA')
OA_countries<-as.data.frame(OA_data[3])
colnames(OA_countries)
names(OA_countries)<-c("Code","n")
OA_countries<-OA_countries %>%spread(Code,n)
OA_countries[is.na(OA_countries)] = 0
OA_countries$cat<-"OA"

data<-bind_rows(OA_countries,foo)
data[is.na(data)] = 0
# first row is oa
data$cat<-NULL
data$replicate<-NULL

bray2 = vegdist(data, "bray") 
hist(bray2)
str(bray2)

bray2<-as.matrix(bray2)
bray2[lower.tri(bray2)] <- 0
bray2[lower.tri(bray2,diag=TRUE)] <- NA

bray2<-as_tibble(bray2)
OA vs. PW<-slice(bray2,1)
OA vs. PW<-as.vector(OA vs. PW)
OA vs. PW<-as.numeric(OA vs. PW)
PW vs. PW<-slice(bray2,2:1001)

PW vs. PW<-c(PW vs. PW)
PW vs. PW<-unlist(PW vs. PW)
PW vs. PW<-as.vector(PW vs. PW)
PW vs. PW<-as_tibble(PW vs. PW)
PW vs. PW<-na.omit(PW vs. PW)
summary(PW vs. PW)

OA vs. PW<-na.omit(OA vs. PW)
OA vs. PW<-unlist(OA vs. PW)
OA vs. PW<-as_tibble(OA vs. PW)
summary(OA vs. PW)
summary(PW vs. PW)

names(OA vs. PW)<-c("bray")
names(PW vs. PW)<-c("bray")
hist(PW vs. PW$bray)
hist(OA vs. PW$bray)
OA vs. PW$cat<-"OA vs. PW"
PW vs. PW$cat<-"PW vs. PW"
data2<-bind_rows(OA vs. PW,PW vs. PW)
data2.2<-data2
data2.2$authors<-"First Authors"
data2.2$dataset<-"All Countries"


t.test(bray,bray2)




##################
# BC Solo no CHN USA
##################

bootstrap_results_countries<-read_csv("./output/bootstrap_results_countries.csv")
foo<-bootstrap_results_countries %>% 
  filter(author=="solo"& Dataset=="CHN & USA excluded") %>% 
  select(n,Code,replicate) %>% 
  arrange(n) %>% 
  spread(Code,n) 
foo[is.na(foo)] = 0
foo$cat<-"PW"
foo$cat<-NULL
foo$replicate<-NULL
bray = vegdist(foo, "bray")
str(bray)
bray<-as.matrix(bray)
bray[lower.tri(bray)] <- 0
bray[lower.tri(bray,diag=TRUE)] <- NA
bray<-as.vector(bray)
bray<-as.vector(na.omit(bray))
hist(bray)



# sole_NOCHNUSA
# coauthor_NOCHNUSA
# sole_ALL
# coauthor_ALL

source("./Rscript/functions/DivRichCalc.R") 
OA_data<-DivRichCalc(sole_NOCHNUSA,'author_first','OA')
OA_countries<-as.data.frame(OA_data[3])
colnames(OA_countries)
names(OA_countries)<-c("Code","n")
OA_countries<-OA_countries %>%spread(Code,n)
OA_countries[is.na(OA_countries)] = 0
OA_countries$cat<-"OA"

data<-bind_rows(OA_countries,foo)
data[is.na(data)] = 0
# first row is oa
data$cat<-NULL
data$replicate<-NULL

bray2 = vegdist(data, "bray") 
hist(bray2)
str(bray2)

bray2<-as.matrix(bray2)
bray2[lower.tri(bray2)] <- 0
bray2[lower.tri(bray2,diag=TRUE)] <- NA

bray2<-as_tibble(bray2)
OA vs. PW<-slice(bray2,1)
OA vs. PW<-as.vector(OA vs. PW)
OA vs. PW<-as.numeric(OA vs. PW)
PW vs. PW<-slice(bray2,2:1001)

PW vs. PW<-c(PW vs. PW)
PW vs. PW<-unlist(PW vs. PW)
PW vs. PW<-as.vector(PW vs. PW)
PW vs. PW<-as_tibble(PW vs. PW)
PW vs. PW<-na.omit(PW vs. PW)
summary(PW vs. PW)

OA vs. PW<-na.omit(OA vs. PW)
OA vs. PW<-unlist(OA vs. PW)
OA vs. PW<-as_tibble(OA vs. PW)
summary(OA vs. PW)
summary(PW vs. PW)

names(OA vs. PW)<-c("bray")
names(PW vs. PW)<-c("bray")
hist(PW vs. PW$bray)
hist(OA vs. PW$bray)
OA vs. PW$cat<-"OA vs. PW"
PW vs. PW$cat<-"PW vs. PW"
data2<-bind_rows(OA vs. PW,PW vs. PW)
data2.3<-data2
data2.3$authors<-"Single Authors"
data2.3$dataset<-"China and USA Excluded"



t.test(bray,bray2)



##################
# BC first of co no CHN USA
##################

bootstrap_results_countries<-read_csv("./output/bootstrap_results_countries.csv")
foo<-bootstrap_results_countries %>% 
  filter(author=="author_first"& Dataset=="CHN & USA excluded") %>% 
  select(n,Code,replicate) %>% 
  arrange(n) %>% 
  spread(Code,n) 
foo[is.na(foo)] = 0
foo$cat<-"PW"
foo$cat<-NULL
foo$replicate<-NULL
bray = vegdist(foo, "bray")
str(bray)
bray<-as.matrix(bray)
bray[lower.tri(bray)] <- 0
bray[lower.tri(bray,diag=TRUE)] <- NA
bray<-as.vector(bray)
bray<-as.vector(na.omit(bray))
hist(bray)



# sole_NOCHNUSA
# coauthor_NOCHNUSA
# sole_ALL
# coauthor_ALL

source("./Rscript/functions/DivRichCalc.R") 
OA_data<-DivRichCalc(coauthor_NOCHNUSA,'author_first','OA')
OA_countries<-as.data.frame(OA_data[3])
colnames(OA_countries)
names(OA_countries)<-c("Code","n")
OA_countries<-OA_countries %>%spread(Code,n)
OA_countries[is.na(OA_countries)] = 0
OA_countries$cat<-"OA"

data<-bind_rows(OA_countries,foo)
data[is.na(data)] = 0
# first row is oa
data$cat<-NULL
data$replicate<-NULL

bray2 = vegdist(data, "bray") 
hist(bray2)
str(bray2)

bray2<-as.matrix(bray2)
bray2[lower.tri(bray2)] <- 0
bray2[lower.tri(bray2,diag=TRUE)] <- NA

bray2<-as_tibble(bray2)
OA vs. PW<-slice(bray2,1)
OA vs. PW<-as.vector(OA vs. PW)
OA vs. PW<-as.numeric(OA vs. PW)
PW vs. PW<-slice(bray2,2:1001)

PW vs. PW<-c(PW vs. PW)
PW vs. PW<-unlist(PW vs. PW)
PW vs. PW<-as.vector(PW vs. PW)
PW vs. PW<-as_tibble(PW vs. PW)
PW vs. PW<-na.omit(PW vs. PW)
summary(PW vs. PW)

OA vs. PW<-na.omit(OA vs. PW)
OA vs. PW<-unlist(OA vs. PW)
OA vs. PW<-as_tibble(OA vs. PW)
summary(OA vs. PW)
summary(PW vs. PW)

names(OA vs. PW)<-c("bray")
names(PW vs. PW)<-c("bray")
hist(PW vs. PW$bray)
hist(OA vs. PW$bray)
OA vs. PW$cat<-"OA vs. PW"
PW vs. PW$cat<-"PW vs. PW"
data2<-bind_rows(OA vs. PW,PW vs. PW)

data2.4<-data2
data2.4$authors<-"First Authors"
data2.4$dataset<-"China and USA Excluded"


t.test(bray,bray2)



#######
#Fig
#######
data<-bind_rows(data2.1,data2.2,data2.3,data2.4)
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
    panel.spacing.x =unit(1, "lines"), 
    panel.spacing.y=unit(2,"lines"),
    legend.position = "none",
    legend.text = element_text(size=20),
    plot.margin =unit(c(3,1,1,1.5), "lines")   #plot margin - top, right, bottom, left
  )    
Fig


facet_labels<-c("A","B","C","D")
Fig<-tag_facet(Fig,open="", close="", tag_pool=facet_labels,vjust=0.5,hjust=-1,size=10)

return(Fig)




}



