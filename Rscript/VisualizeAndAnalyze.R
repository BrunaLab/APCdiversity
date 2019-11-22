#libraries
library(ggplot2)
library(dplyr)

#data
#FirstAuthDiv <- read.csv("CleanData/FirstAuthDiv.csv", header = TRUE)
#FirstRich1000 <- read.csv("CleanData/FirstRich1000itters.csv", header = TRUE)
#FirstRich1000 <- FirstRich1000 %>%
#  select(JournalAndType = X, V1:V1000)

DivMetricsFullPools <- read.csv("CleanData/DivMetricsFullPools.csv", header = TRUE)
FirstDivMetricsByJrnl <- read.csv("CleanData/FirstDivMetricsByJrnl.csv", header = TRUE)
PropSampsSimpsDivsPW <- read.csv("CleanData/ProporSampsSimpsDivPW.csv", header = TRUE)


#over all richness for PW and OA each itteration (OA should stay the same)
FirstRich1000 <- merge(FirstRich1000, FirstAuthDiv, by = "JournalAndType")
#subset data to OA and PW dataframes
FirstRich1000_OA <- FirstRich1000 %>%
  filter(JrnlType == "OA")%>%
  select(V1:V1000)
FirstRich1000_PW<- FirstRich1000 %>%
  filter(JrnlType == "paywall") %>%
  select(V1:V1000)

MeanRichOA <- mean(colMeans(FirstRich1000_OA))
MeanRichPW <- as.data.frame(colMeans(FirstRich1000_PW))
MeanRichPW$MeanRich <- MeanRichPW$`colMeans(FirstRich1000_PW)`
MeanOfMeanRichPW <- mean(MeanRichPW$MeanRich)
#HISTOGRAM OF PW JOURNAL AVERAGE OVERALL RICHNESS with OA AVERAGE RICHNESS
ggplot(MeanRichPW, aes(x = MeanRich))+
  geom_histogram()+
  geom_segment(aes(x = 9.031, y = 0, xend = 9.031, yend = 120),color = "red")+
  geom_segment(aes(x = MeanOfMeanRichPW, y = 0, xend = MeanOfMeanRichPW, yend = 120),color = "blue")
 
#plot rich
ggplot(FirstAuthDiv, aes(JrnlType, MeanRich, color = JrnlType)) +
  geom_boxplot()+
  geom_jitter(width = .07, height = 0, alpha = 0.5 )

#plot diversity
ggplot(FirstAuthDiv, aes(JrnlType, MeanDiveristy, color = JrnlType))+
  geom_boxplot()+
  geom_jitter(width = .08, height = 0, alpha = 0.5 )
