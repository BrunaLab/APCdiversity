#libraries
library(ggplot2)
library(dplyr)

#data
FirstAuthDiv <- read.csv("CleanData/FirstAuthDiv.csv", header = TRUE)


#plot rich
ggplot(FirstAuthDiv, aes(JrnlType, MeanRich, color = JrnlType)) +
  geom_boxplot()+
  geom_point(alpha = .5)

#plot diversity
ggplot(FirstAuthDiv, aes(JrnlType, MeanDiveristy, color = JrnlType))+
  geom_boxplot()+
  geom_jitter(width = .2, height = 0, alpha = 0.5 )
