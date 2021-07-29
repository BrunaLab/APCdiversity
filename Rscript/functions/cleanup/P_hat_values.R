#########################################
# RICHNESS
#########################################
figure_values<-ungroup(figure_values)
P_Hat<-figure_values
P_Hat$P_Hat<-NA
P_Hat$JrnlType<-NULL
##########
# All countries, coauthored
crit<-figure_values %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  select(OA_Rich)

perc<-bootstrap_results %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  ungroup() %>% 
  tally(Richness<crit$OA_Rich) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA

P_Hat$P_Hat[P_Hat$author=="author_first" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########


##########
# without USA CHN, coauthored
crit<-figure_values %>% 
  filter(Dataset=="Without China & USA") %>% 
  filter(author=="author_first") %>% 
  select(OA_Rich)

perc<-bootstrap_results %>% 
  filter(Dataset=="Without China & USA") %>% 
  filter(author=="author_first") %>% 
  ungroup() %>% 
  tally(Richness<crit$OA_Rich) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA

P_Hat$P_Hat[P_Hat$author=="author_first" & 
              P_Hat$Dataset=="Without China & USA"]<-perc_belowOA
###########

##########
# All countries, coauthored
crit<-figure_values %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  select(OA_Rich)

perc<-bootstrap_results %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  ungroup() %>% 
  tally(Richness<crit$OA_Rich) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA

P_Hat$P_Hat[P_Hat$author=="solo" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########


##########
# without USA CHN, coauthored
crit<-figure_values %>% 
  filter(Dataset=="Without China & USA") %>% 
  filter(author=="solo") %>% 
  select(OA_Rich)

perc<-bootstrap_results %>% 
  filter(Dataset=="Without China & USA") %>% 
  filter(author=="solo") %>% 
  ungroup() %>% 
  tally(Richness<crit$OA_Rich) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA

P_Hat$P_Hat[P_Hat$author=="solo" & 
              P_Hat$Dataset=="Without China & USA"]<-perc_belowOA
###########

##################################################################################
# DIVERISTY
############################################################################
figure_values<-ungroup(figure_values)
P_Hat<-figure_values
P_Hat$P_Hat<-NA
P_Hat$JrnlType<-NULL
##########
# All countries, coauthored
crit<-figure_values %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  select(OA_Div)

perc<-bootstrap_results %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  ungroup() %>% 
  tally(InvSimp<crit$OA_Div) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA

P_Hat$P_Hat[P_Hat$author=="author_first" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########


##########
# without USA CHN, coauthored
crit<-figure_values %>% 
  filter(Dataset=="Without China & USA") %>% 
  filter(author=="author_first") %>% 
  select(OA_Div)

perc<-bootstrap_results %>% 
  filter(Dataset=="Without China & USA") %>% 
  filter(author=="author_first") %>% 
  ungroup() %>% 
  tally(InvSimp<crit$OA_Div) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA

P_Hat$P_Hat[P_Hat$author=="author_first" & 
              P_Hat$Dataset=="Without China & USA"]<-perc_belowOA
###########

##########
# All countries, coauthored
crit<-figure_values %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  select(OA_Div)

perc<-bootstrap_results %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  ungroup() %>% 
  tally(InvSimp<crit$OA_Div) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA

P_Hat$P_Hat[P_Hat$author=="solo" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########


##########
# without USA CHN, coauthored
crit<-figure_values %>% 
  filter(Dataset=="Without China & USA") %>% 
  filter(author=="solo") %>% 
  select(OA_Div)

perc<-bootstrap_results %>% 
  filter(Dataset=="Without China & USA") %>% 
  filter(author=="solo") %>% 
  ungroup() %>% 
  tally(InvSimp<crit$OA_Div) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA

P_Hat$P_Hat[P_Hat$author=="solo" & 
              P_Hat$Dataset=="Without China & USA"]<-perc_belowOA
###########







#######################################
# P_HAT

# single, all
label_data<-ungroup(label_data)
P_Hat<-label_data
P_Hat$P_Hat<-NA
P_Hat$color<-NULL
##########
# All countries, coauthored, High
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(IncomeGroup=="High\nIncome") %>% 
  select(perc)

perc<-Subsampled_Income_summary_plot %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(IncomeGroup=="High\nIncome") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA

P_Hat$P_Hat[P_Hat$IncomeGroup=="High\nIncome" &
        P_Hat$author=="author_first" & 
        P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########

##########
# All countries, coauthored, Lower-middle\nIncome
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(IncomeGroup=="Lower-middle\nIncome") %>% 
  select(perc)

perc<-Subsampled_Income_summary_plot %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(IncomeGroup=="Lower-middle\nIncome") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA


P_Hat$P_Hat[P_Hat$IncomeGroup=="Lower-middle\nIncome" &
              P_Hat$author=="author_first" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA

###########

##########

# All countries, coauthored, Upper-middle\nIncome
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(IncomeGroup=="Upper-middle\nIncome") %>% 
  select(perc)

perc<-Subsampled_Income_summary_plot %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(IncomeGroup=="Upper-middle\nIncome") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA


P_Hat$P_Hat[P_Hat$IncomeGroup=="Upper-middle\nIncome" &
              P_Hat$author=="author_first" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########


##########
# All countries, coauthored, Low\nIncome
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(IncomeGroup=="Low\nIncome") %>% 
  select(perc)

perc<-Subsampled_Income_summary_plot %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(IncomeGroup=="Low\nIncome") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$IncomeGroup=="Low\nIncome" &
              P_Hat$author=="author_first" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########

##########
# All countries, coauthored, High
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(IncomeGroup=="High\nIncome") %>% 
  select(perc)

perc<-Subsampled_Income_summary_plot %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(IncomeGroup=="High\nIncome") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA

P_Hat$P_Hat[P_Hat$IncomeGroup=="High\nIncome" &
              P_Hat$author=="solo" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########

##########
# All countries, coauthored, Lower-middle\nIncome
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(IncomeGroup=="Lower-middle\nIncome") %>% 
  select(perc)

perc<-Subsampled_Income_summary_plot %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(IncomeGroup=="Lower-middle\nIncome") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA


P_Hat$P_Hat[P_Hat$IncomeGroup=="Lower-middle\nIncome" &
              P_Hat$author=="solo" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA

###########

##########

# All countries, coauthored, Upper-middle\nIncome
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(IncomeGroup=="Upper-middle\nIncome") %>% 
  select(perc)

perc<-Subsampled_Income_summary_plot %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(IncomeGroup=="Upper-middle\nIncome") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA


P_Hat$P_Hat[P_Hat$IncomeGroup=="Upper-middle\nIncome" &
              P_Hat$author=="solo" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########


##########
# All countries, coauthored, Low\nIncome
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(IncomeGroup=="Low\nIncome") %>% 
  select(perc)

perc<-Subsampled_Income_summary_plot %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(IncomeGroup=="Low\nIncome") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$IncomeGroup=="Low\nIncome" &
              P_Hat$author=="solo" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########
P_Hat

levels(P_Hat$IncomeGroup)[levels(P_Hat$IncomeGroup)=="Low\nIncome"] <-"Low" 
levels(P_Hat$IncomeGroup)[levels(P_Hat$IncomeGroup)=="Lower-middle\nIncome"] <- "Lower middle"
levels(P_Hat$IncomeGroup)[levels(P_Hat$IncomeGroup)=="Upper-middle\nIncome"] <- "Upper middle"
levels(P_Hat$IncomeGroup)[levels(P_Hat$IncomeGroup)=="High\nIncome"] <-"High" 


P_Hat <- P_Hat %>% dplyr::rename("OA_perc"="perc", 
                                 "Author"="author",
                                 "Countries"="Dataset"
                                 ) %>% 
  select(-n) 


P_Hat$Author<-gsub("author_first","First",P_Hat$Author)
P_Hat$Author<-gsub("solo","Single",P_Hat$Author)


#######################################################
#######################################################
#######################################################
# REGION P_hat
#######################################################
#######################################################
label_data<-ungroup(label_data)
P_Hat<-label_data
P_Hat$P_Hat<-NA

##########
# All countries, coauthored, South Asia
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(Region=="South\nAsia") %>% 
  select(perc)

perc<-fig_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(Region=="South\nAsia") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$Region=="South\nAsia" &
              P_Hat$author=="author_first" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########

##########
# All countries, coauthored, N AM
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(Region=="North\nAmerica") %>% 
  select(perc)

perc<-fig_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(Region=="North\nAmerica") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$Region=="North\nAmerica" &
              P_Hat$author=="author_first" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########

##########
# All countries, coauthored, SS Africa
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(Region=="Sub-Saharan\nAfrica") %>% 
  select(perc)

perc<-fig_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(Region=="Sub-Saharan\nAfrica") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$Region=="Sub-Saharan\nAfrica" &
              P_Hat$author=="author_first" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########

##########
# All countries, coauthored, LatAM Carib
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(Region=="Latin America &\nCaribbean") %>% 
  select(perc)

perc<-fig_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(Region=="Latin America &\nCaribbean") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$Region=="Latin America &\nCaribbean" &
              P_Hat$author=="author_first" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########

##########
# All countries, coauthored, ME North Af
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(Region=="Middle East &\nNorth Africa") %>% 
  select(perc)

perc<-fig_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(Region=="Middle East &\nNorth Africa") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$Region=="Middle East &\nNorth Africa" &
              P_Hat$author=="author_first" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########

##########
# All countries, coauthored, East Asia Pac
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(Region=="East Asia &\nPacific") %>% 
  select(perc)

perc<-fig_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(Region=="East Asia &\nPacific") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$Region=="East Asia &\nPacific" &
              P_Hat$author=="author_first" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########

##########
# All countries, coauthored, Europe Cent Asia
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(Region=="Europe &\nCentral Asia") %>% 
  select(perc)

perc<-fig_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="author_first") %>% 
  filter(Region=="Europe &\nCentral Asia") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$Region=="Europe &\nCentral Asia" &
              P_Hat$author=="author_first" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########


###########
###########
###########
# Solo
###########
###########
###########

###########
# All countries, solo, South Asia
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(Region=="South\nAsia") %>% 
  select(perc)

perc<-fig_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(Region=="South\nAsia") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$Region=="South\nAsia" &
              P_Hat$author=="solo" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########

##########
# All countries, solo, na am
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(Region=="North\nAmerica") %>% 
  select(perc)

perc<-fig_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(Region=="North\nAmerica") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$Region=="North\nAmerica" &
              P_Hat$author=="solo" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########

##########
# All countries, solo, SS Africa
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(Region=="Sub-Saharan\nAfrica") %>% 
  select(perc)

perc<-fig_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(Region=="Sub-Saharan\nAfrica") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$Region=="Sub-Saharan\nAfrica" &
              P_Hat$author=="solo" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########

##########
# All countries, Solo, LatAm Carrib
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(Region=="Latin America &\nCaribbean") %>% 
  select(perc)

perc<-fig_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(Region=="Latin America &\nCaribbean") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$Region=="Latin America &\nCaribbean" &
              P_Hat$author=="solo" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########

##########
# All countries, solo, mid east n afr
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(Region=="Middle East &\nNorth Africa") %>% 
  select(perc)

perc<-fig_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(Region=="Middle East &\nNorth Africa") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$Region=="Middle East &\nNorth Africa" &
              P_Hat$author=="solo" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########

##########
# All countries, solo, E asia pac
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(Region=="East Asia &\nPacific") %>% 
  select(perc)

perc<-fig_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(Region=="East Asia &\nPacific") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$Region=="East Asia &\nPacific" &
              P_Hat$author=="solo" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########

##########
# All countries, solo, europe C asia
crit<-label_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(Region=="Europe &\nCentral Asia") %>% 
  select(perc)

perc<-fig_data %>% 
  filter(Dataset=="All Countries") %>% 
  filter(author=="solo") %>% 
  filter(Region=="Europe &\nCentral Asia") %>% 
  ungroup() %>% 
  tally(perc<crit$perc) %>% 
  mutate(perc_belowOA = n/1000)
perc_belowOA<-perc$perc_belowOA
perc_belowOA



P_Hat$P_Hat[P_Hat$Region=="Europe &\nCentral Asia" &
              P_Hat$author=="solo" & 
              P_Hat$Dataset=="All Countries"]<-perc_belowOA
###########


Region<-P_Hat$Region
Region<-recode_factor(Region,"South\nAsia"="South Asia",
                      "North\nAmerica"="North America",
                      "Sub-Saharan\nAfrica"="Sub-Saharan Africa",
                      "Latin America &\nCaribbean"="Latin America & Caribbean",
                      "Middle East &\nNorth Africa"="Middle East & North Africa",
                      "East Asia &\nPacific"="East Asia & Pacific",
                      "Europe &\nCentral Asia"="Europe & Central Asia",
                      .default = levels(Region))
P_Hat$Region<-Region 

P_Hat <- P_Hat %>% dplyr::rename("OA_perc"="perc", 
                                 "Author"="author",
                                 "Countries"="Dataset") %>% 
  select(-n) 


P_Hat$Author<-gsub("author_first","First",P_Hat$Author)
P_Hat$Author<-gsub("solo","Single",P_Hat$Author)




