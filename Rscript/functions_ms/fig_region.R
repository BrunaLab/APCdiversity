fig_region <- function(DataSet,
                            Subsampled_Countries,
                            countries,
                            sole_ALL,
                            sole_NOCHNUSA,
                            first_ALL,
                            first_NOCHNUSA) {

  
  library(tidyverse)
  library(RColorBrewer)
  library(ggExtra)
  library(egg)


  DataSet <- DataSet %>%
    
    
    mutate(Dataset = ifelse(Dataset == "all", "All Countries", Dataset)) %>%
    
    
    mutate(Dataset = ifelse(Dataset == "no_CHN_USA", "CHN & USA excluded", Dataset))
  
  




  Subsampled_Countries <- Subsampled_Countries %>%
   
    
    mutate(Dataset = ifelse(Dataset == "all", "All Countries", Dataset)) %>%
    
    mutate(Dataset = ifelse(Dataset == "no_CHN_USA", "CHN & USA excluded", Dataset))

  
  DataSet <- as.data.frame(DataSet)
  
  Subsampled_Countries <- as.data.frame(Subsampled_Countries)
  

  Subsampled_Countries$Region <- as.factor(Subsampled_Countries$Region)
  Subsampled_Countries$Region <- as.factor(Subsampled_Countries$Region)
  Subsampled_Countries$Code <- as.factor(Subsampled_Countries$Code)
  Subsampled_Countries$Country <- as.factor(Subsampled_Countries$Country)
  Subsampled_Countries$IncomeGroup <- ordered(Subsampled_Countries$IncomeGroup,
    levels = c("Low income", 
               "Lower middle income",
               "Upper middle income",
               "High income")
  )

  Subsampled_Countries$Region <- ordered(Subsampled_Countries$Region,
    levels = c(
      "South Asia",
      "North America",
      "Sub-Saharan Africa",
      "Latin America & Caribbean",
      "Middle East & North Africa",
      "East Asia & Pacific",
      "Europe & Central Asia"
    )
  )

  Subsampled_Countries$ArticleCat <- "PW"
  Subsampled_Region_summary <- Subsampled_Countries %>%
    group_by(author, Dataset, ArticleCat, replicate, Region) %>%
    summarize(n = n()) %>%
    mutate(perc = n / sum(n) * 100)

  PW_medians <- Subsampled_Region_summary %>%
    group_by(author, Dataset, replicate, Region) %>%
    summarise(median = median(perc))
  PW_medians$JrnlType <- "PW"


  
  sole_ALL <- sole_ALL %>% drop_na(IncomeGroup)
  first_ALL <- first_ALL %>% drop_na(IncomeGroup)
  sole_NOCHNUSA <- sole_NOCHNUSA %>% drop_na(IncomeGroup)
  first_NOCHNUSA <- first_NOCHNUSA %>% drop_na(IncomeGroup)

  first_ALL$Dataset <- "All Countries"
  sole_ALL$Dataset <- "All Countries"
  first_ALL$author <- "author_first"
  sole_ALL$author <- "solo"
  sole_NOCHNUSA$Dataset <- "CHN & USA excluded"
  first_NOCHNUSA$Dataset <- "CHN & USA excluded"
  sole_NOCHNUSA$author <- "solo"
  first_NOCHNUSA$author <- "author_first"

  AllPubs <- bind_rows(
    sole_ALL,
    sole_NOCHNUSA,
    first_ALL,
    first_NOCHNUSA
  )

  levels(as.factor(AllPubs$author))
  levels(as.factor(AllPubs$Dataset))
  AllPubs$IncomeGroup <- as.factor(AllPubs$IncomeGroup)
  levels(AllPubs$IncomeGroup)

  OAData <- AllPubs %>%
    filter(ArticleType == "OA") %>%
    drop_na(Region)

  OA_percs <- OAData %>%
    group_by(author, Dataset, ArticleType, Region) %>%
    summarize(n = n()) %>%
    mutate(perc = n / sum(n) * 100)


  OA_percs$Region <- ordered(OA_percs$Region,
    levels = levels(Subsampled_Countries$Region)
  )

  Subsampled_Region_summary_plot <- Subsampled_Region_summary %>%
    filter(author != "author_all")




  # author.labels <- c(author_first = "First Authors", author_last = "Last Authors", solo= "Single Authors")
  author.labels <- c(author_first = "First Authors", solo = "Single Authors")
  color.labels <- c("Low" = "#A6CEE3", 
                    "Lower middle" = "#1F78B4",
                    "Upper middle" = "#B2DF8A",
                    "High" = "#33A02C")
  Subsampled_Region_summary_plot$author <- 
    as.factor(Subsampled_Region_summary_plot$author)
  Subsampled_Region_summary_plot$author <- 
    ordered(Subsampled_Region_summary_plot$author, 
            levels = c("solo", "author_first", "author_last", "author_all"))

  # DATA SELECTION FOR Figure
  
  if (((countries == "All") == TRUE)) {
    fig_data <- Subsampled_Region_summary_plot %>% 
      filter(Dataset == "All Countries")
    label_data <- OA_percs %>% filter(Dataset == "All Countries")
    
  } else if (((countries == "no_CHN_USA") == TRUE)) {
    fig_data <- Subsampled_Region_summary_plot %>% 
      filter(Dataset == "CHN & USA excluded")
    label_data <- OA_percs %>% filter(Dataset == "CHN & USA excluded")
  } else {
    stop("Please enter 'All' or 'no_CHN_USA' ")
  }



  fig_data <- fig_data %>% drop_na(Region)

  Region <- OA_percs$Region

  Region <- recode_factor(Region,
    "South Asia" = "South\nAsia",
    "North America" = "North\nAmerica",
    "Sub-Saharan Africa" = "Sub-Saharan\nAfrica",
    "Latin America & Caribbean" = "Latin America &\nCaribbean",
    "Middle East & North Africa" = "Middle East &\nNorth Africa",
    "East Asia & Pacific" = "East Asia &\nPacific",
    "Europe & Central Asia" = "Europe &\nCentral Asia",
    .default = levels(Region)
  )

  OA_percs$Region <- Region


  Region <- fig_data$Region
  Region <- recode_factor(Region,
    "South Asia" = "South\nAsia",
    "North America" = "North\nAmerica",
    "Sub-Saharan Africa" = "Sub-Saharan\nAfrica",
    "Latin America & Caribbean" = "Latin America &\nCaribbean",
    "Middle East & North Africa" = "Middle East &\nNorth Africa",
    "East Asia & Pacific" = "East Asia &\nPacific",
    "Europe & Central Asia" = "Europe &\nCentral Asia",
    .default = levels(Region)
  )
  fig_data$Region <- Region

  Region <- label_data$Region
  Region <- recode_factor(Region,
    "South Asia" = "South\nAsia",
    "North America" = "North\nAmerica",
    "Sub-Saharan Africa" = "Sub-Saharan\nAfrica",
    "Latin America & Caribbean" = "Latin America &\nCaribbean",
    "Middle East & North Africa" = "Middle East &\nNorth Africa",
    "East Asia & Pacific" = "East Asia &\nPacific",
    "Europe & Central Asia" = "Europe &\nCentral Asia",
    .default = levels(Region)
  )

  label_data$Region <- Region
  label_data <- label_data %>% dplyr::rename("ArticleCat" = "ArticleType")

  fig_data$author <- factor(fig_data$author, levels = c("solo", "author_first"))

  label_data$author <- factor(label_data$author, levels = c("solo", "author_first"))
  
  
  RegionPlot <- ggplot(fig_data, aes(x = perc, fill = Region, Color = ArticleCat)) +
    geom_histogram(
      position = "identity",
      bins = 100,
      color = "black",
      size = 0.5
    ) +
    
    scale_fill_manual(values = c("#084594", "#2171B5", 
                                 "#4292C6", "#6BAED6", 
                                 "#9ECAE1", "#C6DBEF", 
                                 "#DEEBF7")) +
    
    ylab("Frequency") +
    xlab("Percentage of authors in each region") +
    facet_grid(
      cols = vars(author),
      rows = vars(Region),
      scales = ("free_x"),
      labeller = labeller(author = author.labels)
    ) +
    geom_hline((aes(yintercept = -Inf)), color = "black") +
    geom_vline((aes(xintercept = -Inf)), color = "black") +
    scale_y_continuous(limits = c(0, 800), 
                       breaks = seq(0, 800, by = 200), 
                       expand = c(0, 0.1)) +
    
    coord_cartesian(clip = "off") +
    
    

    geom_segment(
      data = subset(filter(label_data, ArticleCat == "OA")),
      aes(
        x = perc,
        y = as.numeric(Region),
        xend = perc,
        yend = 600
      ),
      color = "red", size = 1.5, linetype = "dashed"
    )
  
  


  RegionPlot

  RegionPlot <- RegionPlot +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(colour = "black", size = 25, vjust = -0.5),
      axis.title.y = element_text(colour = "black", size = 25, vjust = 2),
      strip.text.x = element_text(size = 20, margin = margin(5, 0, 3, 0, "lines")),
      strip.text.y = element_text(size = 30, angle = 0),
      strip.background.x = element_rect(fill = NA, colour = NA),
      strip.background.y = element_rect(fill = NA, colour = NA),
      panel.spacing.x = unit(1, "lines"),
      panel.spacing.y = unit(2, "lines"),
      legend.position = "none",
      legend.text = element_text(size = 20),
      plot.margin = unit(c(3, 1, 1, 1.5), "lines") # plot margin - top, right, bottom, left
    )
  facet_labels <- c("A", "B", "C", "D", "E", "F", 
                    "G", "H", "I", "J", "K", "L", "M", "N")
  RegionPlot <- tag_facet(RegionPlot, open = "", 
                          close = "", tag_pool = facet_labels,
                          vjust = 0.5, hjust = -1, size = 10)
  RegionPlot


  #######################################################
  #######################################################
  #######################################################
  # REGION P_hat
  #######################################################
  #######################################################
  label_data <- ungroup(label_data)
  P_Hat <- label_data
  P_Hat$P_Hat <- NA
  n_boot <- BootMirror_RichDiv %>%
    group_by(author, Dataset) %>%
    summarize(n = n())
  n_boot <- max(as.numeric(n_boot$n))

  ##########
  # All countries, coauthored, South Asia
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "OA") %>%
    filter(Region == "South\nAsia") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    filter(Region == "South\nAsia") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)

  perc_belowOA <- perc$perc_belowOA
  perc_belowOA

  P_Hat$P_Hat[P_Hat$Region == "South\nAsia" &
    P_Hat$author == "author_first" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########

  ##########
  # All countries, coauthored, N AM
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "OA") %>%
    filter(Region == "North\nAmerica") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    filter(Region == "North\nAmerica") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$Region == "North\nAmerica" &
    P_Hat$author == "author_first" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########

  ##########
  # All countries, coauthored, SS Africa
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "OA") %>%
    filter(Region == "Sub-Saharan\nAfrica") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    filter(Region == "Sub-Saharan\nAfrica") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$Region == "Sub-Saharan\nAfrica" &
    P_Hat$author == "author_first" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########

  ##########
  # All countries, coauthored, LatAM Carib
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "OA") %>%
    filter(Region == "Latin America &\nCaribbean") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    filter(Region == "Latin America &\nCaribbean") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$Region == "Latin America &\nCaribbean" &
    P_Hat$author == "author_first" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########

  ##########
  # All countries, coauthored, ME North Af
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "OA") %>%
    filter(Region == "Middle East &\nNorth Africa") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    filter(Region == "Middle East &\nNorth Africa") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$Region == "Middle East &\nNorth Africa" &
    P_Hat$author == "author_first" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########

  ##########
  # All countries, coauthored, East Asia Pac
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(ArticleCat == "OA") %>%
    filter(author == "author_first") %>%
    filter(Region == "East Asia &\nPacific") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    filter(Region == "East Asia &\nPacific") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$Region == "East Asia &\nPacific" &
    P_Hat$author == "author_first" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########

  ##########
  # All countries, coauthored, Europe Cent Asia
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "OA") %>%
    filter(Region == "Europe &\nCentral Asia") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(ArticleCat == "PW") %>%
    filter(author == "author_first") %>%
    filter(Region == "Europe &\nCentral Asia") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$Region == "Europe &\nCentral Asia" &
    P_Hat$author == "author_first" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
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
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "OA") %>%
    filter(Region == "South\nAsia") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    filter(Region == "South\nAsia") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$Region == "South\nAsia" &
    P_Hat$author == "solo" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########

  ##########
  # All countries, solo, na am
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "OA") %>%
    filter(Region == "North\nAmerica") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    filter(Region == "North\nAmerica") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$Region == "North\nAmerica" &
    P_Hat$author == "solo" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########

  ##########
  # All countries, solo, SS Africa
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    filter(Region == "Sub-Saharan\nAfrica") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "OA") %>%
    filter(Region == "Sub-Saharan\nAfrica") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$Region == "Sub-Saharan\nAfrica" &
    P_Hat$author == "solo" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########

  ##########
  # All countries, Solo, LatAm Carrib
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "OA") %>%
    filter(Region == "Latin America &\nCaribbean") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    filter(Region == "Latin America &\nCaribbean") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$Region == "Latin America &\nCaribbean" &
    P_Hat$author == "solo" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########

  ##########
  # All countries, solo, mid east n afr
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "OA") %>%
    filter(Region == "Middle East &\nNorth Africa") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    filter(Region == "Middle East &\nNorth Africa") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$Region == "Middle East &\nNorth Africa" &
    P_Hat$author == "solo" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########

  ##########
  # All countries, solo, E asia pac
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(ArticleCat == "OA") %>%
    filter(author == "solo") %>%
    filter(Region == "East Asia &\nPacific") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(ArticleCat == "PW") %>%
    filter(author == "solo") %>%
    filter(Region == "East Asia &\nPacific") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$Region == "East Asia &\nPacific" &
    P_Hat$author == "solo" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########

  ##########
  # All countries, solo, europe C asia
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(ArticleCat == "OA") %>%
    filter(author == "solo") %>%
    filter(Region == "Europe &\nCentral Asia") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    filter(Region == "Europe &\nCentral Asia") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$Region == "Europe &\nCentral Asia" &
    P_Hat$author == "solo" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
 
  

  ###############
  Region <- P_Hat$Region
  Region <- recode_factor(Region,
    "South\nAsia" = "South Asia",
    "North\nAmerica" = "North America",
    "Sub-Saharan\nAfrica" = "Sub-Saharan Africa",
    "Latin America &\nCaribbean" = "Latin America & Caribbean",
    "Middle East &\nNorth Africa" = "Middle East & North Africa",
    "East Asia &\nPacific" = "East Asia & Pacific",
    "Europe &\nCentral Asia" = "Europe & Central Asia",
    .default = levels(Region)
  )
  P_Hat$Region <- Region

  P_Hat <- P_Hat %>%
    dplyr::rename(
      "OA_perc" = "perc",
      "Author" = "author",
      "Countries" = "Dataset"
    ) %>%
    select(-n)


  P_Hat$Author <- gsub("author_first", "First", P_Hat$Author)
  P_Hat$Author <- gsub("solo", "Single", P_Hat$Author)




  return(list(RegionPlot, P_Hat))
}