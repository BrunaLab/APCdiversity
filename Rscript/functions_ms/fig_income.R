fig_income <- function(DataSet, Subsampled_Countries, 
                            countries, sole_ALL, sole_NOCHNUSA,
                            first_ALL, first_NOCHNUSA) {

  
  library(tidyverse)
  library(RColorBrewer)
  library(ggExtra)
  library(egg)




  DataSet <- as.data.frame(DataSet)

  Subsampled_Countries <- as.data.frame(Subsampled_Countries)

  Subsampled_Countries$IncomeGroup <- as.factor(Subsampled_Countries$IncomeGroup)
  Subsampled_Countries$Region <- as.factor(Subsampled_Countries$Region)
  Subsampled_Countries$Code <- as.factor(Subsampled_Countries$Code)
  Subsampled_Countries$Country <- as.factor(Subsampled_Countries$Country)

  DataSet <- DataSet %>%
    
    
    mutate(Dataset = ifelse(Dataset == "all", "All Countries", Dataset)) %>%
    
      mutate(Dataset = ifelse(Dataset == "No_CHN_USA", "CHN & USA excluded", Dataset))
  
  Subsampled_Countries$Dataset <- as.factor(Subsampled_Countries$Dataset)
  Subsampled_Countries <- Subsampled_Countries %>%
    mutate(Dataset = ifelse(Dataset == "all", "All Countries", Dataset)) %>%
  
    mutate(Dataset = ifelse(Dataset == "2", "CHN & USA excluded", Dataset))





  levels(Subsampled_Countries$IncomeGroup)[levels(Subsampled_Countries$IncomeGroup) == "Low income"] <-
    "Low\nIncome"
  levels(Subsampled_Countries$IncomeGroup)[levels(Subsampled_Countries$IncomeGroup) == "Lower middle income"] <-
    "Lower-middle\nIncome"
  levels(Subsampled_Countries$IncomeGroup)[levels(Subsampled_Countries$IncomeGroup) == "Upper middle income"] <-
    "Upper-middle\nIncome"
  levels(Subsampled_Countries$IncomeGroup)[levels(Subsampled_Countries$IncomeGroup) == "High income"] <- 
    "High\nIncome"

  Subsampled_Countries$IncomeGroup <- ordered(Subsampled_Countries$IncomeGroup,
    levels = c("Low\nIncome", 
               "Lower-middle\nIncome", 
               "Upper-middle\nIncome", 
               "High\nIncome")
  )


  Subsampled_Countries$Dataset <- as.factor(Subsampled_Countries$Dataset)
  Subsampled_Countries$ArticleCat <- "PW"
  as.factor(Subsampled_Countries$ArticleCat)

  Subsampled_Income_summary <- Subsampled_Countries %>%
    group_by(author, Dataset, ArticleCat, replicate, IncomeGroup) %>%
    summarize(n = n()) %>%
    mutate(perc = n / sum(n) * 100)

  PW_medians <- Subsampled_Income_summary %>%
    filter(ArticleCat == "PW") %>%
    group_by(author, Dataset, replicate, IncomeGroup) %>%
    summarise(median = median(perc))
  PW_medians$ArticleCat <- "PW"
 
  

  ###################################
  # OA
  ###################################

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

  levels(AllPubs$IncomeGroup)[levels(AllPubs$IncomeGroup) == "Low"] <-
    "Low\nIncome"
  levels(AllPubs$IncomeGroup)[levels(AllPubs$IncomeGroup) == "Lower middle"] <-
    "Lower-middle\nIncome"
  levels(AllPubs$IncomeGroup)[levels(AllPubs$IncomeGroup) == "Upper middle"] <- 
    "Upper-middle\nIncome"
  levels(AllPubs$IncomeGroup)[levels(AllPubs$IncomeGroup) == "High"] <- 
    "High\nIncome"
  levels(as.factor(AllPubs$IncomeGroup))
  levels(as.factor(AllPubs$author))

  OAData <- AllPubs %>% filter(ArticleType == "OA")
 
  
  OA_percs <- OAData %>%
    group_by(author, Dataset, IncomeGroup) %>%
    summarize(n = n()) %>%
    mutate(perc = n / sum(n) * 100)

  OA_percs$IncomeGroup <- ordered(OA_percs$IncomeGroup,
    levels = c("Low\nIncome", 
               "Lower-middle\nIncome", 
               "Upper-middle\nIncome", 
               "High\nIncome")
  )
  OA_percs$color <- NA

  OA_percs$color[OA_percs$IncomeGroup == "Low\nIncome"] <- "'#A6CEE3'"
  OA_percs$color[OA_percs$IncomeGroup == "Lower-middle\nIncome"] <- "'#1F78B4'"
  OA_percs$color[OA_percs$IncomeGroup == "Upper-middle\nIncome"] <- "'#B2DF8A'"
  OA_percs$color[OA_percs$IncomeGroup == "High\nIncome"] <- "'#33A02C'"



  Subsampled_Income_summary_plot <- Subsampled_Income_summary %>%
    filter(author != "author_all")
  Subsampled_Income_summary_plot <- Subsampled_Income_summary_plot %>% drop_na(IncomeGroup)





  author.labels <- c(author_first = "First Authors", solo = "Single Authors")
  color.labels <- c("Low\nIncome" = "#A6CEE3", 
                    "Lower-middle\nIncome" = "#1F78B4",
                    "Upper-middle\nIncome" = "#B2DF8A",
                    "High\nIncome" = "#33A02C")
  Subsampled_Income_summary_plot$author <- 
    as.factor(Subsampled_Income_summary_plot$author)
  Subsampled_Income_summary_plot$author <- 
    ordered(Subsampled_Income_summary_plot$author, 
            levels = c("solo", "author_first"))
  #
  # DATA SELECTION FOR Figure

  
  if (((countries == "All") == TRUE)) {
    fig_data <- Subsampled_Income_summary_plot %>% 
      filter(Dataset == "All Countries")
    label_data <- OA_percs %>% filter(Dataset == "All Countries")
  
  } else if (((countries == "No_CHN_USA") == TRUE)) {
    fig_data <- Subsampled_Income_summary_plot %>% 
      filter(Dataset == "CHN & USA excluded")
    label_data <- OA_percs %>% 
      filter(Dataset == "CHN & USA excluded")
  } else {
    stop("Please enter 'All' or 'No_CHN_USA' ")
  }

  
  Subsampled_Income_summary_plot$author <- 
    factor(Subsampled_Income_summary_plot$author,
    levels = c("solo", "author_first")
  )


  
  label_data$author <- factor(label_data$author, 
                              levels = c("solo", "author_first"))
  label_data$ArticleCat <- "OA"

  
  IncomePlot <- ggplot(Subsampled_Income_summary_plot, aes(
    x = perc,
    fill = IncomeGroup,
    color = ArticleCat
  )) +
    geom_histogram(
      bins = 100,
      color = "black",
      size = 0.5,
      position = "identity"
    ) +
    
    ylab("Frequency") +
    xlab("Percentage of authors in each World Bank Lending Group") +
    
    
    scale_fill_manual(
      values = c("#F7FBFF", "#9ECAE1", "#4292C6", "#084594"),
      name = "National Income Category",
      breaks = c("High\nIncome", 
                 "Upper-middle\nIncome", 
                 "Lower-middle\nIncome",
                 "Low\nIncome")
    ) +

    
    labs(fill = "National Income Group") +
    facet_grid(
      cols = vars(author),
      rows = vars(IncomeGroup),
      labeller = labeller(author = author.labels),
      scales = "free"
    ) +
    geom_hline((aes(yintercept = -Inf)), color = "black") +
    geom_vline((aes(xintercept = -Inf)), color = "black") +
    coord_cartesian(clip = "off") +
   
    
    ## OA POINTS LINES AND LABELS
    geom_segment(
      data = subset(filter(label_data, ArticleCat == "OA")),
      aes(
        x = perc,
        y = as.numeric(IncomeGroup) - 2,
        xend = perc,
        yend = 350
      ),
      color = "red", size = 1.5, linetype = "dashed"
    )
 
  

  IncomePlot <- IncomePlot +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(colour = "black", size = 25, vjust = -0.5),
      axis.title.y = element_text(colour = "black", size = 25, vjust = 2),
      strip.text.x = element_text(size = 30, margin = margin(5, 0, 3, 0, "lines")),
      strip.text.y = element_text(size = 30, angle = 0),
      strip.background.x = element_rect(fill = NA, colour = NA),
      strip.background.y = element_rect(fill = NA, colour = NA),
      panel.spacing.x = unit(2, "lines"),
      panel.spacing.y = unit(4, "lines"),
      legend.position = "none",
      legend.text = element_text(size = 20),
      
    )
  facet_labels <- c("A", "B", "C", "D", "E", "F", "G", "H")
  IncomePlot <- tag_facet(IncomePlot, open = "", close = "", 
                          tag_pool = facet_labels, 
                          vjust = 0.5, hjust = -1, size = 10)
  IncomePlot

  # P_HAT

  # single, all
  label_data <- ungroup(label_data)
  P_Hat <- label_data
  P_Hat$P_Hat <- NA
  P_Hat$color <- NULL
  n_boot <- BootMirror_RichDiv %>%
    group_by(author, Dataset) %>%
    summarize(n = n())
  nboot <- as.numeric(max(as.numeric(n_boot$n)))

  ##########
  # All countries, coauthored, High
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    # filter(ArticleCat=="OA") %>%
    filter(IncomeGroup == "High\nIncome") %>%
    select(perc)

  perc <- Subsampled_Income_summary_plot %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    filter(IncomeGroup == "High\nIncome") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / nboot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA

  P_Hat$P_Hat[P_Hat$IncomeGroup == "High\nIncome" &
    P_Hat$author == "author_first" &
    # P_Hat$ArticleCat=="OA"&
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########

  ##########
  # All countries, coauthored, Lower-middle\nIncome
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    # filter(ArticleCat=="OA") %>%
    filter(IncomeGroup == "Lower-middle\nIncome") %>%
    select(perc)

  perc <- Subsampled_Income_summary_plot %>%
    filter(Dataset == "All Countries") %>%
    filter(ArticleCat == "PW") %>%
    filter(author == "author_first") %>%
    filter(IncomeGroup == "Lower-middle\nIncome") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / nboot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA


  P_Hat$P_Hat[P_Hat$IncomeGroup == "Lower-middle\nIncome" &
    P_Hat$author == "author_first" &
    # P_Hat$ArticleCat=="OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA

  ###########

  ##########

  # All countries, coauthored, Upper-middle\nIncome
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    # filter(ArticleCat=="OA") %>%
    filter(IncomeGroup == "Upper-middle\nIncome") %>%
    select(perc)

  perc <- Subsampled_Income_summary_plot %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    filter(IncomeGroup == "Upper-middle\nIncome") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / nboot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA


  P_Hat$P_Hat[P_Hat$IncomeGroup == "Upper-middle\nIncome" &
    P_Hat$author == "author_first" &
    # P_Hat$ArticleCat=="OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########


  ##########
  # All countries, coauthored, Low\nIncome
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    # filter(ArticleCat=="OA") %>%
    filter(IncomeGroup == "Low\nIncome") %>%
    select(perc)

  perc <- Subsampled_Income_summary_plot %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    filter(IncomeGroup == "Low\nIncome") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / nboot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$IncomeGroup == "Low\nIncome" &
    P_Hat$author == "author_first" &
    # P_Hat$ArticleCat=="OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########

  ##########
  # All countries, coauthored, High
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    # filter(ArticleCat=="OA") %>%
    filter(IncomeGroup == "High\nIncome") %>%
    select(perc)

  perc <- Subsampled_Income_summary_plot %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    filter(IncomeGroup == "High\nIncome") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / nboot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA

  P_Hat$P_Hat[P_Hat$IncomeGroup == "High\nIncome" &
    P_Hat$author == "solo" &
    # P_Hat$ArticleCat=="OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########

  ##########
  # All countries, coauthored, Lower-middle\nIncome
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    # filter(ArticleCat=="OA") %>%
    filter(IncomeGroup == "Lower-middle\nIncome") %>%
    select(perc)

  perc <- Subsampled_Income_summary_plot %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    filter(IncomeGroup == "Lower-middle\nIncome") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / nboot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA


  P_Hat$P_Hat[P_Hat$IncomeGroup == "Lower-middle\nIncome" &
    P_Hat$author == "solo" &
    # P_Hat$ArticleCat=="OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA

  ###########

  ##########

  # All countries, coauthored, Upper-middle\nIncome
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    # filter(ArticleCat=="OA") %>%
    filter(IncomeGroup == "Upper-middle\nIncome") %>%
    select(perc)

  perc <- Subsampled_Income_summary_plot %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    filter(IncomeGroup == "Upper-middle\nIncome") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / nboot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA


  P_Hat$P_Hat[P_Hat$IncomeGroup == "Upper-middle\nIncome" &
    P_Hat$author == "solo" &
    # P_Hat$ArticleCat=="OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########


  ##########
  # All countries, coauthored, Low\nIncome
  crit <- label_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    # filter(ArticleCat=="OA") %>%
    filter(IncomeGroup == "Low\nIncome") %>%
    select(perc)

  perc <- Subsampled_Income_summary_plot %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    filter(IncomeGroup == "Low\nIncome") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / nboot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$IncomeGroup == "Low\nIncome" &
    P_Hat$author == "solo" &
    # P_Hat$ArticleCat=="OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA
  ###########
  
  
  ###########
  P_Hat

  levels(P_Hat$IncomeGroup)[levels(P_Hat$IncomeGroup) == 
                              "Low\nIncome"] <- "Low"
  levels(P_Hat$IncomeGroup)[levels(P_Hat$IncomeGroup) ==
                              "Lower-middle\nIncome"] <- "Lower middle"
  levels(P_Hat$IncomeGroup)[levels(P_Hat$IncomeGroup) == 
                              "Upper-middle\nIncome"] <- "Upper middle"
  levels(P_Hat$IncomeGroup)[levels(P_Hat$IncomeGroup) ==
                              "High\nIncome"] <- "High"


  P_Hat <- P_Hat %>%
    dplyr::rename(
      "OA_perc" = "perc",
      "Author" = "author",
      "Countries" = "Dataset"
    ) %>%
    select(-n)


  P_Hat$Author <- gsub("author_first", "First", P_Hat$Author)
  P_Hat$Author <- gsub("solo", "Single", P_Hat$Author)

  P_Hat <- filter(P_Hat, P_Hat > -1)

  return(list(IncomePlot, P_Hat))
}