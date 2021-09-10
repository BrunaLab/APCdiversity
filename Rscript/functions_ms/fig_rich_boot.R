fig_rich_boot <- function(Boot_RichDiv, sole_ALL, first_ALL, sole_NOCHNUSA, first_NOCHNUSA) {
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(RColorBrewer)
  library(egg)
  # Boot_RichDiv<-BootMirror_RichDiv
  Boot_RichDiv <- Boot_RichDiv %>%
    mutate(Dataset = ifelse(Dataset == "all", "All Countries", Dataset)) %>%
    mutate(Dataset = ifelse(Dataset == "no_CHN_USA", "CHN & USA excluded", Dataset))

  source("./Rscript/functions_ms/DivRichCalc.R")
  OArich_first_ALL_OApool <- DivRichCalc(first_ALL, "author_first", "both", "OA")
  OArich_first_ALL_OApool <- as.numeric(OArich_first_ALL_OApool[1])

  OArich_first_NOCHNUSA_OApool <- DivRichCalc(first_NOCHNUSA, "author_first", "both", "OA")
  OArich_first_NOCHNUSA_OApool <- as.numeric(OArich_first_NOCHNUSA_OApool[1])

  OArich_sole_ALL_OApool <- DivRichCalc(sole_ALL, "author_first", "both", "OA")
  OArich_sole_ALL_OApool <- as.numeric(OArich_sole_ALL_OApool[1])

  OArich_sole_NOCHNUSA_OApool <- DivRichCalc(sole_NOCHNUSA, "author_first", "both", "OA")
  OArich_sole_NOCHNUSA_OApool <- as.numeric(OArich_sole_NOCHNUSA_OApool[1])

  


  author <- rep(c("author_first", "author_first", "solo", "solo"), 1)
  Dataset <- rep(c("All Countries", "CHN & USA excluded"), 2)
  
  
  OA_articleType <- c(rep("allOA", 4))

  OA_Richness <- c(
    OArich_first_ALL_OApool,
    OArich_first_NOCHNUSA_OApool,
    OArich_sole_ALL_OApool,
    OArich_sole_NOCHNUSA_OApool
  )
  
  
  OA_Richness <- bind_cols(OA_Richness, author, Dataset, OA_articleType)
  names(OA_Richness) <- c("value", "author", "Dataset", "Articles_Value")

  means_bootstrap_results <- Boot_RichDiv %>%
    group_by(author, Dataset, BootType) %>%
    summarize(mean(Richness))



  
  figure_values <- inner_join(means_bootstrap_results, OA_Richness, by = c("author", "Dataset"))
  figure_values <- figure_values %>%
    select(author, Dataset, "ArticleCat" = Articles_Value, "mean_Rich" = `mean(Richness)`, "OA_Richness" = value)
  
  
  figure_values$ArticleCat <- rep(c("PW"), 4)
  
  
  figure_values$mean_Rich <- round(figure_values$mean_Rich, digits = 1)
  figure_values$OA_Richness <- round(figure_values$OA_Richness, digits = 1)
  summary(as.factor(Boot_RichDiv$author))
  summary(as.factor(Boot_RichDiv$Dataset))
  Boot_RichDiv$ArticleCat <- "PW"
  summary(as.factor(Boot_RichDiv$ArticleCat))



  yWO_Rich <- 275
  yALL_Rich <- 275
  ySolo_Rich <- 300


  author.labels <- c(author_first = "First Authors", solo = "Single Authors")
 
  
  Boot_RichDiv$author <- factor(Boot_RichDiv$author,
    levels = c("solo", "author_first")
  )


  figure_values$author <- as.factor(figure_values$author)

  
  figure_values$Dataset <- as.factor(figure_values$Dataset)
  
  
  pRich <-
    ggplot(Boot_RichDiv, aes(x = Richness, fill = ArticleCat)) +
    geom_histogram(
      bins = 40, color = "black",
      # fill="darkgray",
      size = 0.1, alpha = 0.6, position = "identity"
    ) +
    facet_grid(
      cols = vars(Dataset),
      rows = vars(author),
      labeller = labeller(author = author.labels),
      scales = "free_y"
    ) +
    geom_hline((aes(yintercept = -Inf)), color = "black") +
    geom_vline((aes(xintercept = -Inf)), color = "black") +
    scale_fill_manual(values = c("gray65", "black")) +
    guides(fill = guide_legend("Article\nCat.")) +
    scale_x_continuous(breaks = seq(0, 90, by = 10), expand = c(0.1, 0.02)) +
    scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, by = 50), expand = c(0, 0.1)) +
    # scale_y_continuous(expand = c(0,0))+
    xlab("Geographic Richness (R)") +
    ylab("Frequency") +
    coord_cartesian(clip = "off") +
    
    
    # OA Value
    geom_segment(
      data = subset(filter(figure_values, author == "author_first" & Dataset == "All Countries" & ArticleCat == "PW")),
      aes(x = OA_Richness, y = 0, xend = OA_Richness, yend = yALL_Rich),
      colour = "red", linetype = "solid"
    ) +
    geom_text(
      data = subset(filter(figure_values, author == "author_first" & Dataset == "All Countries" & ArticleCat == "PW")),
      aes(x = OA_Richness + 0, y = yALL_Rich + 15, label = (paste("OA", as.character(OA_Richness), sep = " == "))),
      parse = TRUE, color = "red", size = 2
    ) +

    
    geom_segment(
      data = subset(filter(figure_values, author == "solo" & Dataset == "All Countries" & ArticleCat == "PW")),
      aes(x = OA_Richness, y = 0, xend = OA_Richness, yend = ySolo_Rich),
      colour = "red", linetype = "solid"
    ) +
    geom_text(
      data = subset(filter(figure_values, author == "solo" & Dataset == "All Countries" & ArticleCat == "PW")),
      aes(x = OA_Richness - 0, y = ySolo_Rich + 15, label = (paste("OA", as.character(OA_Richness), sep = " == "))),
      parse = TRUE, color = "red", size = 2
    ) +

    
    geom_segment(
      data = subset(filter(figure_values, author == "author_first" & Dataset == "CHN & USA excluded" & ArticleCat == "PW")),
      aes(x = OA_Richness, y = 0, xend = OA_Richness, yend = yWO_Rich),
      colour = "red", linetype = "solid"
    ) +
    geom_text(
      data = subset(filter(figure_values, author == "author_first" & Dataset == "CHN & USA excluded" & ArticleCat == "PW")),
      aes(x = OA_Richness - 0, y = yWO_Rich + 15, label = (paste("OA", as.character(OA_Richness), sep = " == "))),
      parse = TRUE, color = "red", size = 2
    ) +
   
    
    geom_segment(
      data = subset(filter(figure_values, author == "solo" & Dataset == "CHN & USA excluded" & ArticleCat == "PW")),
      aes(x = OA_Richness, y = 0, xend = OA_Richness, yend = ySolo_Rich - 10),
      colour = "red", linetype = "solid"
    ) +
    geom_text(
      data = subset(filter(figure_values, author == "solo" & Dataset == "CHN & USA excluded" & ArticleCat == "PW")),
      aes(x = OA_Richness - 0, y = ySolo_Rich + 15, label = (paste("OA", as.character(OA_Richness), sep = " == "))),
      parse = TRUE, color = "red", size = 2
    )

  pRich <- pRich +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(colour = "black", size = 10, vjust = -0.5),
      axis.title.y = element_text(colour = "black", size = 10, hjust = 0.5, ),
      strip.text.x = element_text(size = 10, margin = margin(0, 0, 3, 0, "lines")),
      strip.text.y = element_text(size = 10, angle = 0),
      strip.background.y = element_rect(fill = NA, colour = NA),
      strip.background.x = element_rect(fill = NA, colour = NA),
      legend.title = element_text(colour = "black", size = 60),
      legend.text = element_text(colour = "black", size = 60),
      legend.position = ("none"),
      panel.spacing.x = unit(1.0, "lines"),
      panel.spacing.y = unit(2, "lines"),
      # strip.text.y.right = element_text(angle = 0),
      plot.margin = unit(c(1, 1, 1, 1.5), "lines")
    )

  facet_labels <- c("A", "B", "C", "D")
  pRich <- tag_facet(pRich, open = "", close = "", tag_pool = facet_labels, vjust = -1)
  pRich






  ##################################################################################
  # RICHNESS
  ############################################################################
  n_boot <- Boot_RichDiv %>%
    group_by(author, Dataset) %>%
    summarize(n = n())
  boot_runs <- max(as.numeric(n_boot$n))
  figure_values <- ungroup(figure_values)
  P_Hat <- figure_values
  P_Hat$P_Hat <- NA
  P_Hat$JrnlType <- NULL
  ##########
  # P-HAT for All countries, coauthored, PW
  crit <- figure_values %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    select(OA_Richness)

  perc <- Boot_RichDiv %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    ungroup() %>%
    tally(Richness < crit$OA_Richness) %>%
    mutate(perc_belowOA = n / boot_runs)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA

  P_Hat$P_Hat[P_Hat$author == "author_first" &
    P_Hat$Dataset == "All Countries" &
    P_Hat$ArticleCat == "PW"] <- perc_belowOA
  ###########


  ##########
  # # P-HAT without USA CHN, coauthored, PW
  crit <- figure_values %>%
    filter(Dataset == "CHN & USA excluded") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    select(OA_Richness)

  perc <- Boot_RichDiv %>%
    filter(Dataset == "CHN & USA excluded") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    ungroup() %>%
    tally(Richness < crit$OA_Richness) %>%
    mutate(perc_belowOA = n / boot_runs)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA

  P_Hat$P_Hat[P_Hat$author == "author_first" &
    P_Hat$Dataset == "CHN & USA excluded" &
    P_Hat$ArticleCat == "PW"] <- perc_belowOA
  ###########

  ##########
  # # P-HAT All countries, coauthored, PW
  crit <- figure_values %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    select(OA_Richness)

  perc <- Boot_RichDiv %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    ungroup() %>%
    tally(Richness < crit$OA_Richness) %>%
    mutate(perc_belowOA = n / boot_runs)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA

  P_Hat$P_Hat[P_Hat$author == "solo" &
    P_Hat$Dataset == "All Countries" &
    P_Hat$ArticleCat == "PW"] <- perc_belowOA
  ###########


  ##########
  # # P-HAT without USA CHN, coauthored, PW
  crit <- figure_values %>%
    filter(Dataset == "CHN & USA excluded") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    select(OA_Richness)

  perc <- Boot_RichDiv %>%
    filter(Dataset == "CHN & USA excluded") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    ungroup() %>%
    tally(Richness < crit$OA_Richness) %>%
    mutate(perc_belowOA = n / boot_runs)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA

  P_Hat$P_Hat[P_Hat$author == "solo" &
    P_Hat$Dataset == "CHN & USA excluded" &
    P_Hat$ArticleCat == "PW"] <- perc_belowOA

  P_Hat <- P_Hat %>% arrange(Dataset, desc(author))

 
  
  P_Hat <- P_Hat %>% arrange(Dataset, desc(author))

  #########

  return(list(pRich, P_Hat))
}
