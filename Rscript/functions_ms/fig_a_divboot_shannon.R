fig_a_divboot_shannon <- function(BootMirror_RichDiv,
                               sole_ALL,
                               first_ALL,
                               sole_NOCHNUSA,
                               first_NOCHNUSA) {
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(RColorBrewer)
  library(egg)

  source("./Rscript/functions_ms/DivRichCalc.R")
  OAdiv_coauthor_ALL <- DivRichCalc(first_ALL, "author_first", "both", "OA")
  OAdiv_coauthor_ALL <- as.numeric(OAdiv_coauthor_ALL[4])

  OAdiv_coauthor_NOCHNUSA <- DivRichCalc(first_NOCHNUSA, "author_first", "both", "OA")
  OAdiv_coauthor_NOCHNUSA <- as.numeric(OAdiv_coauthor_NOCHNUSA[4])

  OAdiv_sole_ALL <- DivRichCalc(sole_ALL, "author_first", "both", "OA")
  OAdiv_sole_ALL <- as.numeric(OAdiv_sole_ALL[4])

  OAdiv_sole_NOCHNUSA <- DivRichCalc(sole_NOCHNUSA, "author_first", "both", "OA")
  OAdiv_sole_NOCHNUSA <- as.numeric(OAdiv_sole_NOCHNUSA[4])

  OA_Diversity <- c(
    OAdiv_coauthor_ALL, OAdiv_coauthor_NOCHNUSA,
    OAdiv_sole_ALL, OAdiv_sole_NOCHNUSA
  )
  OA_Diversity <- as_tibble(OA_Diversity)
  OA_Diversity <- dplyr::rename(OA_Diversity, "OA_Diversity" = "value")

  means_Boot_RichDiv <- BootMirror_RichDiv %>%
    group_by(author, Dataset) %>%
    summarize(mean(Shannon))

  figure_values <- bind_cols(means_Boot_RichDiv, OA_Diversity)
  figure_values
  names(figure_values) <- c("author", "Dataset", "mean_Div", "OA_Div")
  figure_values$JrnlType <- "PW"
  figure_values$mean_Div <- round(figure_values$mean_Div, digits = 1)
  figure_values$OA_Div <- round(figure_values$OA_Div, digits = 1)
  summary(as.factor(BootMirror_RichDiv$author))
  summary(as.factor(BootMirror_RichDiv$Dataset))
  summary(as.factor(BootMirror_RichDiv$JrnlType))

  yWO_Div <- 275
  yALL_Div <- 275
  ySolo_Div <- 275
  author.labels <- c(author_first = "First Authors", solo = "Single Authors")

  BootMirror_RichDiv$Dataset <- gsub("all", "All Countries", BootMirror_RichDiv$Dataset)
  BootMirror_RichDiv$Dataset <- gsub("no_CHN_USA", "Without China & USA", BootMirror_RichDiv$Dataset)
  figure_values$Dataset <- gsub("no_CHN_USA", "Without China & USA", figure_values$Dataset)
  figure_values$Dataset <- gsub("all", "All Countries", figure_values$Dataset)

  BootMirror_RichDiv$author <- factor(BootMirror_RichDiv$author,
    levels = c("solo", "author_first")
  )
  figure_values$author <- factor(figure_values$author,
    levels = c("solo", "author_first")
  )



  pDiv <-
    ggplot(BootMirror_RichDiv, aes(x = Shannon, fill = JrnlType)) +
    geom_histogram(
      bins = 50, color = "black", fill = "darkgray",
      size = 0.1, alpha = 0.4, position = "identity"
    ) +
    facet_grid(
      cols = vars(Dataset), rows = vars(author),
      labeller = labeller(author = author.labels),
      scales = "free_y"
    ) +
    xlim(2.5, 4) +
    # ylim(0,400)+
    # scale_x_continuous(breaks = seq(0,70, by=10),expand=c(0.1,0.02))+
    scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 50), expand = c(0, 0.1)) +
    #
    geom_hline((aes(yintercept = -Inf)), color = "black") +
    geom_vline((aes(xintercept = -Inf)), color = "black") +
    coord_cartesian(clip = "off") +
    
    geom_segment(
      data = subset(filter(figure_values, author == "author_first" & Dataset == "All Countries")),
      aes(x = OA_Div, y = 0, xend = OA_Div, yend = yALL_Div),
      colour = "red", linetype = "solid"
    ) +
   

    geom_segment(
      data = subset(filter(figure_values, author == "solo" & Dataset == "All Countries")),
      aes(x = OA_Div, y = 0, xend = OA_Div, yend = ySolo_Div),
      colour = "red", linetype = "solid"
    ) +
    
    
    geom_segment(
      data = subset(filter(figure_values, author == "author_first" & Dataset == "CHN & USA excluded")),
      aes(x = OA_Div, y = 0, xend = OA_Div, yend = yWO_Div),
      colour = "red", linetype = "solid"
    ) +
   
    geom_segment(
      data = subset(filter(figure_values, author == "solo" & Dataset == "CHN & USA excluded")),
      aes(x = OA_Div, y = 0, xend = OA_Div, yend = ySolo_Div),
      colour = "red", linetype = "solid"
    ) +

    guides(fill = guide_legend("Journal\nCategory")) +
    xlab("Geographic Diversity (Shannon Index)") +
    ylab("Frequency")


  pDiv <- pDiv +
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
  pDiv <- tag_facet(pDiv, open = "", close = "", tag_pool = facet_labels, vjust = -1)
  pDiv
  return(pDiv)
}
