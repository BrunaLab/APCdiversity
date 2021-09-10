fig_div_boot <- function(Boot_RichDiv, sole_ALL, 
                            sole_NOCHNUSA, first_ALL, first_NOCHNUSA) {
  # Boot_RichDiv<-BootMirror_RichDiv
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(RColorBrewer)
  library(egg)
  levels(as.factor(Boot_RichDiv$Dataset))
  Boot_RichDiv <- Boot_RichDiv %>%
    mutate(Dataset = ifelse(Dataset == 
                              "all countries", 
                            "All Countries", 
                            Dataset)) %>%
    mutate(Dataset = ifelse(Dataset ==
                              "CHN & USA excluded", 
                            "China & USA excluded", 
                            Dataset))



  source("./Rscript/functions_ms/DivRichCalc.R")

  # OA Mirror

  OAdiv_first_ALL_OAmirror <- DivRichCalc(first_ALL, 
                                          "author_first", "OA", "OA")
  OAdiv_first_ALL_OAmirror <- as.numeric(OAdiv_first_ALL_OAmirror[2])

  OAdiv_first_NOCHNUSA_OAmirror <- DivRichCalc(first_NOCHNUSA,
                                               "author_first", "OA", "OA")
  OAdiv_first_NOCHNUSA_OAmirror <- as.numeric(OAdiv_first_NOCHNUSA_OAmirror[2])

  OAdiv_sole_ALL_OAmirror <- DivRichCalc(sole_ALL, 
                                         "author_first", "OA", "OA")
  OAdiv_sole_ALL_OAmirror <- as.numeric(OAdiv_sole_ALL_OAmirror[2])

  OAdiv_sole_NOCHNUSA_OAmirror <- DivRichCalc(sole_NOCHNUSA,
                                              "author_first", "OA", "OA")
  OAdiv_sole_NOCHNUSA_OAmirror <- as.numeric(OAdiv_sole_NOCHNUSA_OAmirror[2])

  # OA - Parent

  OAdiv_first_ALL_OAparent <- DivRichCalc(first_ALL,
                                          "author_first", "PW", "OA")
  OAdiv_first_ALL_OAparent <- as.numeric(OAdiv_first_ALL_OAparent[2])

  OAdiv_first_NOCHNUSA_OAparent <- DivRichCalc(first_NOCHNUSA, 
                                               "author_first", "PW", "OA")
  OAdiv_first_NOCHNUSA_OAparent <- as.numeric(OAdiv_first_NOCHNUSA_OAparent[2])

  OAdiv_sole_ALL_OAparent <- DivRichCalc(sole_ALL,
                                         "author_first", "PW", "OA")
  OAdiv_sole_ALL_OAparent <- as.numeric(OAdiv_sole_ALL_OAparent[2])

  OAdiv_sole_NOCHNUSA_OAparent <- DivRichCalc(sole_NOCHNUSA, 
                                              "author_first", "PW", "OA")
  OAdiv_sole_NOCHNUSA_OAparent <- as.numeric(OAdiv_sole_NOCHNUSA_OAparent[2])




  author <- rep(c("author_first", "author_first", "solo", "solo"), 2)
  Dataset <- rep(c("All Countries", "China & USA excluded"), 4)
  # OA_articleType<-c(rep("allOA",4),rep("mirror",4),rep("OAinPW",4))
  OA_articleType <- c(rep("OA-mirror", 4), rep("Parent", 4))

  OA_Diversity <- c(
    OAdiv_first_ALL_OAmirror,
    OAdiv_first_NOCHNUSA_OAmirror,
    OAdiv_sole_ALL_OAmirror,
    OAdiv_sole_NOCHNUSA_OAmirror,
    OAdiv_first_ALL_OAparent,
    OAdiv_first_NOCHNUSA_OAparent,
    OAdiv_sole_ALL_OAparent,
    OAdiv_sole_NOCHNUSA_OAparent
  )


  OA_Diversity <- bind_cols(OA_Diversity, author, Dataset, OA_articleType)
  names(OA_Diversity) <- c("value", "author", "Dataset", "Articles_Value")

  means_bootstrap_results <- Boot_RichDiv %>%
    group_by(author, Dataset, BootType) %>%
    summarize(mean(InvSimp)) %>%
    mutate(Dataset = ifelse(Dataset == "all", "All Countries", Dataset)) %>%
    mutate(Dataset = ifelse(Dataset == 
                              "no_CHN_USA", "China & USA excluded", Dataset))



  # means_bootstrap_results$BootType<-gsub("MirrorvOAinPW","Mirror",means_bootstrap_results$BootType)
  # means_bootstrap_results$BootType<-gsub("OAinPWvPW","OAinPW",means_bootstrap_results$BootType)
  #
  figure_values <- inner_join(means_bootstrap_results, 
                              OA_Diversity, by = c("author", "Dataset"))
  figure_values <- figure_values %>%
    select(author, Dataset, "ArticleCat" = Articles_Value, 
           "mean_Div" = `mean(InvSimp)`, "OA_Div" = value)
  # names(figure_values)<-c("author","Dataset", "ArticleCat","mean_Div", "OA_Div")

  # figure_values<-figure_values %>% filter(ArticleCat=="allOA")
  figure_values$ArticleCat2 <- figure_values$ArticleCat
  figure_values$ArticleCat <- rep(c("PW"), 8)
  # figure_values$ArticleCat<-rep(c("PW","OAinPW"),4)

  figure_values$mean_Div <- round(figure_values$mean_Div, digits = 1)
  figure_values$OA_Div <- round(figure_values$OA_Div, digits = 1)
  summary(as.factor(Boot_RichDiv$author))
  summary(as.factor(Boot_RichDiv$Dataset))
  Boot_RichDiv$ArticleCat <- "PW"
  summary(as.factor(Boot_RichDiv$ArticleCat))

  yWO_Div <- 460
  yALL_Div <- 460
  ySolo_Div <- 460
  author.labels <- c(author_first = "First Authors", solo = "Single Authors")

  figure_values$author <- as.factor(figure_values$author)

  #
  Boot_RichDiv$author <- as.factor(Boot_RichDiv$author)
  #
  Boot_RichDiv$author <- factor(Boot_RichDiv$author,
    levels = c("solo", "author_first")
  )
  #

  #
  figure_values$author <- factor(figure_values$author,
    levels = c("solo", "author_first")
  )
  #



  pDiv <-
    ggplot(Boot_RichDiv, aes(x = InvSimp, fill = ArticleCat)) +
    geom_histogram(
      bins = 40, color = "black",
      # fill="darkgray",
      size = 0.1, alpha = 0.6, position = "identity"
    ) +
    facet_grid(
      cols = vars(Dataset), rows = vars(author),
      labeller = labeller(author = author.labels),
      scales = "free_y"
    ) +
    scale_fill_manual(values = c("gray65", "black", "white")) +
    # scale_fill_manual(values=c("darkgray", "red4"))+
    # scale_fill_brewer(palette = "greys")+
    geom_hline((aes(yintercept = -Inf)), color = "black") +
    geom_vline((aes(xintercept = -Inf)), color = "black") +
    guides(fill = guide_legend("Article\nCat.")) +
    scale_x_continuous(breaks = seq(0, 30, by = 5), 
                       expand = c(0.1, 0.02)) +
    scale_y_continuous(limits = c(0, 600), 
                       breaks = seq(0, 600, by = 100), expand = c(0, 0.1)) +
    # scale_y_continuous(expand = c(0,0))+
    xlab("Geographic Diversity, D (Reciprocal of Simpson's Index)") +
    labs(x = "Geographic Diversity", y = "Frequency") +
    coord_cartesian(clip = "off") +
   
    
    geom_segment(
      data = subset(filter(figure_values, 
                           author == "author_first" & 
                             Dataset == "All Countries" & 
                             ArticleCat2 == "OA-mirror")),
      aes(x = OA_Div, y = 0, xend = OA_Div, yend = yALL_Div),
      colour = "red", linetype = "dashed"
    ) +
  
    
    geom_segment(
      data = subset(filter(figure_values, author == "solo" &
                             Dataset == "All Countries" &
                             ArticleCat2 == "OA-mirror")),
      aes(x = OA_Div, y = 0, xend = OA_Div, yend = ySolo_Div),
      colour = "red", linetype = "dashed"
    ) +
  
    
    geom_segment(
      data = subset(filter(figure_values, author == "author_first" & 
                             Dataset == "China & USA excluded" & 
                             ArticleCat2 == "OA-mirror")),
      aes(x = OA_Div, y = 0, xend = OA_Div, yend = yWO_Div),
      colour = "red", linetype = "dashed"
    ) +
  
    
    geom_segment(
      data = subset(filter(figure_values, author == "solo" &
                             Dataset == "China & USA excluded" & 
                             ArticleCat2 == "OA-mirror")),
      aes(x = OA_Div, y = 0, xend = OA_Div, yend = ySolo_Div - 10),
      colour = "red", linetype = "dashed"
    )
 
  
  pDiv <- pDiv +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(colour = "black", size = 10, vjust = -0.5),
      axis.title.y = element_text(colour = "black", size = 10, hjust = 0.5, ),
      strip.text.x = element_text(size = 10, 
                                  margin = margin(0, 0, 3, 0, "lines")),
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
  pDiv <- tag_facet(pDiv, open = "", close = "",
                    tag_pool = facet_labels, vjust = -1)
  pDiv

  
  return(pDiv)
  # return(list(pDiv,P_Hat))
}
