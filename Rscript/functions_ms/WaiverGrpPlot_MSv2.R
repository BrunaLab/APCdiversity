WaiverGrpPlot_MSv2 <- function(DataSet,
                               Subsampled_Countries,
                               countries,
                               sole_ALL,
                               sole_NOCHNUSA,
                               first_ALL,
                               first_NOCHNUSA,
                               WaiverCountries) {
  # DataSet<-BootMirror_RichDiv
  # countries<-"All"

  # Subsampled_Countries<-BootMirror_Countries
  Subsampled_Countries <- left_join(Subsampled_Countries, WaiverCountries) %>% 
    replace_na(list(WaiverGroup = "no waiver"))
  # levels(as.factor(Subsampled_Countries$WaiverGroup))



  # countries<-"All"
  # countries<-"no_CHN_USA"
  library(tidyverse)
  library(RColorBrewer)
  library(ggExtra)
  library(egg)


  DataSet <- DataSet %>%
    # mutate(OA_group = ifelse(OA_group == "OA", "PW", author)) %>%
    # mutate(author = ifelse(Dataset == "sole_NOCHNUSA", "solo", author)) %>%
    # mutate(author = ifelse(Dataset == "sole_ALL", "solo", author)) %>%
    # mutate(author = ifelse(Dataset == "first_NOCHNUSA", "author_first", author)) %>%
    # mutate(author = ifelse(Dataset == "first_ALL", "author_first", author)) %>%
    mutate(Dataset = ifelse(Dataset == "All", "All Countries", Dataset)) %>%
    # mutate(Dataset = ifelse(Dataset == "first_ALL", "All Countries", Dataset)) %>%
    # mutate(Dataset = ifelse(Dataset == "sole_NOCHNUSA","CHN & USA excluded", Dataset)) %>%
    mutate(Dataset = ifelse(Dataset == "no_CHN_USA", 
                            "CHN & USA excluded",
                            Dataset))
  # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset)) %>%
  # mutate(Dataset = ifelse(Dataset == "CHN & USA excluded", "Without China & USA", Dataset))




  Subsampled_Countries <- Subsampled_Countries %>%
    # mutate(author = ifelse(Dataset == "sole_NOCHNUSA", "solo", author)) %>%
    # mutate(author = ifelse(Dataset == "sole_ALL", "solo", author)) %>%
    # mutate(author = ifelse(Dataset == "first_NOCHNUSA", "author_first", author)) %>%
    # mutate(author = ifelse(Dataset == "first_ALL", "author_first", author)) %>%
    # mutate(Dataset = ifelse(Dataset == "sole_ALL", "All Countries", Dataset)) %>%
    mutate(Dataset = ifelse(Dataset == "All", 
                            "All Countries",
                            Dataset)) %>%
    # mutate(Dataset = ifelse(Dataset == "sole_NOCHNUSA","CHN & USA excluded", Dataset)) %>%
    mutate(Dataset = ifelse(Dataset == "no_CHN_USA", 
                            "CHN & USA excluded", 
                            Dataset))

  # vars<-list(DataSet,Subsampled_Countries,countries,sole_ALL,sole_NOCHNUSA,first_ALL,first_NOCHNUSA)

  # DataSet<-as.data.frame(vars[1])
  DataSet <- as.data.frame(DataSet)
  # Subsampled_Countries<-as.data.frame(vars[2])
  Subsampled_Countries <- as.data.frame(Subsampled_Countries)
  # countries<-vars[3]
  # countries<-countries


  Subsampled_Countries$WaiverGroup <- 
    as.factor(Subsampled_Countries$WaiverGroup)
  Subsampled_Countries$WaiverGroup <- 
    as.factor(Subsampled_Countries$WaiverGroup)
  Subsampled_Countries$Code <- 
    as.factor(Subsampled_Countries$Code)
  Subsampled_Countries$Country <- 
    as.factor(Subsampled_Countries$Country)
  Subsampled_Countries$IncomeGroup <- 
    ordered(Subsampled_Countries$IncomeGroup,
    levels = c("Low income", 
               "Lower middle income",
               "Upper middle income",
               "High income")
  )

  # Subsampled_Countries$WaiverGroup <- ordered(Subsampled_Countries$WaiverGroup,
  #                                        levels =c("South Asia",
  #                                                  "North America",
  #                                                  "Sub-Saharan Africa",
  #                                                  "Latin America & Caribbean",
  #                                                  "Middle East & North Africa",
  #                                                  "East Asia & Pacific",
  #                                                  "Europe & Central Asia"))
  Subsampled_Countries$WaiverGroup <- ordered(Subsampled_Countries$WaiverGroup,
    levels = c(
      "GroupA",
      "GroupB",
      "no waiver"
    )
  )


  Subsampled_Countries$ArticleCat <- "PW"
  Subsampled_Waiver_summary <- Subsampled_Countries %>%
    group_by(author, Dataset, ArticleCat, replicate, WaiverGroup) %>%
    summarize(n = n()) %>%
    mutate(perc = n / sum(n) * 100)


  PW_medians <- Subsampled_Waiver_summary %>%
    group_by(author, Dataset, replicate, WaiverGroup) %>%
    summarise(median = median(perc))
  PW_medians$JrnlType <- "PW"




  # OAinPW_medians<-Subsampled_Income_summary %>%
  #   filter(ArticleCat=="OAinPW") %>%
  #   group_by(author,Dataset,replicate,WaiverGroup) %>%
  #   summarise(median=median(perc))
  # OAinPW_medians$ArticleCat<-"OAinPW"
  # ###################################
  ###################################
  # OA
  ###################################
  ###################################
  #
  # one_author_pubs_ALL<-read_csv(file="./data_clean/sole_author_pubs_ALL_first_author.csv")
  # coauthor_pubs_ALL<-read_csv(file="./data_clean/coauthor_pubs_ALL_first_author.csv")
  # one_author_pubsNOCHNUSA<-read_csv(file="./data_clean/one_author_pubsNOCHNUSA.csv")
  # coauthor_pubsNOCHNUSA<-read_csv(file="./data_clean/coauthor_pubsNOCHNUSA.csv")
  #
  #
  #
  # coauthor_pubs_ALL_first<-coauthor_pubs_ALL
  # coauthor_pubs_ALL_first$author<-"author_first"
  #
  # coauthor_pubsNOCHNUSA_first<-coauthor_pubsNOCHNUSA
  # coauthor_pubsNOCHNUSA_first$author<-"author_first"
  #
  # coauthor_pubs_ALL_first<-coauthor_pubs_ALL %>%
  #   group_by(DOI) %>%
  #   filter(AuthorNum == 1)
  # coauthor_pubs_ALL_first$author<-"author_first"
  #
  # coauthor_pubsNOCHNUSA_last<-coauthor_pubsNOCHNUSA %>%
  #   group_by(DOI) %>%
  #   filter(AuthorNum == max(AuthorNum)) %>%
  #   filter(AuthorNum>1)
  # coauthor_pubsNOCHNUSA_last$author<-"author_last"
  #
  # coauthor_pubsNOCHNUSA_first<-coauthor_pubsNOCHNUSA %>%
  #   group_by(DOI) %>%
  #   filter(AuthorNum == 1)
  # coauthor_pubsNOCHNUSA_first$author<-"author_first"

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

  AllPubs <- left_join(AllPubs, WaiverCountries, by = "Code") %>% 
    replace_na(list(WaiverGroup = "no waiver"))

  OAData <- AllPubs %>%
    filter(ArticleType == "OA") %>%
    drop_na(WaiverGroup)

  OA_percs <- OAData %>%
    group_by(author, Dataset, ArticleType, WaiverGroup) %>%
    summarize(n = n()) %>%
    mutate(perc = n / sum(n) * 100)


  OA_percs$WaiverGroup <- ordered(OA_percs$WaiverGroup,
    levels = levels(Subsampled_Countries$WaiverGroup)
  )

  Subsampled_Waiver_summary_plot <- Subsampled_Waiver_summary %>%
    filter(author != "author_all")




  # author.labels <- c(author_first = "First Authors", author_last = "Last Authors", solo= "Single Authors")
  author.labels <- c(author_first = "First Authors", solo = "Single Authors")
  color.labels <- c("GroupA" = "#A6CEE3", 
                    "GroupB" = "#1F78B4", 
                    "no waiver" = "#B2DF8A")
  Subsampled_Waiver_summary_plot$author <-
    as.factor(Subsampled_Waiver_summary_plot$author)
  Subsampled_Waiver_summary_plot$author <-
    ordered(Subsampled_Waiver_summary_plot$author, 
            levels = c("solo", "author_first", "author_last", "author_all"))

  # DATA SELECTION FOR Figure
  # if (((vars[3]=="All")==TRUE)) {
  if (((countries == "All") == TRUE)) {
    fig_data <- Subsampled_Waiver_summary_plot %>%
      filter(Dataset == "All Countries")
    label_data <- OA_percs %>% filter(Dataset == "All Countries")
    # } else if (((vars[3]=="no_CHN_USA")==TRUE)) {
  } else if (((countries == "no_CHN_USA") == TRUE)) {
    fig_data <- Subsampled_Waiver_summary_plot %>%
      filter(Dataset == "CHN & USA excluded")
    label_data <- OA_percs %>% filter(Dataset == "CHN & USA excluded")
  } else {
    stop("Please enter 'All' or 'no_CHN_USA' ")
  }



  fig_data <- fig_data %>% drop_na(WaiverGroup)

  WaiverGroup <- OA_percs$WaiverGroup

  WaiverGroup <- recode_factor(WaiverGroup,
    "GroupA" = "Group A (100%)",
    "GroupB" = "Group B (50%)",
    "no waiver" = "No waiver",
    .default = levels(WaiverGroup)
  )

  OA_percs$WaiverGroup <- WaiverGroup


  WaiverGroup <- fig_data$WaiverGroup
  WaiverGroup <- recode_factor(WaiverGroup,
    "GroupA" = "Group A (100%)",
    "GroupB" = "Group B (50%)",
    "no waiver" = "No waiver",
    .default = levels(WaiverGroup)
  )

  fig_data$WaiverGroup <- WaiverGroup

  WaiverGroup <- label_data$WaiverGroup
  WaiverGroup <- recode_factor(WaiverGroup,
    "GroupA" = "Group A (100%)",
    "GroupB" = "Group B (50%)",
    "no waiver" = "No waiver",
    .default = levels(WaiverGroup)
  )

  #
  label_data$WaiverGroup <- WaiverGroup
  label_data <- label_data %>% dplyr::rename("ArticleCat" = "ArticleType")

  fig_data$author <- factor(fig_data$author,
                            levels = c("solo", "author_first"))

  label_data$author <- factor(label_data$author, 
                              levels = c("solo", "author_first"))
  # as.factor(fig_data$ArticleCat)
  # as.factor(fig_data$author)
  # filter(fig_data,author=="author_first")
  WaiverGroupPlot <- ggplot(fig_data, 
                            aes(x = n, 
                                fill = WaiverGroup, 
                                Color = ArticleCat)) +
    geom_histogram(
      position = "identity",
      bins = 100,
      color = "black",
      size = 0.5
    ) +
    # scale_color_brewer(palette = "PRGn")+
    # scale_fill_brewer(palette = "PRGn")+
    scale_fill_manual(values = c("#084594", "#9ECAE1", "#F7FBFF")) +
    # name="Waiver Catgeory",
    # breaks=c("Group A\n(100%)", "Group B\n(50%)","No\nWaiver"))+
    ylab("Frequency") +
    xlab("Percentage of authors in each Elsevier Waiver Group") +
    facet_grid(
      cols = vars(author),
      rows = vars(WaiverGroup),
      scales = ("free_x"),
      labeller = labeller(author = author.labels)
    ) +
    geom_hline((aes(yintercept = -Inf)), color = "black") +
    geom_vline((aes(xintercept = -Inf)), color = "black") +
    scale_y_continuous(limits = c(0, 400),
                       breaks = seq(0, 400, by = 100), 
                       expand = c(0, 0.1)) +
    # scale_x_continuous(limits = c(0, 60),breaks = seq(0,60, by=10),expand=c(0,0.1))+
    coord_cartesian(clip = "off") +
    # xlim(-5,80)+
    # ylim(-5,1300)+
    # scale_x_continuous(breaks = seq(0,100, by=10),expand = c(0, 0))+
    # scale_y_continuous(breaks = seq(0,700, by=150),expand = c(0.0, 0))+
    # brewer.pal(4, "paired") gets the hex codes from palette
    # coord_flip()+
    ## OA POINTS LINES AND LABELS
    # geom_segment(data = label_data,
    #              aes(x = perc,
    #                  y = as.numeric(WaiverGroup)+.7,
    #                  xend = perc,
    #                  yend = 600),
    #              linetype="solid", color="red")+
    # geom_text(data = label_data,
    #           aes(x=perc,
    #               y=670,
    #               label = "OA"),color="red", size=8)
    #
    # geom_text(data = subset(filter(label_data, ArticleCat=="OA")),
    #           aes(x=perc,
    #               y=670,
    #               label = "OA"),color="red", size=4)+
    geom_segment(
      data = subset(filter(label_data, ArticleCat == "OA")),
      aes(
        x = perc,
        y = as.numeric(WaiverGroup),
        xend = perc,
        yend = 350
      ),
      color = "red", size = 1.5, linetype = "dashed"
    )


  # +
  #
  # geom_segment(data = subset(filter(label_data, ArticleCat=="OAinPW")),
  #              aes(x = perc,
  #                  y = as.numeric(WaiverGroup),
  #                  xend = perc,
  #                  yend = 600),
  #              linetype="solid", color="blue")+
  #
  # geom_text(data = subset(filter(label_data, ArticleCat=="OAinPW")),
  #           aes(x=perc,
  #               y=700,
  #               label = "OA in PW"),color="blue", size=7)



  WaiverGroupPlot

  WaiverGroupPlot <- WaiverGroupPlot +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(colour = "black", 
                                  size = 25, vjust = -0.5),
      axis.title.y = element_text(colour = "black", 
                                  size = 25, vjust = 2),
      strip.text.x = element_text(size = 20, 
                                  margin = margin(5, 0, 3, 0, "lines")),
      strip.text.y = element_text(size = 30, angle = 0),
      strip.background.x = element_rect(fill = NA, colour = NA),
      strip.background.y = element_rect(fill = NA, colour = NA),
      panel.spacing.x = unit(1, "lines"),
      panel.spacing.y = unit(2, "lines"),
      legend.position = "none",
      legend.text = element_text(size = 20),
      plot.margin = unit(c(3, 1, 1, 1.5), "lines") # plot margin - top, right, bottom, left
    )
  facet_labels <- c("A", "B", "C", "D", "E", "F")
  WaiverGroupPlot <- tag_facet(WaiverGroupPlot, open = "",
                               close = "", tag_pool = facet_labels, 
                               vjust = 0.5, hjust = -1, size = 10)
  WaiverGroupPlot


  #######################################################
  #######################################################
  #######################################################
  # WaiverGroup P_hat
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
    filter(WaiverGroup == "Group A (100%)") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    filter(WaiverGroup == "Group A (100%)") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)

  perc_belowOA <- perc$perc_belowOA
  perc_belowOA

  P_Hat$P_Hat[P_Hat$WaiverGroup == "Group A (100%)" &
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
    filter(WaiverGroup == "Group B (50%)") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    filter(WaiverGroup == "Group B (50%)") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$WaiverGroup == "Group B (50%)" &
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
    filter(WaiverGroup == "No waiver") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "author_first") %>%
    filter(ArticleCat == "PW") %>%
    filter(WaiverGroup == "No waiver") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$WaiverGroup == "No waiver" &
    P_Hat$author == "author_first" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "All Countries"] <- perc_belowOA

  #
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
    filter(WaiverGroup == "Group A (100%)") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    filter(WaiverGroup == "Group A (100%)") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$WaiverGroup == "Group A (100%)" &
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
    filter(WaiverGroup == "Group B (50%)") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "PW") %>%
    filter(WaiverGroup == "Group B (50%)") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$WaiverGroup == "Group B (50%)" &
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
    filter(WaiverGroup == "No waiver") %>%
    select(perc)

  perc <- fig_data %>%
    filter(Dataset == "All Countries") %>%
    filter(author == "solo") %>%
    filter(ArticleCat == "OA") %>%
    filter(WaiverGroup == "No waiver") %>%
    ungroup() %>%
    tally(perc < crit$perc) %>%
    mutate(perc_belowOA = n / n_boot)
  perc_belowOA <- perc$perc_belowOA
  perc_belowOA



  P_Hat$P_Hat[P_Hat$WaiverGroup == "Sub-Saharan\nAfrica" &
    P_Hat$author == "solo" &
    P_Hat$ArticleCat == "OA" &
    P_Hat$Dataset == "No waiver"] <- perc_belowOA

  ###############
  # WaiverGroup<-P_Hat$WaiverGroup
  # WaiverGroup<-recode_factor(WaiverGroup,"Group A (100%)"="GroupA",
  #                       "Group B (50%)"="GroupB",
  #                       "No waiver"="no waiver",
  #                       .default = levels(WaiverGroup))
  # P_Hat$WaiverGroup<-WaiverGroup

  P_Hat <- P_Hat %>%
    dplyr::rename(
      "OA_perc" = "perc",
      "Author" = "author",
      "Countries" = "Dataset"
    ) %>%
    select(-n)


  P_Hat$Author <- gsub("author_first", "First", P_Hat$Author)
  P_Hat$Author <- gsub("solo", "Single", P_Hat$Author)




  return(list(WaiverGroupPlot, P_Hat))
}