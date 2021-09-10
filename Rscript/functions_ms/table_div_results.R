table_div_results <- function(sole_ALL, sole_NOCHNUSA, first_ALL, first_NOCHNUSA) {
  BootOAinPW_RichDiv <- read_csv("./data_clean/BootOAinPW_RichDiv.csv")
  # BootOAinPW_Countries<-read_csv("./output/BootOAinPW_Countries.csv")
  BootMirror_RichDiv <- read_csv("./data_clean/BootMirror_RichDiv.csv")
  # BootMirror_Countries<-read_csv("./output/BootMirror_Countries.csv")

  library(tidyverse)

  # OA - Mirror -------------------------------------------------------------

  OAdiv_first_ALL_OApool <- DivRichCalc(first_ALL, "author_first", "OA", "OA")
  OAdiv_first_ALL_OApool <- as.numeric(OAdiv_first_ALL_OApool[2])
  OArich_first_ALL_OApool <- DivRichCalc(first_ALL, "author_first", "OA", "OA")
  OArich_first_ALL_OApool <- as.numeric(OArich_first_ALL_OApool[1])
  OAeven_first_ALL_OApool <- DivRichCalc(first_ALL, "author_first", "OA", "OA")
  OAeven_first_ALL_OApool <- as.numeric(OAeven_first_ALL_OApool[5])

  OAdiv_first_NOCHNUSA_OApool <- DivRichCalc(first_NOCHNUSA, 
                                             "author_first", "OA", "OA")
  OAdiv_first_NOCHNUSA_OApool <- as.numeric(OAdiv_first_NOCHNUSA_OApool[2])
  OArich_first_NOCHNUSA_OApool <- DivRichCalc(first_NOCHNUSA,
                                              "author_first", "OA", "OA")
  OArich_first_NOCHNUSA_OApool <- as.numeric(OArich_first_NOCHNUSA_OApool[1])
  OAeven_first_NOCHNUSA_OApool <- DivRichCalc(first_NOCHNUSA,
                                              "author_first", "OA", "OA")
  OAeven_first_NOCHNUSA_OApool <- as.numeric(OAeven_first_NOCHNUSA_OApool[5])


  OAdiv_sole_ALL_OApool <- DivRichCalc(sole_ALL, "author_first", "OA", "OA")
  OAdiv_sole_ALL_OApool <- as.numeric(OAdiv_sole_ALL_OApool[2])
  OArich_sole_ALL_OApool <- DivRichCalc(sole_ALL, "author_first", "OA", "OA")
  OArich_sole_ALL_OApool <- as.numeric(OArich_sole_ALL_OApool[1])
  OAeven_sole_ALL_OApool <- DivRichCalc(sole_ALL, "author_first", "OA", "OA")
  OAeven_sole_ALL_OApool <- as.numeric(OAeven_sole_ALL_OApool[5])


  OAdiv_sole_NOCHNUSA_OApool <- DivRichCalc(sole_NOCHNUSA,
                                            "author_first", "OA", "OA")
  OAdiv_sole_NOCHNUSA_OApool <- as.numeric(OAdiv_sole_NOCHNUSA_OApool[2])
  OArich_sole_NOCHNUSA_OApool <- DivRichCalc(sole_NOCHNUSA,
                                             "author_first", "OA", "OA")
  OArich_sole_NOCHNUSA_OApool <- as.numeric(OArich_sole_NOCHNUSA_OApool[1])
  OAeven_sole_NOCHNUSA_OApool <- DivRichCalc(sole_NOCHNUSA,
                                             "author_first", "OA", "OA")
  OAeven_sole_NOCHNUSA_OApool <- as.numeric(OAeven_sole_NOCHNUSA_OApool[5])



  author <- c(rep("First", 2), rep("Single", 2))
  Dataset <- rep(c("All Countries", "Without China & USA"), 2)
  # OA_articleType<-c(rep("allOA",4),rep("mirror",4),rep("OAinPW",4))
  OA_articleType <- c(rep("mirror_OA", 4))

  OA_Diversity <- data.frame(
    "OA" = c(
      OAdiv_first_ALL_OApool,
      OAdiv_first_NOCHNUSA_OApool,
      OAdiv_sole_ALL_OApool,
      OAdiv_sole_NOCHNUSA_OApool
    ),
    "Author" = author,
    "Dataset" = Dataset
  )

  OA_Richness <- data.frame(
    "OA" = c(
      OArich_first_ALL_OApool,
      OArich_first_NOCHNUSA_OApool,
      OArich_sole_ALL_OApool,
      OArich_sole_NOCHNUSA_OApool
    ),
    "Author" = author,
    "Dataset" = Dataset
  )



  OA_Evenness <- data.frame(
    "OA" = c(
      OAeven_first_ALL_OApool,
      OAeven_first_NOCHNUSA_OApool,
      OAeven_sole_ALL_OApool,
      OAeven_sole_NOCHNUSA_OApool
    ),
    "Author" = author,
    "Dataset" = Dataset
  )

  OA_Richness$OA <- round(OA_Richness$OA, 0)
  OA_Evenness$OA <- round(OA_Evenness$OA, 2)
  OA_Diversity$OA <- round(OA_Diversity$OA, 1)

  Observed <- bind_rows(OA_Richness, OA_Diversity, OA_Evenness)
  Observed$Metric <- c(rep("Richness", 4),
                       rep("Diversity", 4), rep("Evenness", 4))

  Observed <- Observed %>%
    select(Metric, Author, Dataset, OA) %>%
    arrange(desc(Metric), Dataset, desc(Author))
  Observed <- pivot_wider(Observed,
                          names_from = Dataset, values_from = c(OA))

  Observed_mirror_OA <- Observed
  Observed_mirror_OA$OA_jrnl <- "mirror"


  # OA - Parent -------------------------------------------------------------


  OAdiv_first_ALL_OApool <- DivRichCalc(first_ALL, "author_first", "PW", "OA")
  OAdiv_first_ALL_OApool <- as.numeric(OAdiv_first_ALL_OApool[2])
  OArich_first_ALL_OApool <- DivRichCalc(first_ALL, "author_first", "PW", "OA")
  OArich_first_ALL_OApool <- as.numeric(OArich_first_ALL_OApool[1])
  OAeven_first_ALL_OApool <- DivRichCalc(first_ALL, "author_first", "PW", "OA")
  OAeven_first_ALL_OApool <- as.numeric(OAeven_first_ALL_OApool[5])

  OAdiv_first_NOCHNUSA_OApool <- DivRichCalc(first_NOCHNUSA,
                                             "author_first", "PW", "OA")
  OAdiv_first_NOCHNUSA_OApool <- as.numeric(OAdiv_first_NOCHNUSA_OApool[2])
  OArich_first_NOCHNUSA_OApool <- DivRichCalc(first_NOCHNUSA, 
                                              "author_first", "PW", "OA")
  OArich_first_NOCHNUSA_OApool <- as.numeric(OArich_first_NOCHNUSA_OApool[1])
  OAeven_first_NOCHNUSA_OApool <- DivRichCalc(first_NOCHNUSA,
                                              "author_first", "PW", "OA")
  OAeven_first_NOCHNUSA_OApool <- as.numeric(OAeven_first_NOCHNUSA_OApool[5])


  OAdiv_sole_ALL_OApool <- DivRichCalc(sole_ALL, "author_first", "PW", "OA")
  OAdiv_sole_ALL_OApool <- as.numeric(OAdiv_sole_ALL_OApool[2])
  OArich_sole_ALL_OApool <- DivRichCalc(sole_ALL, "author_first", "PW", "OA")
  OArich_sole_ALL_OApool <- as.numeric(OArich_sole_ALL_OApool[1])
  OAeven_sole_ALL_OApool <- DivRichCalc(sole_ALL, "author_first", "PW", "OA")
  OAeven_sole_ALL_OApool <- as.numeric(OAeven_sole_ALL_OApool[5])


  OAdiv_sole_NOCHNUSA_OApool <- DivRichCalc(sole_NOCHNUSA, 
                                            "author_first", "PW", "OA")
  OAdiv_sole_NOCHNUSA_OApool <- as.numeric(OAdiv_sole_NOCHNUSA_OApool[2])
  OArich_sole_NOCHNUSA_OApool <- DivRichCalc(sole_NOCHNUSA, 
                                             "author_first", "PW", "OA")
  OArich_sole_NOCHNUSA_OApool <- as.numeric(OArich_sole_NOCHNUSA_OApool[1])
  OAeven_sole_NOCHNUSA_OApool <- DivRichCalc(sole_NOCHNUSA, 
                                             "author_first", "PW", "OA")
  OAeven_sole_NOCHNUSA_OApool <- as.numeric(OAeven_sole_NOCHNUSA_OApool[5])



  author <- c(rep("First", 2), rep("Single", 2))
  Dataset <- rep(c("All Countries", "Without China & USA"), 2)
  # OA_articleType<-c(rep("allOA",4),rep("mirror",4),rep("OAinPW",4))
  OA_articleType <- c(rep("parent_OA", 4))

  OA_Diversity <- data.frame(
    "OA" = c(
      OAdiv_first_ALL_OApool,
      OAdiv_first_NOCHNUSA_OApool,
      OAdiv_sole_ALL_OApool,
      OAdiv_sole_NOCHNUSA_OApool
    ),
    "Author" = author,
    "Dataset" = Dataset
  )

  OA_Richness <- data.frame(
    "OA" = c(
      OArich_first_ALL_OApool,
      OArich_first_NOCHNUSA_OApool,
      OArich_sole_ALL_OApool,
      OArich_sole_NOCHNUSA_OApool
    ),
    "Author" = author,
    "Dataset" = Dataset
  )



  OA_Evenness <- data.frame(
    "OA" = c(
      OAeven_first_ALL_OApool,
      OAeven_first_NOCHNUSA_OApool,
      OAeven_sole_ALL_OApool,
      OAeven_sole_NOCHNUSA_OApool
    ),
    "Author" = author,
    "Dataset" = Dataset
  )

  OA_Richness$OA <- round(OA_Richness$OA, 0)
  OA_Evenness$OA <- round(OA_Evenness$OA, 2)
  OA_Diversity$OA <- round(OA_Diversity$OA, 1)

  Observed <- bind_rows(OA_Richness, OA_Diversity, OA_Evenness)
  Observed$Metric <- c(rep("Richness", 4), 
                       rep("Diversity", 4), rep("Evenness", 4))

  Observed <- Observed %>%
    select(Metric, Author, Dataset, OA) %>%
    arrange(desc(Metric), Dataset, desc(Author))
  Observed <- pivot_wider(Observed, names_from = Dataset, values_from = c(OA))

  Observed_parent_OA <- Observed
  Observed_parent_OA$OA_jrnl <- "parent"

  # bootstrap values mirror -------------------------------------------------

  Div_means_Mirror <- BootMirror_RichDiv %>%
    select(InvSimp, author, Dataset) %>%
    group_by(author, Dataset) %>%
    summarise(
      mean = mean(InvSimp),
      SD = sd(InvSimp)
    )
  Div_means_Mirror$metric <- "Diversity"


  Rich_means_Mirror <- BootMirror_RichDiv %>%
    select(Richness, author, Dataset) %>%
    group_by(author, Dataset) %>%
    summarise(
      mean = mean(Richness),
      SD = sd(Richness)
    )
  Rich_means_Mirror$metric <- "Richness"

  Even_means_Mirror <- BootMirror_RichDiv %>%
    select(Even, author, Dataset) %>%
    group_by(author, Dataset) %>%
    summarise(
      mean = mean(Even),
      SD = sd(Even)
    )
  Even_means_Mirror$metric <- "Evenness"



  means_mirror <- bind_rows(Rich_means_Mirror, 
                            Div_means_Mirror,
                            Even_means_Mirror)
  means_mirror$author <- gsub("author_first", "First", means_mirror$author)
  means_mirror$author <- gsub("solo", "Single", means_mirror$author)
  means_mirror$Dataset <- gsub("CHN & USA excluded", 
                               "Without China & USA", means_mirror$Dataset)
  means_mirror <- means_mirror %>%
    select(metric, author, Dataset, mean, SD) %>%
    dplyr::rename("Author" = "author", "Metric" = "metric")
  means_mirror <- pivot_wider(means_mirror, 
                              names_from = Dataset, 
                              values_from = c(mean, SD))

  means_mirror$OA_jrnl <- "mirror"


  # bootstrap values parent -------------------------------------------------

  Div_means_OAinPW <- BootOAinPW_RichDiv %>%
    select(InvSimp, author, Dataset) %>%
    group_by(author, Dataset) %>%
    summarise(
      mean = mean(InvSimp),
      SD = sd(InvSimp)
    )
  Div_means_OAinPW$metric <- "Diversity"


  Rich_means_OAinPW <- BootOAinPW_RichDiv %>%
    select(Richness, author, Dataset) %>%
    group_by(author, Dataset) %>%
    summarise(
      mean = mean(Richness),
      SD = sd(Richness)
    )
  Rich_means_OAinPW$metric <- "Richness"

  Even_means_OAinPW <- BootOAinPW_RichDiv %>%
    select(Even, author, Dataset) %>%
    group_by(author, Dataset) %>%
    summarise(
      mean = mean(Even),
      SD = sd(Even)
    )
  Even_means_OAinPW$metric <- "Evenness"



  means_OAinPW <- bind_rows(Rich_means_OAinPW, 
                            Div_means_OAinPW, 
                            Even_means_OAinPW)
  means_OAinPW$author <- gsub("author_first", 
                              "First", 
                              means_OAinPW$author)
  means_OAinPW$author <- gsub("solo", 
                              "Single", 
                              means_OAinPW$author)
  means_OAinPW$Dataset <- gsub("CHN & USA excluded", 
                               "Without China & USA", 
                               means_OAinPW$Dataset)
  means_OAinPW <- means_OAinPW %>%
    select(metric, author, Dataset, mean, SD) %>%
    dplyr::rename("Author" = "author", "Metric" = "metric")
  means_OAinPW <- pivot_wider(means_OAinPW,
                              names_from = Dataset,
                              values_from = c(mean, SD))

  means_OAinPW$OA_jrnl <- "parent"

  #######################################
  # bind the two sets
  #######################################

  means <- bind_rows(means_OAinPW, means_mirror)
  observed <- bind_rows(Observed_mirror_OA, Observed_parent_OA)
  #######################################
  # put together in a a Table
  #######################################

  names(means)
  means <- means %>% select(
    Metric,
    Author,
    `mean_All Countries`,
    `SD_All Countries`,
    `mean_Without China & USA`,
    `SD_Without China & USA`,
    OA_jrnl
  )





  Table2 <- left_join(observed, means)
  names(Table2)
  Table2 <- Table2 %>% select(
    Metric,
    Author,
    `All Countries`,
    `mean_All Countries`,
    `SD_All Countries`,
    `Without China & USA`,
    `mean_Without China & USA`,
    `SD_Without China & USA`,
    `OA_jrnl`
  )


  Table2 <- Table2 %>% arrange(
    OA_jrnl,
    Metric,
    Author
  )





  #######################################
  # p-hats mirror
  #######################################



  BootVals_Mirror <- BootMirror_RichDiv %>% 
    select(Richness, Even, InvSimp, author, Dataset)
  boot_runs_Mirror <- BootVals_Mirror %>%
    group_by(author, Dataset) %>%
    summarize(n = n())
  n_max <- max(boot_runs_Mirror$n)
  BootVals_Mirror$run <- (rep(seq(1:n_max), nrow(boot_runs_Mirror)))
  BootVals_Mirror <- pivot_wider(BootVals_Mirror,
    names_from = c(Dataset, author),
    values_from = c(Richness, Even, InvSimp)
  ) %>%
    select(-run)
  colnames(BootVals_Mirror)
  names(BootVals_Mirror) <- c(
    "Rich_all_solo",
    "Rich_all_first",
    "Rich_NO_solo",
    "Rich_NO_first",
    "Even_all_solo",
    "Even_all_first",
    "Even_NO_solo",
    "Even_NO_first",
    "Div_all_solo",
    "Div_all_first",
    "Div_NO_solo",
    "Div_NO_first"
  )

  BootVals_Mirror$OA_jrnl <- "mirror"

  #######################################
  # p-hats parent
  #######################################


  BootVals_OAinPW <- BootOAinPW_RichDiv %>% 
    select(Richness, Even, InvSimp, author, Dataset)
  boot_runs_OAinPW <- BootVals_OAinPW %>%
    group_by(author, Dataset) %>%
    summarize(n = n())
  n_max <- max(boot_runs_OAinPW$n)
  BootVals_OAinPW$run <- (rep(seq(1:n_max), nrow(boot_runs_OAinPW)))
  BootVals_OAinPW <- pivot_wider(BootVals_OAinPW,
    names_from = c(Dataset, author),
    values_from = c(Richness, Even, InvSimp)
  ) %>%
    select(-run)
  colnames(BootVals_OAinPW)
  names(BootVals_OAinPW) <- c(
    "Rich_all_solo",
    "Rich_all_first",
    "Rich_NO_solo",
    "Rich_NO_first",
    "Even_all_solo",
    "Even_all_first",
    "Even_NO_solo",
    "Even_NO_first",
    "Div_all_solo",
    "Div_all_first",
    "Div_NO_solo",
    "Div_NO_first"
  )

  BootVals_OAinPW$OA_jrnl <- "parent"

  #######################################
  # put together in a a Table
  #######################################

  # P_HAT OAinPW

  # single, all
  P_Hat <- data.frame(Table2[1:2])
  P_Hat$P_Hat_all <- NA
  P_Hat$P_Hat_NO <- NA
  boot_reps <- nrow(BootVals_OAinPW)
  ##########
  # All countries, coauthored, High
  colnames(BootVals_OAinPW)

  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Diversity") %>%
    filter(Author == "First") %>%
    filter(OA_jrnl == "parent") %>%
    tally((BootVals_OAinPW$Div_all_first < `All Countries`) / boot_reps)
  P_Hat[1, 3] <- as.numeric(crit)

  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Diversity") %>%
    filter(Author == "Single") %>%
    filter(OA_jrnl == "parent") %>%
    tally((BootVals_OAinPW$Div_all_solo < `All Countries`) / boot_reps)
  P_Hat[2, 3] <- as.numeric(crit)

  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Eveness") %>%
    filter(Author == "First") %>%
    filter(OA_jrnl == "parent") %>%
    tally((BootVals_OAinPW$Even_all_first < `All Countries`) / boot_reps)
  P_Hat[3, 3] <- as.numeric(crit)


  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Eveness") %>%
    filter(Author == "Single") %>%
    filter(OA_jrnl == "parent") %>%
    tally((BootVals_OAinPW$Even_all_solo < `All Countries`) / boot_reps)
  P_Hat[4, 3] <- as.numeric(crit)

  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Richness") %>%
    filter(Author == "First") %>%
    filter(OA_jrnl == "parent") %>%
    tally((BootVals_OAinPW$Rich_all_first < `All Countries`) / boot_reps)
  P_Hat[5, 3] <- as.numeric(crit)

  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Richness") %>%
    filter(Author == "Single") %>%
    filter(OA_jrnl == "parent") %>%
    tally(BootVals_OAinPW$Rich_all_solo < `All Countries`) / boot_reps
  P_Hat[6, 3] <- as.numeric(crit)



  # NO CHN USA

  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Diversity") %>%
    filter(Author == "First") %>%
    filter(OA_jrnl == "parent") %>%
    tally((BootVals_OAinPW$Div_NO_first < `All Countries`) / boot_reps)
  P_Hat[1, 4] <- as.numeric(crit)

  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Diversity") %>%
    filter(Author == "Single") %>%
    filter(OA_jrnl == "parent") %>%
    tally((BootVals_OAinPW$Div_NO_solo < `All Countries`) / boot_reps)
  P_Hat[2, 4] <- as.numeric(crit)

  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Evenness") %>%
    filter(Author == "First") %>%
    filter(OA_jrnl == "parent") %>%
    tally((BootVals_OAinPW$Even_NO_first < `All Countries`) / boot_reps)
  P_Hat[3, 4] <- as.numeric(crit)

  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Evenness") %>%
    filter(Author == "Single") %>%
    filter(OA_jrnl == "parent") %>%
    tally((BootVals_OAinPW$Even_NO_solo < `All Countries`) / boot_reps)
  P_Hat[4, 4] <- as.numeric(crit)



  #
  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Richness") %>%
    filter(Author == "First") %>%
    filter(OA_jrnl == "parent") %>%
    tally((BootVals_OAinPW$Rich_NO_first < `All Countries`) / boot_reps)
  P_Hat[5, 4] <- as.numeric(crit)

  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Richness") %>%
    filter(Author == "Single") %>%
    filter(OA_jrnl == "parent") %>%
    tally((BootVals_OAinPW$Rich_NO_solo < `All Countries`) / boot_reps)
  P_Hat[6, 4] <- as.numeric(crit)
  #

  #


  P_Hat$P_Hat_all <- round(P_Hat$P_Hat_all, 3)
  P_Hat$P_Hat_NO <- round(P_Hat$P_Hat_NO, 3)
  P_Hat_parent <- P_Hat
  P_Hat_parent <- na.omit(P_Hat_parent)
  P_Hat_parent$OA_jrnl <- "parent"


  P_Hat$P_Hat_all <- round(P_Hat$P_Hat_all, 3)
  P_Hat$P_Hat_NO <- round(P_Hat$P_Hat_NO, 3)


  #######################################
  # P HAT Mirror

  # single, all
  P_Hat <- data.frame(Table2[1:2])
  P_Hat$P_Hat_all <- NA
  P_Hat$P_Hat_NO <- NA
  boot_reps <- nrow(BootVals_Mirror)
  ##########
  # All countries, coauthored, High
  colnames(BootVals_Mirror)


  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Diversity") %>%
    filter(Author == "First") %>%
    filter(OA_jrnl == "mirror") %>%
    tally((BootVals_Mirror$Div_all_first < `All Countries`) / boot_reps)
  P_Hat[1, 3] <- as.numeric(crit)


  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Diversity") %>%
    filter(Author == "Single") %>%
    filter(OA_jrnl == "mirror") %>%
    tally((BootVals_Mirror$Div_all_solo < `All Countries`) / boot_reps)
  P_Hat[2, 3] <- as.numeric(crit)


  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Eveness") %>%
    filter(Author == "First") %>%
    filter(OA_jrnl == "mirror") %>%
    tally((BootVals_Mirror$Even_all_first < `All Countries`) / boot_reps)
  P_Hat[3, 3] <- as.numeric(crit)


  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Eveness") %>%
    filter(Author == "Single") %>%
    filter(OA_jrnl == "mirror") %>%
    tally((BootVals_Mirror$Even_all_solo < `All Countries`) / boot_reps)
  P_Hat[4, 3] <- as.numeric(crit)



  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Richness") %>%
    filter(Author == "First") %>%
    filter(OA_jrnl == "mirror") %>%
    tally((BootVals_Mirror$Rich_all_first < `All Countries`) / boot_reps)
  P_Hat[5, 3] <- as.numeric(crit)

  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Richness") %>%
    filter(Author == "Single") %>%
    filter(OA_jrnl == "mirror") %>%
    tally((BootVals_Mirror$Rich_all_solo < `All Countries`) / boot_reps)
  P_Hat[6, 3] <- as.numeric(crit)


  # NO CHN USA

  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Diversity") %>%
    filter(Author == "First") %>%
    filter(OA_jrnl == "mirror") %>%
    tally((BootVals_Mirror$Div_NO_first < `All Countries`) / boot_reps)
  P_Hat[1, 4] <- as.numeric(crit)

  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Diversity") %>%
    filter(Author == "Single") %>%
    filter(OA_jrnl == "mirror") %>%
    tally((BootVals_Mirror$Div_NO_solo < `All Countries`) / boot_reps)
  P_Hat[2, 4] <- as.numeric(crit)

  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Evenness") %>%
    filter(Author == "First") %>%
    filter(OA_jrnl == "mirror") %>%
    tally((BootVals_Mirror$Even_NO_first < `All Countries`) / boot_reps)
  P_Hat[3, 4] <- as.numeric(crit)
  P_Hat$P_Hat_all <- round(P_Hat$P_Hat_all, 3)
  P_Hat$P_Hat_NO <- round(P_Hat$P_Hat_NO, 3)

  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Evenness") %>%
    filter(Author == "Single") %>%
    filter(OA_jrnl == "mirror") %>%
    tally((BootVals_Mirror$Even_NO_solo < `All Countries`) / boot_reps)
  P_Hat[4, 4] <- as.numeric(crit)


  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Richness") %>%
    filter(Author == "First") %>%
    filter(OA_jrnl == "mirror") %>%
    tally((BootVals_Mirror$Rich_NO_first < `All Countries`) / boot_reps)
  P_Hat[5, 4] <- as.numeric(crit)


  crit <- Table2 %>%
    select(Metric, `All Countries`, Author, OA_jrnl) %>%
    filter(Metric == "Richness") %>%
    filter(Author == "Single") %>%
    filter(OA_jrnl == "mirror") %>%
    tally((BootVals_Mirror$Rich_NO_solo < `All Countries`) / boot_reps)
  P_Hat[6, 4] <- as.numeric(crit)


  P_Hat$P_Hat_all <- round(P_Hat$P_Hat_all, 3)
  P_Hat$P_Hat_NO <- round(P_Hat$P_Hat_NO, 3)

  P_Hat_mirror <- P_Hat
  P_Hat_mirror <- na.omit(P_Hat_mirror)
  P_Hat_mirror$OA_jrnl <- "mirror"
  ########################
  # Bind into a table
  ########################

  P_Hat <- bind_rows(P_Hat_mirror, P_Hat_parent)

  ###########################################
  # BIND INTO TABLE
  names(Table2)
  Table2 <- left_join(Table2, P_Hat) %>%
    select(
      Metric, Author, `All Countries`, 
      `mean_All Countries`, `SD_All Countries`, `P_Hat_all`,
      `Without China & USA`, `mean_Without China & USA`,
      `SD_Without China & USA`, P_Hat_NO, OA_jrnl
    )
  Table2$`mean_All Countries` <-
    round(Table2$`mean_All Countries`, 2)
  Table2$`SD_All Countries` <- 
    round(Table2$`SD_All Countries`, 2)
  Table2$`mean_Without China & USA` <- 
    round(Table2$`mean_Without China & USA`, 2)
  Table2$`SD_Without China & USA` <- 
    round(Table2$`SD_Without China & USA`, 2)
  ######################
  Table2 <- Table2 %>%
    mutate(PW_AllCountries = paste(`mean_All Countries`, 
                                   `SD_All Countries`, 
                                   sep = "+/-")) %>%
    select(-`mean_All Countries`, -`SD_All Countries`) %>%
    mutate(PW_noUSAorCHN = paste(`mean_Without China & USA`,
                                 `SD_Without China & USA`,
                                 sep = "+/-")) %>%
    select(-`mean_Without China & USA`, -`SD_Without China & USA`) %>%
    select(
      Metric, Author,
      `All Countries`, PW_AllCountries, P_Hat_all,
      `Without China & USA`, PW_noUSAorCHN, P_Hat_NO, OA_jrnl
    )


  Table2$PW_AllCountries <- as.character(Table2$PW_AllCountries)
  Table2$PW_noUSAorCHN <- as.character(Table2$PW_noUSAorCHN)
  Table2$PW_AllCountries <- str_replace(Table2$PW_AllCountries, "[+]", " \u00B1 ")
  Table2$PW_AllCountries <- str_replace(Table2$PW_AllCountries, "[/]", "")
  Table2$PW_AllCountries <- str_replace(Table2$PW_AllCountries, "[-]", "")
  Table2$PW_noUSAorCHN <- str_replace(Table2$PW_noUSAorCHN, "[+]", " \u00B1 ")
  Table2$PW_noUSAorCHN <- str_replace(Table2$PW_noUSAorCHN, "[/]", "")
  Table2$PW_noUSAorCHN <- str_replace(Table2$PW_noUSAorCHN, "[-]", "")
  names(Table2) <- c(
    "Metric", "Author", "OA (All Countries)", "Mean PW (All Countries)",
    "phat1", "OA (USA & CHN excluded)",
    "Mean PW (USA & CHN excluded)", "phat2", "OA Source"
  )
  # Table2 <- Table2 %>% select("Metric","Author","OA (All Countries)",
  #                                         "Mean PW (All Countries)","PW 95% CI (All Countries)",
  #                                         "OA (USA & CHN excluded)","Mean PW (USA & CHN excluded)",
  #                                         "PW 95% CI (USA & CHN excluded)")
  names(Table2)
  Table2 <- Table2 %>% arrange(`OA Source`, Metric, Author)

  return(Table2)
}
