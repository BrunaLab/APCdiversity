n_OA_plots <- function(AllData, n_countries) {
  # n_countries<-20
  AllData$Journal <- as.factor(AllData$Journal)
  AllData$Code <- as.factor(AllData$Code)
  n_by_pair <- AllData %>%
    filter(ArticleType == "OA") %>%
    group_by(refID) %>%
    slice(1) %>%
    group_by(pair_key, Code, JrnlType) %>%
    summarize(n = n()) %>%
    arrange(Code) %>%
    spread(JrnlType, n) %>%
    replace_na(list(OA = 0, PW = 0)) %>%
    mutate(total_n = OA + PW)

  top_countries <- n_by_pair %>%
    group_by(Code) %>%
    summarize(n = sum(total_n)) %>%
    arrange(desc(n)) %>%
    slice(1:n_countries) %>%
    select(Code)

  top_n_by_pair <- n_by_pair %>%
    filter(Code %in% top_countries$Code)
  # top_n_by_pair$total_n<-as.factor(top_n_by_pair$total_n)
  top_n_by_pair$Code <- droplevels(top_n_by_pair$Code)
  jrnl.names <- AllData %>%
    filter(JrnlType == "OA") %>%
    select(Journal, pair_key) %>%
    distinct() %>%
    arrange(pair_key) %>%
    dplyr::rename("Mirror" = "Journal")
  top_n_by_pair <- left_join(top_n_by_pair, jrnl.names)
  # top_n_by_pair<- top_n_by_pair %>% mutate(OAperc=OA/total_n*100,PWperc=PW/total_n*100)
  top_n_by_pair$Mirror <- droplevels(top_n_by_pair$Mirror)

  plot_data <- top_n_by_pair %>%
    group_by(Code) %>%
    summarize(nPW = sum(PW), nOA = sum(OA)) %>%
    mutate(perc_pw = nPW / (nPW + nOA) * 100, perc_oa = nOA / (nPW + nOA) * 100)

  Codes <- AllData %>%
    group_by(refID) %>%
    slice(1) %>%
    ungroup() %>%
    select(First_Author_Country, Code, IncomeGroup, Region) %>%
    group_by(Code) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(Code)

  plot_data <- left_join(plot_data, Codes) %>% 
    arrange(perc_pw)

  plot_data$IncomeGroup <- as.factor(plot_data$IncomeGroup)
  levels(plot_data$IncomeGroup)[levels(plot_data$IncomeGroup) == 
                                  "Low"] <- "Low"
  levels(plot_data$IncomeGroup)[levels(plot_data$IncomeGroup) == 
                                  "Lower middle"] <- "Lower-middle"
  levels(plot_data$IncomeGroup)[levels(plot_data$IncomeGroup) == 
                                  "Upper middle"] <- "Upper-middle"
  levels(plot_data$IncomeGroup)[levels(plot_data$IncomeGroup) == 
                                  "High"] <- "High"
  plot_data$IncomeGroup <- ordered(plot_data$IncomeGroup,
    levels = c("Low", "Lower-middle", "Upper-middle", "High")
  )

  plot_data$color <- NA
  plot_data$color[plot_data$IncomeGroup == "Low"] <- "'#084594'"
  plot_data$color[plot_data$IncomeGroup == "Lower-middle"] <- "'#4292C6'"
  plot_data$color[plot_data$IncomeGroup == "Upper-middle"] <- "'#9ECAE1'"
  plot_data$color[plot_data$IncomeGroup == "High"] <- "'#F7FBFF'"


  
  
  # for stacked percent bar chart
  # plot_data2 <- plot_data %>% 
  #   select(Code, perc_pw, perc_oa, IncomeGroup) %>% 
  #   pivot_longer(perc_pw:perc_oa, names_to = "Income", values_to = "perc") %>% 
  #   pivot_wider(names_from = Code, values_from = perc)
  # # ggplot(plot_data2, aes(fill=IncomeGroup, y=IncomeGroup, x=Code)) + 
  # #   geom_bar(position="fill", stat="identity")
  # ggplot(plot_data2, aes(x = reorder(Code,-perc), y = perc, fill = Income)) +
  #   geom_col(position = "fill")
  
  
  
  
  color.labels <- c("Low" = "#084594", "Lower-middle" = "#4292C6",
                    "Upper-middle" = "#9ECAE1", "High" = "#F7FBFF")

  library(RColorBrewer)
  library(ggrepel)

  p1 <- ggplot(plot_data, aes(
    # x = perc_oa, # this is for percentage 
    # y = perc_pw, # this is for percentage
    x = nOA,
    y = nPW,
    color = IncomeGroup,
    fill = IncomeGroup,
    label = Code
  )) +
    geom_point(
      size = 6,
      color = "black",
      shape = 21,
      position = position_dodge(width = 0.2)
    ) +
    geom_abline(intercept = 0, slope = 1,lty="dashed")+
    # geom_hline(
    #   yintercept = 50,
    #   color = "darkgray",
    #   size = 1.5,
    #   linetype = "dashed"
    # ) +
    # geom_vline(
    #   xintercept = 50,
    #   color = "darkgray",
    #   size = 1.5,
    #   linetype = "dashed"
    # ) +
    scale_fill_manual(
      values = c("#F7FBFF", "#9ECAE1", "#4292C6", "#084594"),
      name = "National Income Category",
      breaks = c("High", "Upper-middle", "Lower-middle", "Low")
    ) +
    # this would be for percentage
    # scale_x_continuous(limits = c(0, 50), 
    #                    breaks = seq(0, 50, by = 10),
    #                    expand = c(0, 0.1)) +
    # scale_y_continuous(limits = c(50, 100),
    #                    breaks = seq(50, 100, by = 10),
    #                    expand = c(0, 0.1))
    scale_x_continuous(limits = c(0, 350), 
                     breaks = seq(0, 350, by = 25),
                     expand = c(0, 0.1)) +
    scale_y_continuous(limits = c(0, 350),
                       breaks = seq(0, 350, by = 25),
                       expand = c(0, 0.1))


  p1 <- p1 +
    theme_classic() +
    geom_text_repel(
      segment.color = "black",
      color = "black",
      size = 7,
      box.padding = 0.35,
      max.overlaps = 45
    ) +

    labs(
      y = "OA Articles in Parent journals",
      x = "Articles in Mirror journals"
    ) +
    theme(
      # legend.position = "right",
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(colour = "black", size = 24, vjust = -0.5),
      axis.title.y = element_text(
        colour = "black", size = 24, vjust = -0.5,
        margin = margin(t = 0, r = 20, b = 0, l = 0)
      ),
      plot.title = element_text(size = 8),
      legend.text = element_text(colour = "black", size = 16, vjust = 0.5),
      legend.title = element_text(size = 20),
      legend.position = c(0.85, 0.65),
      plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "lines")
    )
  p1
  return(p1)
}
