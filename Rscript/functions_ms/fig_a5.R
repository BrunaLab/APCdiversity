fig_a5 <- function(DataSet, AuPosition) {
  library(tidyverse)
  library(egg)
  names(DataSet)
  # DataSet<-AllData
  # AuPosition<-"author_first"
  vars <- list(DataSet, AuPosition)
  DataSet <- as.data.frame(DataSet)
  DataSet$IncomeGroup <- as.factor(DataSet$IncomeGroup)
  DataSet$IncomeGroup <- ordered(DataSet$IncomeGroup,
                                 levels = c("High", "Upper middle", 
                                            "Lower middle", "Low"))
  # levels(DataSet$IncomeGroup)
  # DataSet<-AllData
  # AuPosition<-"author_first"
  if ((AuPosition == "author_first") == TRUE) {
    first_author_income_cats <- as.data.frame(DataSet) %>%
      filter(AuthorNum == 1) %>%
      group_by(JrnlType, ArticleType, IncomeGroup) %>%
      drop_na("IncomeGroup") %>%
      tally() %>%
      mutate(percentage = n / sum(n) * 100)

    xlabeltext <- "World Bank Lending Group (all lead authors)"
  } else if ((AuPosition == "author_last") == TRUE) {
    first_author_income_cats <- as.data.frame(DataSet) %>%
      group_by(refID) %>%
      filter(AuthorNum == max(AuthorNum)) %>%
      group_by(ArticleType, IncomeGroup) %>%
      drop_na("IncomeGroup") %>%
      tally() %>%
      mutate(percentage = n / sum(n) * 100)

    xlabeltext <- "World Bank Lending Group (all lead authors)"
  } else {
    stop("Please enter 'author_first' or 'author_last'")
  }
  #
  #
  #
  first_author_income_cats$JrnlType <- 
    gsub("OA", "Mirror", first_author_income_cats$JrnlType)
  first_author_income_cats$JrnlType <- 
    gsub("PW", "Parent", first_author_income_cats$JrnlType)
  # Figure
  first_author_income_cats$category <-
    paste(first_author_income_cats$JrnlType, 
          first_author_income_cats$ArticleType, 
          sep = "-")
  # labels <- c(OA = "Open Access Articles", PW = "Paywalled Articles")
  plot1 <- ggplot(first_author_income_cats, 
                  aes(x = IncomeGroup, y = percentage)) +
    geom_bar(stat = "identity") +
    xlab(xlabeltext) +
    ylab("Articles (%)") +
    scale_y_continuous(limits = c(0, 100), 
                       breaks = seq(0, 100, by = 20), expand = c(0, 0.1)) +
    # scale_x_discrete(limits = bar_order)+
    facet_grid(
      rows = vars(category),
      # labeller=labeller(JrnlType = labels)
    ) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf)
  plot1 <- plot1 +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(colour = "black", size = 22, vjust = -0.5),
      axis.title.y = element_text(colour = "black", size = 22, hjust = 0.5, ),
      strip.text.x = element_text(size = 0, margin =
                                    margin(0, 0, 3, 0, "lines")),
      strip.text.y = element_text(size = 0, angle = 0),
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
  plot1

  facet_labels <- c(
    "(A) OA Articles - Mirror",
    "(B) OA Articles - Parent",
    "(C) non-OA Articles - Parent"
  )
  plot1 <- tag_facet(plot1, open = "", close = "", 
                     tag_pool = facet_labels, size = 8)
  plot1


  return(plot1)
}