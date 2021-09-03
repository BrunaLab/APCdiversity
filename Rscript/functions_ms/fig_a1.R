fig_a1 <- function(AllData) {
  library(tidyverse)

  AllGeo <- AllData %>%
    group_by(refID) %>%
    filter(AuthorNum == 1)

  # This takes the codes and converts them to names
  library(countrycode)
  AllGeo$Code <-
    countrycode(AllGeo$Code,
      origin = "iso3c",
      destination = "country.name"
    )


  AllGeo$IncomeGroup <- as.factor(AllGeo$IncomeGroup)

  levels(AllGeo$IncomeGroup)[levels(AllGeo$IncomeGroup) ==
    "Lower middle"] <- "Lower-middle"
  levels(AllGeo$IncomeGroup)[levels(AllGeo$IncomeGroup) ==
    "Upper middle"] <- "Upper-middle"

  AllGeo$IncomeGroup <- ordered(AllGeo$IncomeGroup,
    levels = c("Low", "Lower-middle", "Upper-middle", "High")
  )



  AllGeo <- AllGeo %>%
    filter(First_Author_Country != "NA" & Code != "NA") %>%
    ungroup() %>%
    select(First_Author_Country, Code, Region, IncomeGroup) %>%
    add_count(Code) %>%
    group_by(Code) %>%
    slice(1) %>%
    arrange(desc(n)) %>%
    filter(IncomeGroup != "NA") %>%
    ungroup() %>%
    mutate(perc = n / sum(n) * 100)
  sum(AllGeo$perc)

  AllGeo$perc <- round(AllGeo$perc, 2)
  AllGeo <- arrange(AllGeo, desc(AllGeo$perc))
  AllGeo <- transform(AllGeo, Code = reorder(Code, perc))
  AllGeo$IncomeGroup <- as.factor(AllGeo$IncomeGroup)
  AllGeo$IncomeGroup <- droplevels(AllGeo$IncomeGroup)
  levels(AllGeo$IncomeGroup)

  cutoff <- 25 # This is how many countries you want on the chart, all the rest will be in "OTHER"
  AllGeo2 <- arrange(AllGeo, desc(perc)) %>% select(Code, n, perc, IncomeGroup)
  most.common.authors <- slice(AllGeo, 1:cutoff)
  lst.common.authors <- slice(AllGeo, (cutoff + 1):nrow(AllGeo))
  # lst.common.authors$Code<-"all others"
  # lst.common.authors$IncomeGroup<-"all others"
  lst.common.authors <- lst.common.authors %>%
    group_by(IncomeGroup) %>%
    summarize(n = sum(n), perc = sum(perc), n_countries = n_distinct(Code))

  lst.common.authors$Code <- paste(lst.common.authors$n_countries,
    lst.common.authors$IncomeGroup,
    sep = " ",
    collapse = NULL
  )

  lst.common.authors$IncomeGroup <- ordered(lst.common.authors$IncomeGroup,
    levels = c(
      "Low",
      "Lower-middle",
      "Upper-middle",
      "High"
    )
  )
  lst.common.authors <- arrange(lst.common.authors, desc(IncomeGroup))

  most.common.authors$IncomeGroup <- ordered(most.common.authors$IncomeGroup,
    levels = c(
      "Low",
      "Lower-middle",
      "Upper-middle",
      "High"
    )
  )

  most.common.authors$Code <- as.factor(most.common.authors$Code)


  most.common.authors <- bind_rows(most.common.authors, lst.common.authors)

  # This is needed to put them in order in the plot with OTHER at the end of the graph
  order <- rev(seq(1:nrow(most.common.authors))) # REV is what makes it go tyop to bottom if flipped coordinates
  most.common.authors$Code <- factor(
    most.common.authors$Code,
    most.common.authors$Code[levels = order]
  )
  # rm(order,AllGeo,lst.common.authors)
  most.common.authors


  label_x <- "First author country"


  AppFig1 <- ggplot(
    most.common.authors,
    aes(x = Code, y = perc, fill = IncomeGroup)
  ) +
    geom_bar(stat = "identity", colour = "black", size = 0.1) +
    geom_text(aes(label = n), hjust = -.2, size = 2) +
    scale_y_continuous(
      limits = c(0, 40),
      breaks = seq(0, 40, by = 5), expand = c(0, 0.1)
    ) +
    # geom_text(size = 3, position = position_stack(vjust = 0.5))+
    xlab("Country") +
    ylab("Percentage of Articles") +
    # scale_fill_brewer(palette = "Greys")+
    # "#F7FBFF" "#DEEBF7" "#C6DBEF" "#9ECAE1" "#6BAED6" "#4292C6" "#2171B5" "#084594"
    # scale_fill_manual(values=c("#2171B5","#6BAED6","#BDD7E7","#EFF3FF"),
    # scale_fill_manual(values=c("#F7FBFF","#C6DBEF","#6BAED6","#084594"),
    scale_fill_manual(
      values = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#084594"),
      # scale_fill_manual(values=c("#222C61","#7E82AC","#AFB8D9","#C7C5D5"),
      # scale_fill_manual(values=c("#19245C","#626FA3","#98A2D9","#26378F"),
      name = "National Income Category",
      breaks = c("High", "Upper-middle", "Lower-middle", "Low"), drop = FALSE
    ) +
    # #
    coord_flip()
  AppFig1 <- AppFig1 +
    theme_classic() +
    # labs(title = “Main title”, subtitle = “My subtitle”, caption = title_text)+
    # labs(title = title_text,size=10)+
    theme(
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(colour = "black", size = 12, vjust = -0.5),
      axis.title.y = element_text(
        colour = "black",
        size = 12, hjust = 0.5,
        vjust = 0.5, angle = 90
      ),
      plot.title = element_text(size = 8),
      legend.text = element_text(colour = "black", size = 8, vjust = 0.5),
      legend.title = element_text(size = 8),
      legend.position = "right",
      # legend.key.height = unit(1,"lines"),
      plot.margin = unit(c(1, 1, 1, 1.5), "lines")
    )
  AppFig1



  return(AppFig1)
}
