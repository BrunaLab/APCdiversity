fig_countries <- function(DataSet, AuPosition, JrnlType, ArticleType, cutoff) {
  library(tidyverse)
  # DataSet<-single_ALL
  # AuPosition<-"author_first"
  # JrnlType<-"OA"
  # ArticleType<-"OA"
  # cutoff<-15
  
  vars <- list(DataSet, AuPosition, JrnlType, ArticleType, cutoff)

  if ((vars[2] == "author_first") == TRUE &
    (vars[3] == "OA") == TRUE &
    (vars[4] == "OA") == TRUE) {
    AllGeo <- DataSet %>%
      group_by(refID) %>%
      filter(AuthorNum == 1) %>%
      filter(JrnlType == "OA") %>%
      filter(ArticleType == "OA")
  } else if ((vars[2] == "author_first") == TRUE &
    (vars[3] == "PW") == TRUE &
    (vars[4] == "OA") == TRUE) {
    AllGeo <- DataSet %>%
      group_by(refID) %>%
      filter(AuthorNum == 1) %>%
      filter(JrnlType == "PW") %>%
      filter(ArticleType == "OA")
  } else if ((vars[2] == "author_first") == TRUE &
    (vars[3] == "both") == TRUE &
    (vars[4] == "OA") == TRUE) {
    AllGeo <- DataSet %>%
      group_by(refID) %>%
      filter(AuthorNum == 1) %>%
      filter(ArticleType == "OA")
  } else if ((vars[2] == "author_first") == TRUE &
    (vars[3] == "PW") == TRUE &
    (vars[4] == "PW") == TRUE) {
    AllGeo <- DataSet %>%
      group_by(refID) %>%
      filter(AuthorNum == 1) %>%
      filter(JrnlType == "PW") %>%
      filter(ArticleType == "PW")
  } else {
    stop("oops...you probably entered 'author_last' for b")
  }

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
    select(First_Author_Country, Code, Region, IncomeGroup, JrnlType) %>%
    add_count(Code) %>%
    group_by(Code) %>%
    slice(1) %>%
    arrange(desc(n)) %>%
    filter(IncomeGroup != "NA") %>%
    ungroup() %>%
    mutate(perc = n / sum(n) * 100)
  sum(AllGeo$perc)
  # view(AllGeo)
  AllGeo$perc <- round(AllGeo$perc, 2)
  AllGeo <- arrange(AllGeo, desc(AllGeo$perc))
  AllGeo <- transform(AllGeo, Code = reorder(Code, perc))
  AllGeo$IncomeGroup <- as.factor(AllGeo$IncomeGroup)
  AllGeo$IncomeGroup <- droplevels(AllGeo$IncomeGroup)
  levels(AllGeo$IncomeGroup)

  cutoff <- as.numeric(vars[5]) # This is how many countries you want 
                                # on the chart, all the rest will be in "OTHER"
                                # cutoff = 20 # This is how many countries you 
                                # want on the chart, rest will be in "OTHER"
  AllGeo2 <- arrange(AllGeo, desc(perc)) %>% 
    select(Code, n, perc, IncomeGroup, JrnlType)
  most.common.authors <- slice(AllGeo, 1:cutoff)
  lst.common.authors <- slice(AllGeo, (cutoff + 1):nrow(AllGeo))
  # lst.common.authors$Code<-"all others"
  # lst.common.authors$IncomeGroup<-"all others"
  lst.common.authors <- lst.common.authors %>%
    group_by(IncomeGroup) %>%
    summarize(n = sum(n), perc = sum(perc), n_countries = n_distinct(Code))

  lst.common.authors$Code <- paste(lst.common.authors$n_countries,
    lst.common.authors$IncomeGroup,
    sep = " ", collapse = NULL
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

  # This is needed to put them in order in 
  # the plot with OTHER at the end of the graph
  # REV is what makes it go top to bottom if flipped coordinates
  order <- rev(seq(1:nrow(most.common.authors))) 
  most.common.authors$Code <- 
    factor(most.common.authors$Code, most.common.authors$Code[levels = order])
  # rm(order,AllGeo,lst.common.authors)
  most.common.authors


  # Figure
  if (((vars[3] == "both") == TRUE) &
    ((vars[4] == "OA") == TRUE) &
    ((vars[2] == "author_first") == TRUE)) {
    # # title_text=paste("Fig. 3a: Country in which the first",
    # "author of ---- articles is based.",sep=" ")
    label_x <- "First author country"
    facet_label <- "(A) Open Access articles"
    legend_on <- c(.9, .30)
    legend_on <- "none"
  } else if (((vars[3] == "OA") == TRUE) &
    ((vars[4] == "OA") == TRUE) &
    ((vars[2] == "author_first")) == TRUE) {
    # title_text=paste("Fig. 3b: Country in which the first",
    #                  "author of ----- articles is based.",sep=" ")
    label_x <- "First author country"
    facet_label <- "A. Mirror Journals"
    legend_on <- "none"
  } else if (((vars[3] == "PW") == TRUE) &
    ((vars[4] == "OA") == TRUE) &
    ((vars[2] == "author_first")) == TRUE) {
    # title_text=paste("Fig. 3b: Country in which the first",
    #                  "author of ----- articles is based.",sep=" ")
    label_x <- "First author country"
    facet_label <- "B. Parent Journals (Open Access)"
    legend_on <- "right"
  } else if (((vars[3] == "PW") == TRUE) &
    ((vars[4] == "PW") == TRUE) &
    ((vars[2] == "author_first")) == TRUE) {
    # title_text=paste("Fig. 3b: Country in which the first",
    #                  "author of ----- articles is based.",sep=" ")
    label_x <- "First author country"
    facet_label <- "C. Parent Journals (Subscription)"
    legend_on <- "none"

    # } else if (((vars[3]=="PW")==TRUE) &
    #            ((vars[4]=="OA")==TRUE) &
    #            ((vars[2]=="author_last"))==TRUE){
    #   # title_text=paste("Fig. 3b: Country in which the first",
    #   #                  "author of paywalled articles is based.",sep=" ")
    #   label_x="Last author country"
    #   facet_label="(A) OA Mirror Journals"
    #   legend_on="none"
    #
    # } else if (((vars[3]=="PW")==TRUE) &
    #            ((vars[4]=="OA")==TRUE) &
    #            ((vars[2]=="author_first"))==TRUE){
    #   # title_text=paste("Fig. 3b: Country in which the first",
    #   #                  "author of ----- articles is based.",sep=" ")
    #   label_x="Last author country"
    #   facet_label="(B) Open Access"
    #   legend_on="none"
    #
    # } else if (((vars[3]=="PW")==TRUE) &
    #            ((vars[4]=="PW")==TRUE) &
    #            ((vars[2]=="author_first"))==TRUE){
    #   # title_text=paste("Fig. 3b: Country in which the first",
    #   #                  "author of paywalled articles is based.",sep=" ")
    #   label_x="Last author country"
    #   facet_label="(C) Subscription Journals"
    #   legend_on="none"
  } else {
    stop("ooops...did you enter 'author_last' for b?")
  }

  CountryPlot <- ggplot(most.common.authors, aes(x = Code, y = perc, fill = IncomeGroup)) +
    # CountryPlot<-ggplot(most.common.authors, aes(x=Code,y=perc))+
    geom_bar(
      stat = "identity",
      position = "dodge",
      colour = "black",
      size = 0.1
    ) +
    # scale_fill_manual(drop=FALSE) + #This keeps all the factors
    # in the legend eveniof it doesn't have any of one of the levels in the plot
    geom_text(aes(label = n), hjust = -.2, size = 2) +
    ggtitle(facet_label) +
    # annotate("text", x=14, y=35,label=facet_label) +
    # ylim(0,50)+
    scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 5), expand = c(0, 0.1)) +
    # geom_text(size = 3, position = position_stack(vjust = 0.5))+
    xlab("Country") +
    ylab("Percentage of Articles") +
    coord_flip() +
    # scale_fill_brewer(palette = "RdYlBu")+
    # "#F7FBFF" "#DEEBF7" "#C6DBEF" "#9ECAE1" "#6BAED6" "#4292C6" "#2171B5" "#084594"
    # scale_fill_manual(values=c("#2171B5","#6BAED6","#BDD7E7","#EFF3FF"),
    scale_fill_manual(
      values = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#084594"),
      # scale_fill_manual(values=c("#222C61","#7E82AC","#AFB8D9","#C7C5D5"),
      # scale_fill_manual(values=c("#19245C","#626FA3","#98A2D9","#26378F"),
      name = "Income Category",
      breaks = c("High", "Upper-middle", "Lower-middle", "Low"), drop = FALSE
    )
  #




  if (((vars[3] == "PW") == TRUE) &
    ((vars[4] == "PW") == TRUE)) {
    CountryPlot <- CountryPlot +
      theme_classic() +
      # labs(title = “Main title”, subtitle = “My subtitle”, caption = title_text)+
      # labs(title = title_text,size=10)+
      theme(
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(colour = "black", size = 8, vjust = -0.5),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.8),
        legend.text = element_text(colour = "black", size = 6, vjust = 0.5),
        legend.title = element_text(size = 6),
        legend.position = legend_on,
        # legend.key.height = unit(1,"lines"),
        plot.margin = unit(c(1, 1, 1, 1.5), "lines")
      )
  } else if ((vars[3] == "OA") == TRUE & (vars[4] == "OA") == TRUE) {
    CountryPlot <- CountryPlot +
      theme_classic() +
      # labs(title = “Main title”, subtitle = “My subtitle”, caption = title_text)+
      # labs(title = title_text,size=10)+
      theme(
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.8),
        legend.text = element_text(colour = "black", size = 6, vjust = 0.5),
        legend.title = element_text(size = 6),
        legend.position = legend_on,
        # legend.key.height = unit(1,"lines"),
        plot.margin = unit(c(1, 1, 1, 1.5), "lines")
      )
  } else if ((vars[3] == "PW") == TRUE & (vars[4] == "OA") == TRUE) {
    CountryPlot <- CountryPlot +
      theme_classic() +
      # labs(title = “Main title”, subtitle = “My subtitle”, caption = title_text)+
      # labs(title = title_text,size=10)+
      theme(
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.8),
        legend.text = element_text(colour = "black", size = 6, vjust = 0.5),
        legend.title = element_text(size = 8),
        legend.position = c(0.9, 0.5),
        # legend.justification = c("right", "top"),
        # legend.key.height = unit(1,"lines"),
        plot.margin = unit(c(1, 1, 1, 1.5), "lines")
      )
  } else {
    stop("ooops...fail.")
  }
  #
  # CountryPlot<-CountryPlot+
  #   theme_classic()+
  #   # labs(title = “Main title”, subtitle = “My subtitle”, caption = title_text)+
  #   # labs(title = title_text,size=10)+
  #   theme(
  #     axis.text.x = element_text(size=8),
  #     axis.text.y = element_text(size=8),
  #     # axis.title.x=element_text(colour="black", size = 12, vjust=-0.5),
  #     axis.title.x=element_blank(),
  #     plot.title = element_text(size=8),
  #     legend.text = element_text(colour="black", size = 6, vjust=0.5),
  #     legend.title = element_text(size=8),
  #     legend.position=legend_on,
  #     # legend.key.height = unit(1,"lines"),
  #     plot.margin =unit(c(1,1,1,1.5), "lines")
  #   )

  CountryPlot


  return(CountryPlot)
}