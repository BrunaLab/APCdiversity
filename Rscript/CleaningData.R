# purpose of script:
# read in data and begin to 

# libraries
# insrall id you need to
#install.packages("bibliometrix", dependencies=TRUE) ### installs bibliometrix package and dependencies
#install.packages("digest")
#install.packages("tidyverse")

library(digest)
library(bibliometrix)   ### load bibliometrix package
library(dplyr)
library(tidyverse)

# data
articles_X <- readFiles('data/DrBruna.bib') # articles data from x jourals
articles_nonX <-readFiles('') # articles from pay/non-x journals

# process the articles
articles_X_df <- convert2df(articles_X, dbsource = "scopus", format = "bibtex")
articles_nonX_df <- convert2df(articles_nonX, dbsource = "scopus", format = "bibtex")

# save as a csv file
write.csv(articles_X_df,"figures/scopusX.csv")

# their default bibliometric analyses
results_X <- biblioAnalysis(articles_X_df, sep = ";")
options(width=100)
# a summary of their analyses
summary_X <- summary(object = results_X, k = 10, pause = FALSE)
# The have some nice plots
plot(x = results_X, k = 10, pause = FALSE)

# extract the author countries)
# These data will be the last column of the resulting df
AuthorGeoX <- metaTagExtraction(articles_X_df, Field = "AU_CO", sep = ";")

AuthorGeoX$AuthorCountry # This is it.
# You need to put each value in its own cell, then convert from wide to long.
# Find me to see what I mean. tidyverse has some nice tools to do this.




