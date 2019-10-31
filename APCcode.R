library(tidyverse)
library(bibliometrix)
# read in the file
articles_X <- readFiles('./data/DrBruna.bib')
# process the articles
articles_X_df <- convert2df(articles_X, dbsource = "scopus", format = "bibtex")
str(articles_X_df)

# save as a csv file
write.csv(articles_X_df,"./output/scopusX.csv")

#What sources are in the scopus file?
# read in the list of X journals published by Elsevier
all_X_journals<-read.csv("./data/elsevier_x_journals.csv")
str(all_X_journals)
all_X_journals$journal<-as.character(all_X_journals$journal) # convert from factor to character
all_X_journals$journal<-tolower(all_X_journals$journal) #convert to lower case
all_X_journals$journal<-as.factor(all_X_journals$journal) # convert to factor
summary(all_X_journals)
# which journals are in the scopus search results?
articles_X_journals<-(articles_X_df$SO)
articles_X_journals<-noquote(articles_X_journals)
articles_X_journals<-tolower(articles_X_journals) # convert to lower case
articles_X_journals<-as.factor(articles_X_journals) # convert to factor

articles_X_journals<-levels(articles_X_journals)
summary(articles_X_journals)
articles_X_journals<-as.factor(articles_X_journals) # convert to factor
articles_X_journals<-as.data.frame(articles_X_journals)
names(articles_X_journals)[1]<-"journal"
write.csv(articles_X_journals,"./output/scopusX_journals_returned.csv")


# Which ones from the X list are in the SCOPUS Seearch?
commonX<- semi_join(articles_X_journals,all_X_journals,by="journal")  # X journals in the scopus search
commonX$no_apc<-commonX$journal
NotReturnedScopus<-anti_join(all_X_journals,articles_X_journals,by="journal")  
write.csv(commonX,"./output/jrnls_to_search.csv")



# their default bibliometric analyses
results_X <- biblioAnalysis(articles_X_df, sep = ";")
options(width=100)
# a summary of their analyses
summary_X <- summary(object = results_X, k = 10, pause = FALSE)
# The have some nice plots
plot(x = results, k = 10, pause = FALSE)

# extract the author countries)
# These data will be the last column of the resulting df
AuGeoX <- metaTagExtraction(articles_X_df, Field = "AU_CO", sep = ";")
AuGeoX$AU_CO # This is it.
# You need to put each value in its own cell, then convert from wide to long.
# FInd me to see what I mean. tidyverse has some nice tools to do this.





############################################
# Articles from non-APC journals from WOS
############################################

library(refsplitr)

######################
# Load the data
######################

WOS<-references_read(data = './data/wos/', dir = TRUE, include_all=FALSE)

# save the data as a csv; that way you don't have to read it in again.
write.csv(WOS,"./output/WOS_references.csv")
# load the csv as an object
WOS<-read.csv("./output/WOS_references.csv")


######################
# Process the data & disambiaguate the author names
######################
WOS_cln<-authors_clean(WOS)
head(WOS$cln, 20)


######################
# save the preliminary disambiguation as a csv file
write.csv(WOS_cln$prelim,"./output/WOS_prelim.csv")

# load to show
WOS_cln_prelim<-read.csv("./output/WOS_cln_prelim.csv")

# save the names suggested for review as a csv file
write.csv(WOS_cln$review,"./output/WOS_cln_review.csv")

######################
# Accept the disambiguation or load / merge your corrections 
WOS_refined <- authors_refine(WOS_cln$review,WOS_cln$prelim)

# save the disambiguated data set
write.csv(WOS_refined,"./output/WOS_refined.csv")
######################


WOS_refined$country

######################


######################
# Georeference the author locations
WOS_georef <-authors_georef(data=WOS_refined, 
                                address_column = "address")


######################
# Visualizations

# Plot No. pf authors x country

plot_addresses_country <- plot_addresses_country(WOS_georef$addresses)

# Plot author location
plot_addresses_points <- plot_addresses_points(WOS_georef$addresses)
plot_addresses_points

# # Plot social network x country
# plot_net_coauthor <- plot_net_coauthor(example_georef$addresses)

# Plot coauthorships x country
plot_net_country <- plot_net_country(WOS_georef$addresses)
plot_net_country$plot


# Plot coauthorships x locality
plot_net_address <- plot_net_address(WOS_georef$addresses)
plot_net_address$plot
######################














library(bibliometrix)
D <- readFiles('./data/DrBruna.bib')
scopus2df(D)
convert2df(D)

M <- convert2df(D, dbsource = "scopus", format = "plaintext")


D <- readFiles('./data/pubmed1.txt')
D <- readFiles('./data/pmc_result.txt')
M <- convert2df(D, dbsource = "pubmed", format = "plaintext")


library(revtools)
data <- read_bibliography('./data/pubmed1.txt')

library(RISmed)
search_query <- EUtilsSummary('Analytica Chimica Acta:X', type='esearch', db='pubmed')
summary(search_query)
QueryId(search_query)
records<- EUtilsGet(search_query)
class(records)
pubmed_data <- data.frame('Address'=C1(records),'Abstract'=AbstractText(records))
head(pubmed_data,1)


search_topic <- 'copd'
search_query <- EUtilsSummary(search_topic, retmax=100, mindate=2012, maxdate=2012)
summary(search_query)
QueryId(search_query)
records<- EUtilsGet(search_query)
class(records)
pubmed_data <- data.frame('Title'=ArticleTitle(records),'Abstract'=AbstractText(records))
head(pubmed_data,1)




install.packages("easyPubMed")
library(easyPubMed)
new_query <- 'Bladder[TIAB] AND Northwestern[AD] AND Chicago[AD] AND "2018"[PDAT]' 
out.A <- batch_pubmed_download(pubmed_query_string = new_query, 
                               format = "xml", 
                               batch_size = 20,
                               dest_file_prefix = "easyPM_example",
                               encoding = "ASCII")
print(out.A) 


my_PM_list <- articles_to_list(pubmed_data = my_abstracts_xml)
class(my_PM_list[1])










###########################################################################
# Michael L. Bernauer
# mlbernauer@gmail.com
# 12/14/2014
# Module for parsing PubMed Medline files.
# Files should be downloaded to your
# computer and loaded into R by passing the
# file path into the medline function.
# The function returns a list containing
# each Medline entry.
#
# USAGE:
# source('medline.R')
# medline_records <- medline("/home/user/Downloads/pubmed_results.txt")
#
# USAGE:
# query <- "\"unversity of new mexico\"[AD] AND \"pharmacy\"[AD]"
# entrez_results < entrez_fetcher(query, "pubmed", "medline")
#
# Results can be parsed with the medline function
# medline_results <- medline_parser(entrez_results)
############################################################################

library(RCurl)
library(XML)

medline_parser = function(file_name){
  if(file.exists(file_name)){
    lines <- readLines(file_name)
  }
  else {
    lines <- strsplit(file_name, "\n")[[1]]
  }
  medline_records <- list()
  key <- 0
  record <- 0
  for(line in lines){
    header <- sub(" {1,20}", "", substring(line, 1, 4))
    value <- sub("^.{6}", "", line)
    if(header == "" & value == ""){
      next
    }
    else if(header == "PMID"){
      record = record + 1
      medline_records[[record]] <- list()
      medline_records[[record]][header] <- value
    }
    else if(header == "" & value != ""){
      medline_records[[record]][key] <- paste(medline_records[[record]][key], value)
    }
    else{
      key <- header
      if(is.null(medline_records[[record]][key][[1]])){
        medline_records[[record]][key] <- value
      }
      else { 
        medline_records[[record]][key] <- paste(medline_records[[record]][key], value, sep=";")
      }
    }
  }
  return(medline_records)
}

# Function for retrieving PubMed search results to be parsed with
# medlineParser
entrez_fetcher = function(query_string, db="pubmed", type="medline"){
  base <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  query <- gsub(" ", "+", query_string)
  query <- paste("esearch.fcgi?db=", db, "&term=", query, "&usehistory=y", sep="")
  url <- paste(base, query, sep="")
  url_result <- getURL(url)
  
  xml_data <- xmlToList(xmlParse(url_result))
  web <- xml_data["WebEnv"][[1]]
  key <- xml_data["QueryKey"][[1]]
  
  # Assemble Efetch URL
  fetch <- paste(base, "efetch.fcgi?db=", db, "&query_key=", key,
                 "&WebEnv=", web, "&rettype=", type, "&retmode=text", sep="")
  data <- getURL(fetch)
  return(data)
}
