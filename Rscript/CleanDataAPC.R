# the libraries
library(tidyverse)
library(bibliometrix)

################################################################
# THIS IS FOR THE X JOURNALS, WHCIH WERE DOWNLOADED FROM SCOPUS
################################################################

# read in the data 
articles_X <- readFiles('./data/DrBruna.bib')
# process the data and convert to dataframe with bibliometrix
articles_X_df <- convert2df(articles_X, dbsource = "scopus", format = "bibtex")
str(articles_X_df)
# save as a csv file
write.csv(articles_X_df,"./output/scopusX.csv")

################################################################
# NARROW THE RESULTS TO X JOURNALS (turns out other journals snuck into the search)
################################################################
# To find out what X journals are in the scopus results,
# first, load the complete list of X journals published by Elsevier
all_X_journals<-read.csv("./data/elsevier_x_journals.csv")
all_X_journals$journal<-as.character(all_X_journals$journal) # convert journal name from factor to character
all_X_journals$journal<-tolower(all_X_journals$journal) # convert to lower case
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
write.csv(NotReturnedScopus,"./output/Xjrnls_not_in_Scopus_Search.csv")
write.csv(commonX,"./output/jrnls_to_search.csv")

########################
# Use this list to narrow down the SCOPUS 
# search results to include ONLY elsevier X journals. 
articles_X_df_reduced<-articles_X_df
str(articles_X_df_reduced)
articles_X_df_reduced$SO<-as.character(articles_X_df_reduced$SO) # convert from factor to character
articles_X_df_reduced$SO<-tolower(articles_X_df_reduced$SO) #convert to lower case
articles_X_df_reduced$SO<-as.factor(articles_X_df_reduced$SO) # convert to factor
all_X_journals<-all_X_journals %>% rename(SO=journal)
articles_X_df_reduced<-semi_join(articles_X_df_reduced,all_X_journals,by="SO")  # X journals in the scopus search

# remove all the intermediate dataframes from the environment
rm(commonX,articles_X_df,articles_X_journals,NotReturnedScopus)

# Now carry ourt bibliometrix default analyses on the "X journals only" df
results_X <- biblioAnalysis(articles_X_df_reduced, sep = ";")
options(width=100)
# a summary of their analyses
summary_X <- summary(object = results_X, k = 10, pause = FALSE)
# The have some nice plots
plot(x = results_X, k = 10, pause = FALSE)

# We need to extract the countries for each author
# These data will be the last column of the processed df
AuGeoX <- metaTagExtraction(articles_X_df_reduced, Field = "AU_CO", sep = ";")
str(AuGeoX)

# TO STREAMLINE, select only the doi of the article, the journal, the year published, and the information on author country
# note that all author countries are in a single column
X_articles_geodata<-AuGeoX %>% select(DI,SO,PY,AU_CO)
X_articles_geodata<-droplevels(X_articles_geodata)
str(X_articles_geodata)

# this splits up the author countrries - currently in a single column - into multiple columns (each country in its own column)
tempDF<- as.data.frame(str_split(X_articles_geodata$AU_CO, ";", simplify = TRUE))
tempDF <- tempDF %>% mutate_all(na_if,"")  #replace the blanks with NA

tempDF <- data.frame(lapply(tempDF, as.character), stringsAsFactors=FALSE) # Need to do this or gather won't work properly
X_articles_geodata<-cbind(X_articles_geodata,tempDF)
rm(tempDF)
str(X_articles_geodata)
X_articles_geodata<-X_articles_geodata %>% gather(author,country,5:30)
X_articles_geodata$author<-gsub("V","",X_articles_geodata$author) # remove the V, now have the author order
X_articles_geodata$author<-as.numeric(X_articles_geodata$author)
head(X_articles_geodata,10)
X_articles_geodata<-X_articles_geodata %>% arrange(DI,author)
X_articles_geodata<-X_articles_geodata[complete.cases(X_articles_geodata), ]
head(X_articles_geodata,10)
X_articles_geodata$DI<-as.factor(X_articles_geodata$DI)
X_articles_geodata$AU_CO<-NULL #delete the column with all countries in a single cell

# You can add the ISO three digit code for each country using library(countrycode)
library(countrycode) 
X_articles_geodata$country_code<-countrycode(X_articles_geodata$country,"country.name", "iso3c", warn = TRUE)
#By setting "warn=TRUE" it will tell you which ones it couldn't convert. Because of spelling mistakes, etc.
X_articles_geodata$country_code<-as.factor(X_articles_geodata$country_code)
summary(X_articles_geodata$country_code)
head(X_articles_geodata,10)
summary(X_articles_geodata)
X_articles_geodata$jrnl_type<-"OA"
write.csv(X_articles_geodata,"./output/X_Journal_author_countries.csv")



################################################################
# NOW DO THE SAME FOR ARTICLES FROM NON-X JOURNALS 
# These data came from Web of Science)
############################################

#install.packages("refsplitr")
library(refsplitr) #this package is not available for latest R version?

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
# When I first did this, it came back with some errors because some of the references had no authors

# Error in authors_clean(WOS) : The following references have no authors
# (i.e., there are NAs in the AU and AF fields):
#   
#   refID = 24013, 24014, 24015, 24016, 30896, 35339, 35340, 35405, 35406, 35407, 42490
# 
# Before using authors_clean() you MUST:
#   
#   (1) remove these references from the dataframe.
# 
# OR
# 
# (2) Correct the NAs in the AU and AF fields for these references.
# They do not have an author, in which case you can use "None", "Anonymous", "Unknown", etc.
# They may have been written by an Author Consortium (see Column "CA");
# If so you can replace the NAs in AU and AF with the contents of column CA. 

# So I cleaned the errors as per refsplitr vignette by replaceing author name with consortium name
WOS_cln<-authors_clean(WOS)
head(WOS$cln, 20)
# WOS[24013,]
# WOS[24014,]
# WOS[24015,]
# WOS[24016,]
# WOS[30896,]
# WOS[35339,]
# WOS[35340,]
# WOS[35405,]
# WOS[35406,]
# WOS[35407,]
# WOS[42490,]
# WOS[42491,]
WOS$AU <- with( WOS, ifelse( is.na(AU), CA, AU))

WOS$AF <- with( WOS, ifelse( is.na(AF), CA, AF))

##########################################################
# DISAMBIGUATE THE AUTHOR NAMES AND PARSE OUT ADDRESSES
##########################################################
WOS_cln<-authors_clean(WOS)
head(WOS$cln, 20)

# Now save the preliminary disambiguation as a csv file
write.csv(WOS_cln$prelim,"./output/WOS_prelim.csv")

# load to show (and avoid having to run every time)
WOS_cln_prelim<-read.csv("./output/WOS_prelim.csv")

# save the names suggested for review as a csv file
write.csv(WOS_cln$review,"./output/WOS_review.csv")

######################
# Accept the disambiguation or load / merge your corrections 
WOS_refined <- authors_refine(WOS_cln$review,WOS_cln$prelim)

# save the disambiguated data set
write.csv(WOS_refined,"./output/WOS_refined.csv")
######################

head(WOS_refined,10)
WOS_refined_country<-select(WOS_refined, refID, groupID,author_name,author_order,country)
head(WOS_refined_country,20)
write.csv(WOS_refined_country,"./output/WOS_refined_country.csv")

####################################################
# GEOREFERENCING AUTHORS
####################################################


# READ IN THE SAVED DISAMBIGUATED FILE
WOS_refined_country<-read.csv("./output/WOS_refined_country.csv")

# ADD IN THE COUNTRY CODES
library(countrycode) #convert each country name to the ISO 3 digit standardized country code.
WOS_refined_country$country2<-WOS_refined_country$country
# a few changes need to be made because countrycode doesn't recognize them as is
levels(WOS_refined_country$country2)<-c(levels(WOS_refined_country$country2),"UK","central african republic","papua new guinea","federated states of micronesia","netherlands antilles","republic of kosovo")
WOS_refined_country$country2[WOS_refined_country$country2 == "wales"]  <- "UK"
WOS_refined_country$country2[WOS_refined_country$country2 == "scotland"]  <- "UK"
WOS_refined_country$country2[WOS_refined_country$country2 == "england"]  <- "UK"
WOS_refined_country$country2[WOS_refined_country$country2 == "north ireland"]  <- "UK"
WOS_refined_country$country2[WOS_refined_country$country2 == "cent afr republ"]  <- "central african republic"
WOS_refined_country$country2[WOS_refined_country$country2 == "papua n guinea"]  <- "papua new guinea"
WOS_refined_country$country2[WOS_refined_country$country2 == "micronesia"]  <- "federated states of micronesia"
WOS_refined_country$country2[WOS_refined_country$country2 == "neth antilles"]  <- "netherlands antilles"
WOS_refined_country$country2[WOS_refined_country$country2 == "kosovo"]  <- "republic of kosovo"

WOS_refined_country$country_code<-countrycode(WOS_refined_country$country2,"country.name", "iso3c", warn = TRUE)
#By setting "warn=TRUE" it will tell you which ones it couldn't convert. Because of spelling mistakes, etc.
# note that a few are missing because they are not recognized.

WOS_refined_country$country_code<-as.factor(WOS_refined_country$country_code)
summary(WOS_refined_country$country_code)

head(WOS_refined_country,10)
str(WOS_refined_country)

paywall_Journal_author_countries<-WOS_refined_country

paywall_Journal_author_countries$X<-NULL
paywall_Journal_author_countries$groupID<-NULL
paywall_Journal_author_countries$author_name<-NULL
paywall_Journal_author_countries$country<-NULL
paywall_Journal_author_countries<-paywall_Journal_author_countries %>% rename("country"="country2")
paywall_Journal_author_countries<-paywall_Journal_author_countries %>% rename("author"="author_order")


head(paywall_Journal_author_countries,10)
head(X_articles_geodata,10)
str(paywall_Journal_author_countries)
str(WOS)
# Need to add in the doi, so, py so it matches the X journal output
# take WOS df and select the columns you need to insert
slim_WOS<-WOS %>% select(DI,SO,PY,refID)
# use left join to insert them
paywall_Journal_author_countries<-left_join(paywall_Journal_author_countries,slim_WOS,by="refID")
paywall_Journal_author_countries$SO<-tolower(paywall_Journal_author_countries$SO) # convert the journal name to lower case to match
paywall_Journal_author_countries$country<-toupper(paywall_Journal_author_countries$country) # convert the journal name to lower case to match
head(paywall_Journal_author_countries,10)

paywall_Journal_author_countries<-paywall_Journal_author_countries %>% 
  select(refID,DI,SO,PY,author,country,country_code) %>% 
  arrange(refID,author)
paywall_Journal_author_countries$jrnl_type<-"paywall"
paywall_Journal_author_countries$refID<-NULL
paywall_Journal_author_countries$SO<-as.factor(paywall_Journal_author_countries$SO)
write.csv(paywall_Journal_author_countries,"./output/paywall_Journal_author_countries.csv")


######################
str(X_articles_geodata)
str(paywall_Journal_author_countries)
ALLDATA<-bind_rows(X_articles_geodata,paywall_Journal_author_countries)
head(ALLDATA,10)
str(ALLDATA)
ALLDATA$DI<-as.factor(ALLDATA$DI)
ALLDATA$SO<-as.factor(ALLDATA$SO)
ALLDATA$country<-as.factor(ALLDATA$country)
ALLDATA$country_code<-as.factor(ALLDATA$country_code)
ALLDATA$jrnl_type<-as.factor(ALLDATA$jrnl_type)
write.csv(ALLDATA,"./output/AuthorGeoAllJournals.csv")

save(ALLDATA,file="./output/ALLDATA.RData")
load(file="./output/ALLDATA.RData")
head(ALLDATA,10)
str(ALLDATA)









##################################################################
# THIS IS TO GEOREF THE LOCATIONS FOR MAPPING
##################################################################
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








