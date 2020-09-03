library(tidyverse)
library(countrycode)
# library(devtools)
# devtools::install_github("ropensci/refsplitr")
library(refsplitr)
WOS<-references_read(data = './data_raw/raw_data_wos', dir = TRUE, include_all=FALSE)
# write.csv(WOS,"./output/WOS.csv")
# WOS<-read_csv("./output/WOS.csv")
# WOS<-select(WOS,-X1)
head(WOS,10)
colnames(WOS)
WOS_cln<-authors_clean(WOS)
WOS$AU <- with( WOS, ifelse( is.na(AU), CA, AU))
WOS$AF <- with( WOS, ifelse( is.na(AF), CA, AF))
WOS_cln<-authors_clean(WOS)
##########################################################
# DISAMBIGUATE THE AUTHOR NAMES AND PARSE OUT ADDRESSES
##########################################################
head(WOS_cln, 20)
# # Now save the preliminary disambiguation as a csv file
# write_csv(WOS_cln$prelim,"./output/WOS_prelim.csv")
# # load to show (and avoid having to run every time)
# WOS_cln_prelim<-read.csv("./output/WOS_prelim.csv")
# # save the names suggested for review as a csv file
# write_csv(WOS_cln$review,"./output/WOS_review.csv")
######################
# Accept the disambiguation or load / merge your corrections 
WOS_refined <- authors_refine(WOS_cln$review,WOS_cln$prelim)
# save the disambiguated data set
# write_csv(WOS_refined,"./output/WOS_refined.csv")
# WOS_refined<-read.csv("./output/WOS_refined.csv")
######################
colnames(WOS_refined)
WOS_refined$Code
colnames(WOS)
head(WOS_refined)

# comparison refsplitr and bibliometrix
# write_csv(WOS_refined,"./output/WOS_refined.csv")
# WOS_refined<-read_csv("./output/WOS_refined.csv")
colnames(WOS_refined)
WOS_refined_slim<-WOS_refined %>% select(author_name,author_order,
                                              country,RP_address,refID,PY)
WOS_slim<-WOS %>% select(DI,TI,PY,SO,refID)
colnames(WOS_slim)
colnames(WOS_refined_slim)
str(WOS_slim)
str(WOS_refined_slim)

WOS_refined_slim<-as.data.frame(WOS_refined_slim)
WOS_slim<-as.data.frame(WOS_slim)


WOS_refsplitr<-left_join(WOS_refined_slim,WOS_slim,by=c("refID","PY"))
WOS_refsplitr<-arrange(WOS_refsplitr,refID,author_order)
WOS_refsplitr$database<-"wos"


WOS_refsplitr$country<-gsub("england","UK",WOS_refsplitr$country)
WOS_refsplitr$country<-gsub("north ireland","UK",WOS_refsplitr$country)
WOS_refsplitr$country<-gsub("papua n guinea","PNG",WOS_refsplitr$country)
WOS_refsplitr$country<-gsub("scotland","UK",WOS_refsplitr$country)
WOS_refsplitr$country<-gsub("wales","UK",WOS_refsplitr$country)

WOS_refsplitr$Code<-
  countrycode(WOS_refsplitr$country,"country.name", "iso3c", warn = TRUE)
# convert variables to factor
WOS_refsplitr$Code<-as.factor(WOS_refsplitr$Code)



################################################################
# Load the list of Pawall Journals and their OA mirrors 
################################################################
MirrorPairs<-read_csv(file="./data_raw/MirrorPairs.csv")
colnames(MirrorPairs)
WOS_refsplitr<-left_join(WOS_refsplitr,MirrorPairs,by="SO") 


rm(WOS,
   WOS_refined,
   WOS_refined_slim,
   WOS_slim,
   MirrorPairs)


write_csv(WOS_refsplitr,"./output/WOS_refsplitr.csv")
# WOS_refsplitr<-read_csv("./output/WOS_refsplitr.csv")
colnames(WOS_refsplitr)
WOS_refsplitr<-WOS_refsplitr %>% select(refID, database, DOI = DI, pair_key,Journal = SO, 
       JrnlType = journal_cat, Year = PY, Title=TI,
       AuthorNum = author_order, Country = country)

WOS_refsplitr$refID<-as.factor(WOS_refsplitr$refID)
WOS_refsplitr$database<-as.factor(WOS_refsplitr$database)
WOS_refsplitr$DOI<-as.factor(WOS_refsplitr$DOI)
WOS_refsplitr$pair_key<-as.factor(WOS_refsplitr$pair_key)
WOS_refsplitr$Journal<-as.factor(WOS_refsplitr$Journal)
WOS_refsplitr$JrnlType<-as.factor(WOS_refsplitr$JrnlType)
WOS_refsplitr$Year<-as.factor(WOS_refsplitr$Year)
WOS_refsplitr$Title<-as.character(WOS_refsplitr$Title)
WOS_refsplitr$Title<-tolower(WOS_refsplitr$Title)
WOS_refsplitr$Country<-as.factor(WOS_refsplitr$Country)

summary(WOS_refsplitr)
WOS_refsplitr$Country<-gsub("england","UK",WOS_refsplitr$Country)
WOS_refsplitr$Country<-gsub("north ireland","UK",WOS_refsplitr$Country)
WOS_refsplitr$Country<-gsub("papua n guinea","PNG",WOS_refsplitr$Country)
WOS_refsplitr$Country<-gsub("scotland","UK",WOS_refsplitr$Country)
WOS_refsplitr$Country<-gsub("wales","UK",WOS_refsplitr$Country)

WOS_refsplitr$Code<-
  countrycode(WOS_refsplitr$Country,"country.name", "iso3c", warn = TRUE)


write_csv(WOS_refsplitr,"./data_clean/WOS_refsplitr.csv")

# refsplitr dataset 
WOS_refsplitr
#################
colnames(AllData_AuthorFirst)
colnames(AllData_Affiliations)
colnames(AllData_AuGeo)
colnames(AllData_CorrAffil)




# Bibliometrix datasets
missing_jrnls<-c(9,13,16)
AllData_Authors<-read_csv("./data_clean/AllData_Authors.csv")
AllData_Authors<-left_join(AllData_Authors,MirrorPairs,by="SO") 
AllData_Authors<-select(AllData_Authors,-notes) #remove notes column
AllData_Authors<-AllData_Authors %>% filter(!pair_key%in% missing_jrnls)
#####
AllData_CorrAffil<-read_csv("./data_clean/AllData_CorrAffil.csv")
AllData_CorrAffil<-AllData_CorrAffil %>% filter(!pair_key%in% missing_jrnls)

colnames(AllData_CorrAffil)
AllData_CorrAffil <- AllData_CorrAffil %>%
  select(database, DOI = DI, pair_key,Journal = SO, 
         JrnlType = journal_cat, Year = PY, Title=TI,Authors=AU,
         corr_affil)
####
AllData_Affiliations<-read_csv("./data_clean/AllData_Affiliations.csv")
AllData_Affiliations<-AllData_Affiliations %>% filter(!pair_key%in% missing_jrnls)

colnames(AllData_Affiliations)
AllData_Affiliations <- AllData_Affiliations %>%
  select(database, DOI = DI, pair_key,Journal = SO, 
         JrnlType = journal_cat, Year = PY, Title=TI,
         Authors=AU, AU1_UN,AffiliationNum, Inst)
####
missing_jrnls<-c(9,13,16)
AllData_AuthorFirst<-read_csv("./data_clean/AllData_AuthorFirst.csv")
colnames(AllData_AuthorFirst)
# AllData_AuthorFirst<-left_join(AllData_AuthorFirst,MirrorPairs,by="SO") 
# AllData_AuthorFirst<-select(AllData_AuthorFirst,-notes) #remove notes column
AllData_AuthorFirst<-AllData_AuthorFirst %>% filter(!pair_key%in% missing_jrnls)
# AllData_AuthorFirst$AuthorNum<-1
colnames(AllData_AuthorFirst)
AllData_AuthorFirst <- AllData_AuthorFirst %>%
  select(database, DOI = DI, pair_key,Journal = SO, 
         JrnlType = journal_cat, Year = PY, Title=TI,Authors=AU,
         Institutions=AU_UN,AuthorNum,Country=AU1_CO)

AllData_AuthorFirst$Country<-gsub("england","UK",AllData_AuthorFirst$Country)
AllData_AuthorFirst$Country<-gsub("north ireland","UK",AllData_AuthorFirst$Country)
AllData_AuthorFirst$Country<-gsub("papua n guinea","PNG",AllData_AuthorFirst$Country)
AllData_AuthorFirst$Country<-gsub("scotland","UK",AllData_AuthorFirst$Country)
AllData_AuthorFirst$Country<-gsub("wales","UK",AllData_AuthorFirst$Country)

AllData_AuthorFirst$Code<-
  countrycode(AllData_AuthorFirst$Country,"country.name", "iso3c", warn = TRUE)
############
AllData_AuGeo<-read_csv("./data_clean/AllData_AuGeo.csv")
AllData_AuGeo<-AllData_AuGeo %>% filter(!pair_key%in% missing_jrnls)
colnames(AllData_AuGeo)
AllData_AuGeo <- AllData_AuGeo %>%
  select(database, DOI = DI, pair_key,Journal = SO, 
         JrnlType = journal_cat, Year = PY, Title=TI,Authors=AU,
         Institutions=AU_UN,Corr_Author_Inst=AU1_UN,
         Country = country)

AllData_AuGeo$Country<-gsub("england","UK",AllData_AuGeo$Country)
AllData_AuGeo$Country<-gsub("north ireland","UK",AllData_AuGeo$Country)
AllData_AuGeo$Country<-gsub("papua n guinea","PNG",AllData_AuGeo$Country)
AllData_AuGeo$Country<-gsub("scotland","UK",AllData_AuGeo$Country)
AllData_AuGeo$Country<-gsub("wales","UK",AllData_AuGeo$Country)
AllData_AuGeo$Code<-
  countrycode(AllData_AuGeo$Country,"country.name", "iso3c", warn = TRUE)
##########

#####################################################################
# COMPARISON 1: FIRST AUTHORS REFSPLITR VS. BIBLIOMETRIX
#####################################################################
colnames(AllData_AuthorFirst)
colnames(WOS_refsplitr)
AllData_AuthorFirst$Year<-as.numeric(AllData_AuthorFirst$Year)
AllData_AuthorFirst$Country<-as.character(AllData_AuthorFirst$Country)
AllData_AuthorFirst$Country<-noquote(AllData_AuthorFirst$Country)
AllData_AuthorFirst$Country<-tolower(AllData_AuthorFirst$Country)

WOS_refsplitr$Country
WOS_refsplitr$Country<-noquote(WOS_refsplitr$Country)
WOS_refsplitr$Year<-as.numeric(WOS_refsplitr$Year)

wos_1<-WOS_refsplitr %>% filter(AuthorNum==1) %>% select(-refID,-database,-pair_key,-JrnlType,-Title,-Year)
scopus_1 <-AllData_AuthorFirst %>% filter(database=="wos") %>% select(-database,-pair_key,-JrnlType,-Title,-Year)
wos_scopus_1<-full_join(wos_1,scopus_1,by=c("DOI","Journal","AuthorNum"))
colnames(wos_scopus_1)
wos_scopus_1$Code_check<-(wos_scopus_1$Code.x==wos_scopus_1$Code.y)
summary(wos_scopus_1$Code_check)


# from extraction
Country2<-biblioAnalysis(articles_wos_df)
Country<-Country2$CO
Country<-tolower(Country)
Country<-as.data.frame(Country)
colnames(Country)
DOI<-articles_wos_df$DI
DOI<-as.data.frame(DOI)
colnames(DOI)
Country<-bind_cols(DOI,Country)
wos_scopus_1<-full_join(wos_scopus_1,Country,by="DOI")
colnames(wos_scopus_1)
wos_scopus_1$Country<-gsub("england","UK",wos_scopus_1$Country)
wos_scopus_1$Country<-gsub("north ireland","UK",wos_scopus_1$Country)
wos_scopus_1$Country<-gsub("papua n guinea","PNG",wos_scopus_1$Country)
wos_scopus_1$Country<-gsub("scotland","UK",wos_scopus_1$Country)
wos_scopus_1$Country<-gsub("wales","UK",wos_scopus_1$Country)
wos_scopus_1$Code.z<-
  countrycode(wos_scopus_1$Country,"country.name", "iso3c", warn = TRUE)
colnames(wos_scopus_1)
wos_scopus_1$Code_check1v3<-(wos_scopus_1$Code.x==wos_scopus_1$Code.z)
summary(wos_scopus_1$Code_check1v3)
wos_scopus_1$Code_check2v3<-(wos_scopus_1$Code.y==wos_scopus_1$Code.z)
summary(wos_scopus_1$Code_check2v3)


colnames(wos_scopus_1)
wos_scopus_1<-wos_scopus_1 %>% rename(Country.refsplitr=Country.x,
                                      Country.AU1_CO=Country.y,
                                      Country.BiblioA=Country,
                                      Code.refsplitr=Code.x,
                                      Code.AU1_CO=Code.y,
                                      Code.BiblioA=Code.z,
                                      refsplitrvAU1CO=Code_check,
                                      refsplitrvBiblioA=Code_check1v3,
                                      AU1_COvBiblioA=Code_check2v3)


wos_scopus_1<-wos_scopus_1 %>% select(Journal, AuthorNum, 
                                      Authors, Institutions,DOI,
                                      Country.refsplitr, 
                                      Country.AU1_CO,Country.BiblioA,
                                      Code.refsplitr,Code.AU1_CO,
                                      Code.BiblioA, refsplitrvAU1CO,
                                      refsplitrvBiblioA,AU1_COvBiblioA)


summary(wos_scopus_1$refsplitrvAU1CO)
summary(wos_scopus_1$refsplitrvBiblioA)
summary(wos_scopus_1$AU1_COvBiblioA)

wos_diff_summary<-as.data.frame(table(wos_scopus_1$refsplitrvAU1CO))
wos_diff_summary$perc.false<-wos_diff_summary$Freq/sum(wos_diff_summary$Freq)*100
sum(wos_diff_summary$Freq)
wos_diff_summary


wos_diff_summary<-as.data.frame(table(wos_scopus_1$refsplitrvBiblioA))
wos_diff_summary$perc.false<-wos_diff_summary$Freq/sum(wos_diff_summary$Freq)*100
sum(wos_diff_summary$Freq)
wos_diff_summary


wos_diff_summary<-as.data.frame(table(wos_scopus_1$AU1_COvBiblioA))
wos_diff_summary$perc.false<-wos_diff_summary$Freq/sum(wos_diff_summary$Freq)*100
sum(wos_diff_summary$Freq)
wos_diff_summary


########
# DUPLICaTED DOI 
#######
# refsplitr
WOS<-read_csv("./output/WOS.csv")
nrow(WOS)
DupeCount_wos_refsplitr<-WOS %>% 
  group_by(DI) %>%
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(n==2)
DupeCount_wos_refsplitr
perc_duped_wos_refsplitr<-sum(DupeCount_wos_refsplitr$n)/nrow(WOS)*100
perc_duped_wos_refsplitr


nrow(articles_wos_df)
DupeCount_wos_biblio<-articles_wos_df %>% 
  group_by(DI) %>%
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(n==2)
DupeCount_wos_biblio

duped_wos<-sum(DupeCount_wos_biblio$n)
records_wos<-nrow(articles_wos_df)
perc_duped_wos<-duped_wos/records_wos*100
perc_duped_wos



DupeCount_scopus_biblio<-articles_scopus_df %>% 
  group_by(DI) %>%
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(n==2)
DupeCount_scopus_biblio
duped_scopus<-sum(DupeCount_scopus_biblio$n)
records_scopus<-nrow(articles_scopus_df)
perc_duped_scopus<-duped_scopus/records_scopus*100
perc_duped_scopus

################################
#TODO 
# 1) eliminate doi as a sorting mechanism - BETTER - at a random number (.xxxx) to each DOI-after import but before processing
# 2) get corresponding author country data 
# 3) rerun analyses with 1st author country and corresponding (instead of last)








################################################################
# load and add World Bank data on national income categories
CountryData <- read.csv("data_raw/CLASS.csv", header = TRUE)
CountryData <- CountryData[-1,]
CountryData <- CountryData %>%
  select(Code,Region, IncomeGroup = Income.group)

























wos_scopus_1<-wos_scopus_1 %>% select(refID, database.x,database.y,
                                      DOI,pair_key.x,pair_key.y,Journal,
                                      JrnlType.x,JrnlType.y,Year,Authors,
                                      Institutions,Title.x,Title.y,AuthorNum,
                                      Country.x, Country.y,Code.x,Code.y)



AllData_CorrAffil










colnames(AllData)
colnames(WOS_refsplitr)

WOS_biblio<-filter(AllData,database=="wos")

WOS_biblio$refID<-as.factor(WOS_biblio$refID)
WOS_biblio$database<-as.factor(WOS_biblio$database)
WOS_biblio$DOI<-as.factor(WOS_biblio$DOI)
WOS_biblio$pair_key<-as.factor(WOS_biblio$pair_key)
WOS_biblio$Journal<-as.factor(WOS_biblio$Journal)
WOS_biblio$JrnlType<-as.factor(WOS_biblio$JrnlType)
WOS_biblio$Year<-as.factor(WOS_biblio$Year)
WOS_biblio$Title<-as.character(WOS_biblio$Title)
WOS_biblio$Title<-tolower(WOS_biblio$Title)
WOS_biblio$Country<-as.factor(WOS_biblio$Country)


wos_compare<-full_join(WOS_refsplitr,WOS_biblio,by=c("DOI","Journal","Year","AuthorNum"))







# Comparing Biblio metrix and refsplitr
WOS_refined_country<-WOS_refined %>% 
  select(refID, groupID,author_name,author_order,country) %>% 
  arrange(refID,author_order)


wos_refsplitr_au<-WOS_refined_country
WOS_doi<-select(WOS,refID,DI)

colnames(wos_refsplitr_au)
colnames(WOS_doi)

wos_refsplitr_au<-left_join(wos_refsplitr_au,WOS_doi,by="refID")
wos_refsplitr_au$country<-gsub("england","UK",wos_refsplitr_au$country)
wos_refsplitr_au$country<-gsub("north ireland","UK",wos_refsplitr_au$country)
wos_refsplitr_au$country<-gsub("papua n guinea","PNG",wos_refsplitr_au$country)
wos_refsplitr_au$country<-gsub("scotland","UK",wos_refsplitr_au$country)
wos_refsplitr_au$country<-gsub("wales","UK",wos_refsplitr_au$country)

wos_refsplitr_au$country_code<-
  countrycode(wos_refsplitr_au$country,"country.name", "iso3c", warn = TRUE)
# convert variables to factor
wos_refsplitr_au$country_code<-as.factor(wos_refsplitr_au$country_code)
# comparison refsplitr and bibliometrix

# diff in number of authors (refsplitr>bibliometrix)
nrow(wos_refsplitr_au)-nrow(wos_biblio_au_slim)

# comparing first author country
# 1st author refsplitr
refsplitr_1st<-wos_refsplitr_au %>% 
  group_by(refID) %>% 
  filter(author_order==1)
nrow(refsplitr_1st)
colnames(refsplitr_1st)
#1st author biblio
biblio_1st<-wos_biblio_au_slim %>% 
  group_by(key) %>% 
  filter(author==1)
nrow(biblio_1st)
colnames(biblio_1st)

summary(refsplitr_1st$country_code)
summary(biblio_1st$country_code)

first_authors<-full_join(refsplitr_1st,biblio_1st,by="DI")

first_authors$country_code.x<-as.character(first_authors$country_code.x)
first_authors$country_code.y<-as.character(first_authors$country_code.y)
first_authors$country_check<-first_authors$country_code.x==first_authors$country_code.y
summary(first_authors$country_check)

938/(5827+938)


#################

colnames(WOS)
colnames(scopus_biblio_au)

# want these from bibliometrix
intersect(colnames(WOS),colnames(scopus_biblio_au))

c1[6]
setdiff(colnames(WOS),colnames(scopus_biblio_au))
head(WOS$PD,10)
colnames(scopus_biblio_au)
scopus_biblio_conversion<-scopus_biblio_au
scopus_biblio_conversion$filename<-NA
scopus_biblio_conversion$AF<-scopus_biblio_conversion$AU
scopus_biblio_conversion$CA<-NA
scopus_biblio_conversion$PD<-NA
scopus_biblio_conversion$WC<-NA
scopus_biblio_conversion$PG<-NA
scopus_biblio_conversion$RI<-NA
scopus_biblio_conversion$Z9<-NA
scopus_biblio_conversion$OI<-NA
scopus_biblio_conversion$SC<-NA
scopus_biblio_conversion$UT<-NA
scopus_biblio_conversion$PT<-"J"
scopus_biblio_conversion$refID<-scopus_biblio_conversion$key
scopus_biblio_conversion$FN<-"scopus"
#can extract EM (email) from RP
scopus_biblio_conversion$EM<-NA
# could split PP to get these
scopus_biblio_conversion$BP<-NA
scopus_biblio_conversion$EP<-NA
setdiff(colnames(WOS),colnames(scopus_biblio_conversion))
colnames(WOS)
colnames(scopus_biblio_conversion)

foo<-scopus_biblio_conversion %>% select(colnames(WOS))
colnames(foo)==colnames(WOS)
foo$AU<-gsub(";", "\n",foo$AU)
foo$AF<-gsub(";", "\n",foo$AF)
foo$EM<-"foo@foo.com"
foo$RI<-"abc123"
foo$OI<-"abc123"
foo_cln<-authors_clean(foo)
head(foo$AU,10)
head(WOS$AU,10)
foo<-as.data.frame(foo)
write.csv(WOS[1:10],"./output/WOS_check.csv")
write.csv(foo[1:10],"./output/foo_check.csv")
cell<-c("KUMAR A
KUMAR V
RATTAN V
JHA V
BHATTACHARYYA S")


cell<-foo$AU[2]
# split the names
author_names<-str_split(cell, "\n", simplify = TRUE)
# This gets the first word from each (ie, the last name)
last_name<-word(author_names, 1)
# extract all but the first words
# this extracts the rest of the words (i.e., everything after last name)
first_mid_names<-word(author_names, 2, -1)
names_almost<-paste(last_name,first_mid_names,sep=", ")
as.data.frame(names_almost)
# union(x,y): Union of the sets x and y
# intersect(x,y): Intersection of the sets x and y
# setdiff(x,y): Set difference between x and y, consisting of all elements of x that are not in y
# setequal(x,y): Test for equality between x and y
inst<-foo$C1[2]



