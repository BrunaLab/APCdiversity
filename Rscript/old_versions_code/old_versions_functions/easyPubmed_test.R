library(easyPubMed)
library(httr)

myQuery <- "p53 AND Chicago[Affiliation] AND 2019[PDAT]" 
myQuery <- "10.1016/j.cplett.2020.137294[doi]"
myQuery <-'ELTOUKHY[AU] AND "2019"[PDAT]'
myIdList <- get_pubmed_ids(myQuery)

new_query <- 'Bladder[TIAB] AND Northwestern[AD] AND Chicago[AD] AND "2018"[PDAT]' 
out.A <- batch_pubmed_download(pubmed_query_string = new_query, 
                               format = "xml", 
                               batch_size = 20,
                               dest_file_prefix = "easyPM_example",
                               encoding = "ASCII")
print(out.A) 
my_PM_list <- articles_to_list(pubmed_data = my_abstracts_xml)
class(my_PM_list[1])

my_query <- 'Analytica Chimica Acta: X[TA]'
my_query <- '10.1016/j.ccell.2017.08.015[AID]'
myIdList <- get_pubmed_ids(my_query)
# my_query <- 'Damiano Fantini[AU] AND "2018"[PDAT]'
my_entrez_id <- get_pubmed_ids(my_query)
my_abstracts_txt <- fetch_pubmed_data(my_entrez_id, format = "abstract")
my_abstracts_xml <- fetch_pubmed_data(pubmed_id_list = my_entrez_id)
class(my_abstracts_xml) 
my_PM_list <- articles_to_list(pubmed_data = my_abstracts_xml)
# class(my_PM_list[1])
print(my_PM_list)
# print(substr(my_PM_list[4], 1, 510))
#the query produced the following number of results
as.integer(as.character(myIdList$Count))
# curr_PM_record <- my_PM_list[1]
curr_PM_record <- my_PM_list
custom_grep(curr_PM_record, tag = "PubDate")
custom_grep(curr_PM_record, tag = "LastName", format = "char")
# curr_PM_record <- easyPubMed::EPMsamples$NUBL_1618$rec_lst[[37]]


my.df <- article_to_df(curr_PM_record, max_chars = 18)
head(colnames(my.df))
my.df$title <- substr(my.df$title, 1, 15)
my.df$address <- substr(my.df$address, 1, 19)
my.df$jabbrv <- substr(my.df$jabbrv, 1, 10)
my.df2 <- article_to_df(curr_PM_record, autofill = TRUE)
my.df2$title <- substr(my.df2$title, 1, 15)
my.df2$jabbrv <- substr(my.df2$jabbrv, 1, 10)
my.df2$address <- substr(my.df2$address, 1, 50)
xx <- lapply(my_PM_list, article_to_df, autofill = TRUE, max_chars = 50)
full_df <- do.call(rbind, xx)


full_df[seq(1, nrow(full_df), by = 10), c("pmid", "lastname", "jabbrv")] 

#this is the unique WebEnv String
myIdList$WebEnv

#the PubMed ID of the first record produced by this query is the following
myIdList$IdList[[1]]
article_to_df()