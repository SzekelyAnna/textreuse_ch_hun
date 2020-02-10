###Texts pre-processing and dissimilarity calculation###
#code that follows was run on an UBUNTU 18.04.03 LTS operating system

rm(list=ls(all=TRUE))

library(dplyr)
library(textreuse)
library(ngram)
library(qdap)

library(stringr)
library(stringi)
library(pdftools)
library(reshape2)
library(tm)


LANG="en_US.UTF-8"

gdir<-"C:/_swisshun/swiss/"
setwd("C:/_swisshun/swiss/")

source("clean_bill_ch_extract_input.R")
source("clean_bill_ch_extract_output.R")
source("txt_prep_ch_fr.R")

pages_input <- read.csv("./pages_input.csv",stringsAsFactors = F,
                  fileEncoding = "utf-8")
pages_output <- read.csv("./pages_output.csv",stringsAsFactors = F,
                        fileEncoding = "utf-8")

#First pre-processing and conversion of PDF to txt (while extracting the correct pages)

for (i in 1:nrow(pages_input)){
  #i=1
  txt <- paste(paste0(gdir,"input bills/"),pages_input$bill_id[i],".pdf",sep="")
  txt_file <- clean_bill_ch_extract_input(txt)
  txt_file[1]
    
  setwd(paste0(gdir,"input txt/"))
  file_name <- paste(pages_input$bill_id[i], ".txt",sep="")
  fileConn<-file(file_name)
  writeLines(txt_file, fileConn)
  close(fileConn)
  
}

for (i in 1:nrow(pages_output)){
  #i=1
  txt <- paste(paste0(gdir,"output bills/"),pages_output$bill_id[i],".pdf",sep="")
  txt_file <- clean_bill_ch_extract_output(txt)
  txt_file[1]
  
  setwd(paste0(gdir,"output txt/"))
  file_name <- paste(pages_output$bill_id[i], ".txt",sep="")
  fileConn<-file(file_name)
  writeLines(txt_file, fileConn)
  close(fileConn)
  
}



bills <- read.csv(paste0(gdir,"swiss_bills.csv"),stringsAsFactors = F,
                  fileEncoding = "utf-8")
bills$dissimilarity_n2_rep <- NA

#second part of pre-processing and calculation of dissimilarity score
for (i in 1:nrow(bills)) {
  #i=1
  bname1_path <- paste(paste0(gdir,"input txt/"), paste0(bills$bill_id[i], sep = ""), paste(".txt", sep = ""), sep = "")
  bname1 <- bname1_path
  
  bname2_path <- paste(paste0(gdir,"output txt/"), paste0(bills$bill_id[i], sep = ""), paste(".txt", sep = ""), sep = "")
  bname2 <- bname2_path
  
  
  doc1a <- paste(qdap::clean(readLines(bname1)), collapse = "\n")
  doc2a <- paste(qdap::clean(readLines(bname2)), collapse = "\n")
  

  doc1 <- txt_prep_ch_fr(doc1a)
  doc2 <- txt_prep_ch_fr(doc2a)
  
  doc1 <- preprocess(doc1, case="lower",remove.punct = TRUE,remove.numbers = TRUE,fix.spacing = TRUE)
  doc1 <- gsub(" *\\b[[:alpha:]]{1,1}\\b *"," ",doc1)
  
  doc2 <- preprocess(doc2, case="lower",remove.punct = TRUE,remove.numbers = TRUE,fix.spacing = TRUE)
  doc2 <- gsub(" *\\b[[:alpha:]]{1,1}\\b *"," ",doc2)
  
  
  doc1_ngrams2 <- textreuse::tokenize_ngrams(doc1, n=2)
  doc2_ngrams2 <- textreuse::tokenize_ngrams(doc2, n=2)

  bills$dissimilarity_n2_rep[i] <- jaccard_dissimilarity(doc1_ngrams2, doc2_ngrams2)
  
}

# BT: save dissim
write.table(bills[,c(1,24)], paste0(gdir,"rep_19_12.csv"), col.names=T, sep="|", na = "",  qmethod = "double")

