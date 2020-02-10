################################################################################
############ Hungarian bills with original Swiss preprocessing #################
################################################################################

# Function for calculating Jaccard-based dissimilarity indices (1 to 5-grams)
# Includes some text preprocessing: removing special characters, numbers,
# "common words", stopwords

gdir<-"C:/_swisshun/hun/"

common_words <- c("i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x","xi",
									"xiii","xiv","xv","xvi","xvii", "xviii" , "xix", "xx","xx","xxi",
									"xxii","xxiii","xxiv","xxv","xxvi","xxvii","xxviii","xxix","xxx",
									"január", "február", "március", "április", "május", "június",
									"július", "augusztus", "szeptember", "október", "november", "december")

dissim <- function(draft,law){
	txt7 <- draft
	txt8 <- law
	for (i in c(1:20)) { #length(txt7$text)
		if (is.na(txt7$text[i])==F & is.na(txt8$text1[i])==F){
			txt7$text[i] <- preprocess(txt7$text[i], remove.punct = TRUE,
																 remove.numbers = TRUE,fix.spacing = TRUE)
			txt7$text[i] <- gsub(" *\\b[[:alpha:]]{1,1}\\b *"," ",txt7$text[i])
			txt7$text[i] <- removeWords(txt7$text[i], common_words)
			txt7$text[i] <- removeWords(txt7$text[i], stopwords("hu"))
			txt7$text[i] <- gsub("[\r\n]", " ", txt7$text[i])
			txt7$text[i] <- trimws(txt7$text[i])
			
			txt8$text1[i] <- preprocess(txt8$text1[i], remove.punct = TRUE,
																	remove.numbers = TRUE,fix.spacing = TRUE)
			txt8$text1[i] <- gsub(" *\\b[[:alpha:]]{1,1}\\b *"," ",txt8$text1[i])
			txt8$text1[i] <- removeWords(txt8$text1[i], common_words)
			txt8$text1[i] <- removeWords(txt8$text1[i], stopwords("hu"))
			txt8$text1[i] <- gsub("[\r\n]", " ", txt8$text1[i])
			txt8$text1[i] <- trimws(txt8$text1[i])
			if (i%%50 == 0){
				print(i)
			}
		}
	}
	n <- length(txt7$text)
	dissims <- data.frame(bill_id = character(n),
												#dissimilarity_n1=double(n),
												dissimilarity_n2=double(n),
												#dissimilarity_n3=double(n),
												#dissimilarity_n4=double(n),
												#dissimilarity_n5=double(n),
												stringsAsFactors=FALSE)
	
	for (i in c(1:20)) { #length(txt7$text)
		dissims$bill_id[i] <- txt7$doc_id[i]
		if (is.na(txt7$text[i]) | is.na(txt8$text1[i])){
			print(i)
			dissims[i,2] <- c('NA')}
		else {
			doc1 <- txt7$text[i]
			doc2 <- txt8$text1[i]
			
			#doc1_ngrams1 <- textreuse::tokenize_ngrams(doc1, n=1)
			#doc2_ngrams1 <- textreuse::tokenize_ngrams(doc2, n=1)
			
			doc1_ngrams2 <- textreuse::tokenize_ngrams(doc1, n=2)
			doc2_ngrams2 <- textreuse::tokenize_ngrams(doc2, n=2)
			
			#doc1_ngrams3 <- textreuse::tokenize_ngrams(doc1, n=3)
			#doc2_ngrams3 <- textreuse::tokenize_ngrams(doc2, n=3)
			
			#doc1_ngrams4 <- textreuse::tokenize_ngrams(doc1, n=4)
			#doc2_ngrams4 <- textreuse::tokenize_ngrams(doc2, n=4)
			
			#doc1_ngrams5 <- textreuse::tokenize_ngrams(doc1, n=5)
			#doc2_ngrams5 <- textreuse::tokenize_ngrams(doc2, n=5)
			
			dissims$length_words_input[i] <- length(doc1_ngrams2)
			
			#dissims$dissimilarity_n1[i] <- jaccard_dissimilarity(doc1_ngrams1, doc2_ngrams1)
			dissims$dissimilarity_n2[i] <- jaccard_dissimilarity(doc1_ngrams2, doc2_ngrams2)
			#dissims$dissimilarity_n3[i] <- jaccard_dissimilarity(doc1_ngrams3, doc2_ngrams3)
			#dissims$dissimilarity_n4[i] <- jaccard_dissimilarity(doc1_ngrams4, doc2_ngrams4)
			#dissims$dissimilarity_n5[i] <- jaccard_dissimilarity(doc1_ngrams5, doc2_ngrams5)
			
			if (i%%50 == 0){
				print(i)
			}
		}
	}
	return(dissims)
}

library(dplyr)
library(textreuse)
library(ngram)
library(qdap)
library(tm)
library(data.table)
library(readxl)
library(stringr)

# 1994-1998
txt7<-readRDS(file=paste0(gdir,"9498txt7.Rda"))
txt8<-readRDS(file=paste0(gdir,"9498txt8.Rda"))

dissim9498 <- dissim(txt7,txt8)
sum(dissim9498$dissimilarity_n2==0)
write.csv(dissim9498, paste0(gdir,"dissim9498_v2.csv"), fileEncoding = "utf-8", row.names = FALSE)


# 1998-2002
txt7<-readRDS(file=paste0(gdir,"9802txt7.Rda"))
txt8<-readRDS(file=paste0(gdir,"9802txt8.Rda"))

dissim9802 <- dissim(txt7,txt8)
sum(dissim9802$dissimilarity_n2==0)
summary(dissim9802$dissimilarity_n2)
write.csv(dissim9802, paste0(gdir,"dissim9802_v2.csv"), fileEncoding = "utf-8", row.names = FALSE)


# 2002-2006
txt7<-readRDS(file=paste0(gdir,"0206txt7.Rda"))
txt8<-readRDS(file=paste0(gdir,"0206txt8.Rda"))

dissim0206 <- dissim(txt7,txt8)
sum(dissim0206$dissimilarity_n2==0)
summary(dissim0206$dissimilarity_n2)
write.csv(dissim0206, paste0(gdir,"dissim0206_v2.csv"), fileEncoding = "utf-8", row.names = FALSE)


# 2006-2010
txt7<-readRDS(file=paste0(gdir,"0610txt7.Rda"))
txt8<-readRDS(file=paste0(gdir,"0610txt8.Rda"))

dissim0610 <- dissim(txt7,txt8)
sum(dissim0610$dissimilarity_n2==0)
summary(dissim0610$dissimilarity_n2)
write.csv(dissim0206, paste0(gdir,"dissim0610_v2.csv"), fileEncoding = "utf-8", row.names = FALSE)

# 2010-2014
txt7<-readRDS(file=paste0(gdir,"1014txt7.Rda"))
txt8<-readRDS(file=paste0(gdir,"1014txt8.Rda"))

dissim1014 <- dissim(txt7,txt8)
sum(dissim1014$dissimilarity_n2==0)
summary(dissim1014$dissimilarity_n2)
write.csv(dissim0206, paste0(gdir,"dissim1014_v2.csv"), fileEncoding = "utf-8", row.names = FALSE)


# 2014-2018
txt7<-readRDS(file=paste0(gdir,"1418txt7.Rda"))
txt8<-readRDS(file=paste0(gdir,"1418txt8.Rda"))

dissim1418 <- dissim(txt7,txt8)
sum(dissim1418$dissimilarity_n2==0)
summary(dissim1418$dissimilarity_n2)

rel <- data.table(read_excel(paste0(gdir,"relate.xlsx"), sheet = "1418"))
colnames(rel)<-c("tv","bill_id")

rel$bill_id<-str_squish(rel$bill_id)
dissim1418<-merge(rel,dissim1418,by=c("bill_id"))


write.csv(dissim0206, paste0(gdir,"dissim1418_v2.csv"), fileEncoding = "utf-8", row.names = FALSE)

dissim_all <- rbind(dissim9498,dissim9802,dissim0206,
										dissim0610,dissim1014,dissim1418)
summary(as.numeric(dissim_all$dissimilarity_n2))

sum(dissim_all$dissimilarity_n2==0)

write.csv(dissim_all, "C:/_swisshun/hun_dissim/dissim_all_hun.csv", fileEncoding = "utf-8", row.names = FALSE)





rel <- data.table(read_excel(paste0(gdir,"relate.xlsx"), sheet = "9498"))
d9498<-data.table(read.csv(paste0(gdir,"dissim9498_v2.csv")))
colnames(rel)<-c("tv","bill_id")
rel$bill_id<-str_squish(rel$bill_id)
dissim_all<-merge(rel,d9498,by=c("bill_id"))

rel <- data.table(read_excel(paste0(gdir,"relate.xlsx"), sheet = "9802"))
d9802<-data.table(read.csv(paste0(gdir,"dissim9498_v2.csv")))
colnames(rel)<-c("tv","bill_id")
rel$bill_id<-str_squish(rel$bill_id)
dissim_all<-rbind(dissim_all,merge(rel,d9802,by=c("bill_id")))


rel <- data.table(read_excel(paste0(gdir,"relate.xlsx"), sheet = "0206"))
d0206<-data.table(read.csv(paste0(gdir,"dissim9498_v2.csv")))
colnames(rel)<-c("tv","bill_id")
rel$bill_id<-str_squish(rel$bill_id)
dissim_all<-rbind(dissim_all,merge(rel,d0206,by=c("bill_id")))

rel <- data.table(read_excel("C:/_swisshun/hun/relate.xlsx", sheet = "0610"))
d0610<-data.table(read.csv(paste0(gdir,"dissim9498_v2.csv")))
colnames(rel)<-c("tv","bill_id")
rel$bill_id<-str_squish(rel$bill_id)
dissim_all<-rbind(dissim_all,merge(rel,d0610,by=c("bill_id")))

rel <- data.table(read_excel(paste0(gdir,"relate.xlsx"), sheet = "1014"))
d1014<-data.table(read.csv(paste0(gdir,"dissim9498_v2.csv")))
colnames(rel)<-c("tv","bill_id")
rel$bill_id<-str_squish(rel$bill_id)
dissim_all<-rbind(dissim_all,merge(rel,d1014,by=c("bill_id")))

rel <- data.table(read_excel(paste0(gdir,"relate.xlsx"), sheet = "1418"))
d1418<-data.table(read.csv(paste0(gdir,"dissim9498_v2.csv")))
d1418[,2]<-NULL
colnames(rel)<-c("tv","bill_id")
rel$bill_id<-str_squish(rel$bill_id)
dissim_all<-rbind(dissim_all,merge(rel,d1418,by=c("bill_id")))

write.csv(dissim_all, paste0(gdir,"dissim_all_hun_fin.csv"), fileEncoding = "utf-8", row.names = FALSE)

