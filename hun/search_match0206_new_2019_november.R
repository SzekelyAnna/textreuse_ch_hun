cat("\f")
rm(list=ls())
options(scipen = 999)

library(stringr)
library(stringi)
library(tidytext)
library(tidyr)
library(dplyr)
library(data.table)
library(readtext)
library(readxl)
library(plyr)
library(xlsx)
library(pdftools) 
library(SnowballC)
library(corpus)
library(readr)
Sys.setlocale("LC_ALL","Hungarian")

gdir<-"C:/_swisshun/hun/"

source(paste0(gdir,"cleansing1.R"))


dir<-paste0(gdir,"passed_clean/ki0206/")
# cleaned passed read in
txt2 <- readtext(paste0(dir, "*"), encoding = "UTF-8")
#txt2$h <- nchar(txt2$text) 
txt2$text <- str_sub(txt2$text,2,nchar(txt2$text)-1)
#str_sub(txt2$text,nchar(txt2$text),nchar(txt2$text))
#unique(txt2$doc_id)


dir<-paste0(gdir,"drafts_raw/2002-2006_new/")
txt <- readtext(paste0(dir, "*"),encoding = "UTF-8")
txt$text<-tolower(txt$text)
txt<-data.table(txt)
# temp save
txt4<-txt
# load back
txt<-txt4


#2002-2006
dir<-paste0(gdir,"relate.xlsx")
rel <- data.table(read_excel(dir, sheet = "0206"))
txt$doc_id<-str_replace(txt$doc_id,"(\\.html|\\.txt|\\.pdf)","")
txt[,h:=nchar(doc_id)]
txt[h==5,doc_id:=paste("T","0",stri_sub(doc_id, 2, 5), sep="")]

# lowering length by trimming indokol's
#txt$text      <-str_replace(txt$text,"(?<=szövege a következ@ | )(indokolás|i n d o k o l).{200,2000}(számú határozatával meger@sítette |ennek megfelel@en rendelkezik)","")
#txt$text      <-str_replace(txt$text,"i n d o k o l á s.{500,3000}(?=melléklet)","")

# new combo
#str_extract_all(txt[doc_id=="T01218",2],"indokolás :\n|általános indokolás :\n|\nn d o k|i n d o k l á s|i n d o k o l á s| ndokolás\n|\nindoklás\n|\nindokolás\n|\ni n d o k o l á s\n|\nrészletes indokolás\n|\náltalános indokolás\n|\náltalános indoklás\n|\ná l t a l á n o s|\ni n d o k l á s\n|\ni n d o k o l á s\n")
cut<-str_locate_all(txt$text,"általános {1,2}indokolás|i n d o k l á s|i n d o k o l á s| ndokolás\n|\nindoklás\n|\nindokolás\n|\ni n d o k o l á s\n|\nrészletes indokolás\n|\náltalános indokolás\n|\náltalános indoklás\n|\ni n d o k l á s\n")
df<-data.table(t(as.matrix(sapply(cut, function(x) x[c(1,2,3,4,5,6,7,8)]))))
df<-apply(df, 1, function(x) min(x,na.rm = T))
is.na(df) <- sapply(df, is.infinite)
df<-data.table(df)
df<-cbind(df,h=nchar(txt$text))
df[is.na(df)==T,df:=h+1]
txt$text<-str_sub(txt$text,1, df$df-1)
fw1<-NULL
fw1<-data.table(df[,h])
fw1<-cbind(fw1,df[,df-1])
colnames(fw1)<-c("orig_h","altind")
fw1$doc_id<-txt$doc_id


txt$text<-gsub("ű","#",txt$text)
txt$text<-gsub("ő","@",txt$text)
#source("F:/_sync/_Capstone/cleansing1.R")
# § remains!!!!
txt$text<-cleansing_passed(txt$text,0)
txt$text<-str_replace_all(txt$text,"[0-9]{1,10}+(?! év|\\)| társ| | cím| alcím | mft| m ft| tv| törv| korm| milliárd| millió| ezer| forint| százalék|(| )%| bek| rend| f@| ft| ben| nap| év| hónap|§| §| (jan|feb|márc|ápr|máj|jú|aug|szep|okt|nov|dec)|[a-z@#úéáóüö]|)","") #|(| )\\-
txt$text<-str_squish(txt$text)

# temp save
txt3<-txt
# load back
txt<-txt3


#merge
colnames(rel)<-c("doc_id","draft")
rel$draft<-str_squish(rel$draft)
txt2$doc_id<-str_replace(txt2$doc_id,"\\.html|\\.txt|\\.pdf","")

# merge
txt5<-merge(rel,txt2,by=("doc_id") ,all.x = T)
colnames(txt5)<-c("tv","doc_id","text1")

txt6<-merge(txt,txt5,by=("doc_id") ,all.x = T)
tvdraft<-txt6[,c(1,4)]
# merge ends here



fw<-nchar(txt6$text)
fw<-data.table(cbind(fw,txt6$doc_id))
colnames(fw)<-c("start_h","doc_id")
duplicated(fw)
duplicated(fw1)
fw<-merge(fw,fw1,by=("doc_id") ,all.x = T)
duplicated(fw$doc_id)



cut<-str_locate(txt6$text1,"a kihirdetés napja 19.{5,15} [0-9]")
sum(is.na(cut)==F)
cut<-str_locate(txt6$text1,"megjelent a magyar közlöny.{3,40} számában")
sum(is.na(cut)==F)
cut<-str_locate(txt6$text1,"[0-9 ]helyesbítette magyar közlöny [0-9]{4,6}")
sum(is.na(cut)==F)
cut<-str_locate(txt6$text1,"[0-9] helyesbítve magyar közlöny [0-9]{4,6}")
sum(is.na(cut)==F)
# end




# beginning
cut<-NULL
cut<-stri_locate_all_fixed(as.character(txt6$text),str_sub(as.character(txt6$text1),1,40))

# new combo
df<-data.table(t(as.matrix(sapply(cut, function(x) x[c(1,2,3,4,5,6,7,8)]))))
df<-apply(df, 1, function(x) max(x[x<900],na.rm = T)-39)
is.na(df) <- sapply(df, is.infinite)
df<-data.table(df)

df<-cbind(df,str_locate(as.character(txt6$text),"(az országgyűlés|§| fejezet| paragrafus| cikk| általános rendelkez| első rész)")[,2]+2)

df<-cbind(df,str_locate(as.character(txt6$text),"(?<= törvény ).{5,350}r(ó|@|ö)l (?!(és|valamint|illetve|szóló))")[,2]+1)
colnames(df)<-c("df","V2","V3")

df[V3<V2 & is.na(df)==T, df:=V3]
df[is.na(df)==T,df:=ifelse(is.na(V2)==T | V2>1700,1,V2)]

txt6$text<-str_sub(as.character(txt6$text),df$df,nchar(txt6$text))
fw<-cbind(fw,nchar(txt6$text))



#cut<-str_locate(as.character(txt6$text),"melléklet")
#df<-data.table(cut[,1])
cut<-str_locate(as.character(txt6$text),"számú melléklet a.{3,20}törv|melléklet a.{3,20}törv| 1/1 sz melléklet | számú függelék ") # számú melléklet (!=szöveg)|
df2<-data.table(cut[,1])
df2[,h:=nchar(txt6$text)]
df2[is.na(V1)==T,V1:=h+1]
#fw<-cbind(fw,df2)
txt6$text<-str_sub(as.character(txt6$text),1,df2$V1-1)
fw<-cbind(fw,nchar(txt6$text))
fw<-cbind(fw,nchar(txt6$text1))
#colnames(fw)<-c("doc_id","start_h","orig_h","altind","begin","end","mell","passed")
colnames(fw)<-c("doc_id","start_h","orig_h","altind","begin","mell","passed")
fw<-data.table(fw)
fw[,arany1:=(mell-passed)/passed*100]
fw[,arany2:=(begin-passed)/passed*100]
#fw[,arany2:=(end-passed)/passed*100]
sum(fw[is.na(arany1)==F,arany1>30 & arany2>30]) # valszeg rossz passed
sum(fw[is.na(arany2)==F,arany2<(-30) & arany2<(-30)]) # valszeg rossz draft
fw<-merge(fw,tvdraft, by=("doc_id") ,all.x = T)

# cheking international treaties!!!!
fw[arany1<(-60),]
# above 90

#Jaccard
#source("F:/_sync/_Capstone/ngram2.R")

#str_extract_all(txt6$text,"[^ ]§")
txt6$text<-str_replace_all(txt6$text,"(?<=[^ ])§"," §")
txt6$text<-gsub("#","ü",txt6$text)
txt6$text<-gsub("@","ö",txt6$text)
txt6$text1<-gsub("#","ü",txt6$text1)
txt6$text1<-gsub("@","ö",txt6$text1)
txt7<-txt6[,c(1:2)]
txt8<-txt6[,c(1,5)]
txt7$text <-str_replace_all(txt7$text,"[^[:alnum:]§]"," ")
txt8$text1<-str_replace_all(txt8$text1,"[^[:alnum:]§]"," ")
stri_sub(txt7$text, 1, 0) <- " "
stri_sub(txt7$text,  nchar(txt7$text)+1, nchar(txt7$text)) <- " "
stri_sub(txt8$text1, 1, 0) <- " "
stri_sub(txt8$text1,  nchar(txt8$text1)+1, nchar(txt8$text1)) <- " "

txt7$text<-str_squish(txt7$text)
txt8$text1<-str_squish(txt8$text1)



sfile<-paste0(gdir,"0206txt7.Rda")
saveRDS(txt7, file=sfile)
sfile<-paste0(gdir,"0206txt8.Rda")
saveRDS(txt8, file=sfile)
