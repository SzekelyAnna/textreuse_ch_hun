#load libraries
library(dplyr)
library(stringr)
library(stringi)
library(pdftools)
library(reshape2)
library(tm)

clean_bill_ch_extract_input <- function(txt){

txt_full <- pdf_text(txt)

start <- pages_input$page_start[i]
end <- pages_input$page_end[i]

txt_full <- txt_full[start:end]
txt_info <- pdf_info(txt)

page_total <- (end - start) + 1


##### clean documents
txt_clean <- txt_full
txt_clean <- gsub("\r","",txt_clean)

# remove legislative act heading
  txt_clean[1] <- gsub(".*arr\u00EAte:\n.{1,20}I\n","",txt_clean[1])
  txt_clean[1] <- gsub(".*arr\u00EAte :\n.{1,20}I\n","",txt_clean[1])
  
  txt_clean[1] <- gsub(".*arr\u00EAte:\nI\n","",txt_clean[1])
  txt_clean[1] <- gsub(".*arr\u00EAte:\n","",txt_clean[1])
  txt_clean[1] <- gsub(".*arr\u00EAte :\nI\n","",txt_clean[1])
  txt_clean[1] <- gsub(".*arr\u00EAte :\n","",txt_clean[1])
  
  #txt_clean[1] <- stringi::stri_replace_first_regex(txt_clean[1],"((\n.*){0,20})(.*)(modifi\u00E9|modifi\u00E9e|modi-\nfi\u00E9|modi-\nfi\u00E9e|modifi\u00E9s|modifi\u00E9es|modi-\nfi\u00E9es|modi-\nfi\u00E9s)\\s(comme suit|comme\nsuit)","")
  
  txt_clean[1] <- gsub(".*(modifi\u00E9|modifi\u00E9e) comme suit\\:","",txt_clean[1])
  txt_clean[1] <- gsub(".*(modifi\u00E9|modifi\u00E9e) comme suit:","",txt_clean[1])
  
  txt_clean[1] <- gsub(".*(modifi\u00E9|modifi\u00E9e) comme\nsuit\\:","",txt_clean[1])
  txt_clean[1] <- gsub(".*(modifi\u00E9|modifi\u00E9e) comme\nsuit:","",txt_clean[1])
  
  txt_clean[1] <- gsub(".*(modi-\nfi\u00E9|modi-\nfi\u00E9e) comme suit\\:","",txt_clean[1])
  txt_clean[1] <- gsub(".*(modi-\nfi\u00E9|modi-\nfi\u00E9e) comme suit:","",txt_clean[1])


#remove footnotes
    footnotes <- str_extract_all(txt_full, "([a-z]|[A-Z]){1,3}\\d{1,2}")
    footnotes <- melt(footnotes, value.name = "footnote_number")
    
    #remove footnotes in text
    footnotes$text <- footnotes$footnote_number
    footnotes$text <- str_replace(footnotes$text, "\\d{1,2}$", "")
    footnotes$footnote_number <- as.character(footnotes$footnote_number)
    
    if(nrow(footnotes) >= 1) {
    for (i in 1:length(footnotes$footnote_number)){
          txt_clean <- str_replace(txt_clean, footnotes$footnote_number[i], footnotes$text[i])
        }
    }  
    
    #remove footnotes at bottom of page
    
    if(nrow(footnotes) >= 1) {
    
                    footnotes <- rename(footnotes, page_number = L1)
                    footnotes$footnote_number <- str_extract(footnotes$footnote_number, "\\d{1,3}")
                    
                      for (i in length(footnotes$footnote_number)) {
                        txt_page <- footnotes$page_number[i]              
                        footnote_regex <- paste("\\\n", footnotes$footnote_number[i], "\\s(FF|RS|RO).*\n", sep = "")
                        txt_clean[txt_page] <- gsub(footnote_regex,"\n",txt_clean[txt_page])
                      }
                    }
    
    txt_clean <- gsub("\\\n\\d{1,3}\\s*(FF)\\s\\d{4}\\s\\d{3}\n.*","\n",txt_clean)
    txt_clean <- gsub("\\\n\\d{1,3}\\s*(FF)\\s\\d{4}\\s\\d{4}\n.*","\n",txt_clean)
    txt_clean <- gsub("\\\n\\d{1,3}\\s*(FF)\\s\\d{4}\\s(I|II|III|IV|V)\\s\\d{4}\n.*","\n",txt_clean)
    txt_clean <- gsub("\\\n\\d{1,3}\\s*(FF)\\s\\d{4}\\s(I|II|III|IV|V)\\s\\d{3}\n.*","\n",txt_clean)
    txt_clean <- gsub("\\s*(FF)\\s\\d{4}\\s(I|II|III|IV|V)\\s\\d{4}\n.*","\n",txt_clean)
    txt_clean <- gsub("\\s*(FF)\\s\\d{4}\\s(I|II|III|IV|V)\\s\\d{3}\n.*","\n",txt_clean)

#remove footer first page and page numbers
txt_clean[1] <- gsub("\n\\d{1,4}\\-\\d{1,4}\\s+\\d{1,4}\n","\\\n",txt_clean[1])


#remove running title if pages > 1

if(page_total >=2) {
  
  running_title <- str_extract(txt_clean[2], ".*\n")
  running_title <- str_replace_all(running_title, "\\(", "\\\\(")
  running_title <- str_replace_all(running_title, "\\)", "\\\\)")
  running_title <- str_replace_all(running_title, "\\,", "\\\\,")
  running_title <- paste("^", running_title, sep = "")
  txt_clean <- gsub(running_title, "", txt_clean)
  
  running_title_2 <- str_extract(txt_clean[2:page_total], ".*\n")
  running_title_2 <- paste("^", running_title_2, sep = "")
  
          #remove running title with two lines (only from 3 page)
          if(page_total >=3 & length(unique(running_title_2)) == 1 ){
            txt_clean <- gsub(running_title_2[1], "\n", txt_clean)
          }

} 



#for some texts, need to do it manually:

if(page_total >=3) {
  
  running_title <- str_extract(txt_clean[3], ".*\n")
  running_title <- str_replace_all(running_title, "\\(", "\\\\(")
  running_title <- str_replace_all(running_title, "\\)", "\\\\)")
  running_title <- str_replace_all(running_title, "\\,", "\\\\,")
  running_title <- paste("^", running_title, sep = "")
  txt_clean <- gsub(running_title, "", txt_clean)
}

if(page_total >=4) {
  
  running_title <- str_extract(txt_clean[4], ".*\n")
  running_title <- str_replace_all(running_title, "\\(", "\\\\(")
  running_title <- str_replace_all(running_title, "\\)", "\\\\)")
  running_title <- str_replace_all(running_title, "\\,", "\\\\,")
  running_title <- paste("^", running_title, sep = "")
  txt_clean <- gsub(running_title, "", txt_clean)
}

if(page_total >=5) {
  
  running_title <- str_extract(txt_clean[5], ".*\n")
  running_title <- str_replace_all(running_title, "\\(", "\\\\(")
  running_title <- str_replace_all(running_title, "\\)", "\\\\)")
  running_title <- str_replace_all(running_title, "\\,", "\\\\,")
  running_title <- paste("^", running_title, sep = "")
  txt_clean <- gsub(running_title, "", txt_clean)
}

if(page_total >=6) {
  
  running_title <- str_extract(txt_clean[6], ".*\n")
  running_title <- str_replace_all(running_title, "\\(", "\\\\(")
  running_title <- str_replace_all(running_title, "\\)", "\\\\)")
  running_title <- str_replace_all(running_title, "\\,", "\\\\,")
  running_title <- paste("^", running_title, sep = "")
  txt_clean <- gsub(running_title, "", txt_clean)
}

if(page_total >=7) {
  
  running_title <- str_extract(txt_clean[7], ".*\n")
  running_title <- str_replace_all(running_title, "\\(", "\\\\(")
  running_title <- str_replace_all(running_title, "\\)", "\\\\)")
  running_title <- str_replace_all(running_title, "\\,", "\\\\,")
  running_title <- paste("^", running_title, sep = "")
  txt_clean <- gsub(running_title, "", txt_clean)
}

if(page_total >=8) {
  
  running_title <- str_extract(txt_clean[8], ".*\n")
  running_title <- str_replace_all(running_title, "\\(", "\\\\(")
  running_title <- str_replace_all(running_title, "\\)", "\\\\)")
  running_title <- str_replace_all(running_title, "\\,", "\\\\,")
  running_title <- paste("^", running_title, sep = "")
  txt_clean <- gsub(running_title, "", txt_clean)
}

if(page_total >=9) {
  
  running_title <- str_extract(txt_clean[9], ".*\n")
  running_title <- str_replace_all(running_title, "\\(", "\\\\(")
  running_title <- str_replace_all(running_title, "\\)", "\\\\)")
  running_title <- str_replace_all(running_title, "\\,", "\\\\,")
  running_title <- paste("^", running_title, sep = "")
  txt_clean <- gsub(running_title, "", txt_clean)
}

if(page_total >=10) {
  
  running_title <- str_extract(txt_clean[10], ".*\n")
  running_title <- str_replace_all(running_title, "\\(", "\\\\(")
  running_title <- str_replace_all(running_title, "\\)", "\\\\)")
  running_title <- str_replace_all(running_title, "\\,", "\\\\,")
  running_title <- paste("^", running_title, sep = "")
  txt_clean <- gsub(running_title, "", txt_clean)
}

#remove references to FF and RS
txt_clean <- gsub("\n\\d{1,3}\\s+FF\\s\\d{4}\\s{1,4}","",txt_clean)
txt_clean <- gsub("\\s+FF\\s\\d{4}\\s{1,4}","",txt_clean)
txt_clean <- gsub("\n\\d{1,3}\\s+RS\\s\\d{1,3}.*?\n","", txt_clean)
txt_clean <- gsub("\\s+RS\\s\\d{1,3}.*?\n","", txt_clean)


#remove hyphenation
txt_clean <- gsub("\\-\\\n","HYPHEN",txt_clean)
txt_clean <- gsub("HYPHEN\\s+","",txt_clean)
txt_clean <- gsub("HYPHEN", "", txt_clean)

#remove enumeration
txt_clean <- gsub("\\\n\\s+[a-z]\\."," ",txt_clean)
txt_clean <- gsub("\\\n\\s+\\d{1,2}\\.\\s"," ",txt_clean)


#identify and cut annex
#annex_page <- str_detect(txt_clean, "\\\n\\s+Annexe\\\n")
annex_page <- str_detect(txt_clean, "\\s+Annexe\\\n")
annex_page <- match(TRUE,annex_page)

if(!is.na(annex_page)){
  annex_page_list <- annex_page:page_total
  txt_clean <- txt_clean[-annex_page_list]
}


#remove article heading
txt_clean <- gsub("\\\nArt.\\s\\d{1,3}.*?\n", "\n", txt_clean)
txt_clean <- gsub("\\\n\\s+Art.\\s\\d{1,3}.*?\n", "\n", txt_clean)

#remove chapter heading
txt_clean <- gsub("\\\n(Chapitre|Ch.)\\s(I|II|III|IV|V|VI|VII|VIII|XIX|X)\\s.*?\n", "\n", txt_clean)
txt_clean <- gsub("\\\n(I|II|III|IV|V|VI|VII|VIII|XIX|X)\n", "\n", txt_clean)


#remove alinea
txt_clean <- gsub("\\\n\\d{1,2}\\s", "\n", txt_clean)

#DISCUSS

#remove page number from pdf
txt_clean <- gsub("\\d{1,4}\\\n$", "\n", txt_clean)

#OUTPUT
txt_clean <- gsub("\\\n(Conseil des Etats|Conseil national)\\,\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}\\s+(Conseil des Etats|Conseil national)\\,\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*?$", "\\\n", txt_clean)
txt_clean <- gsub("\\\n (Conseil des Etats|Conseil national)\\,\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}\\s+(Conseil des Etats|Conseil national)\\,\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*?$", "\\\n", txt_clean)

txt_clean <- gsub("\\\n(Conseil des Etats|Conseil national)\\,\\s(1er)\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}\\s+(Conseil des Etats|Conseil national)\\,\\s(1er)\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*?$", "\\\n", txt_clean)
txt_clean <- gsub("\\\n(Conseil des Etats|Conseil national)\\,\\s(1er)\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}\\s+(Conseil des Etats|Conseil national)\\,\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*?$", "\\\n", txt_clean)
txt_clean <- gsub("\\\n(Conseil des Etats|Conseil national)\\,\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}\\s+(Conseil des Etats|Conseil national)\\,\\s(1er)\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*?$", "\\\n", txt_clean)

txt_clean <- gsub("\\\n (Conseil des Etats|Conseil national)\\,\\s(1er)\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}\\s+(Conseil des Etats|Conseil national)\\,\\s(1er)\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*?$", "\\\n", txt_clean)
txt_clean <- gsub("\\\n (Conseil des Etats|Conseil national)\\,\\s(1er)\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}\\s+(Conseil des Etats|Conseil national)\\,\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*?$", "\\\n", txt_clean)
txt_clean <- gsub("\\\n (Conseil des Etats|Conseil national)\\,\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}\\s+(Conseil des Etats|Conseil national)\\,\\s(1er)\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*?$", "\\\n", txt_clean)

txt_clean <- gsub("\\\n\\s*(Conseil des Etats|Conseil national)\\,\\s(1er)\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}\\s+(Conseil des Etats|Conseil national)\\,\\s(1er)\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*?$", "\\\n", txt_clean)
txt_clean <- gsub("\\\n\\s*(Conseil des Etats|Conseil national)\\,\\s(1er)\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}\\s+(Conseil des Etats|Conseil national)\\,\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*?$", "\\\n", txt_clean)
txt_clean <- gsub("\\\n\\s*(Conseil des Etats|Conseil national)\\,\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}\\s+(Conseil des Etats|Conseil national)\\,\\s(1er)\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*?$", "\\\n", txt_clean)

txt_clean <- gsub("\\\n(Conseil des Etats|Conseil national)\\,\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*", "\\\n", txt_clean)
txt_clean <- gsub("\\(Conseil des Etats|Conseil national)\\,\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*", "\\\n", txt_clean)
txt_clean <- gsub("\\\n\\s*(Conseil des Etats|Conseil national)\\,\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*", "\\\n", txt_clean)


txt_clean <- gsub("\\\n(Conseil des Etats|Conseil national)\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*", "\\\n", txt_clean)
txt_clean <- gsub("\\(Conseil des Etats|Conseil national)\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*", "\\\n", txt_clean)
txt_clean <- gsub("\\\n\\s*(Conseil des Etats|Conseil national)\\s\\d{1,2}\\s(janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|aou00FBt|septembre|octobre|novembre|d\u00E9cembre)\\s\\d{4}.*", "\\\n", txt_clean)


#remove minority propositions
txt_clean <- gsub("\\\n(Proposition de la minorit\u00E9 I|Proposition de la minorit\u00E9 II)\\\n\\([^\\)]+\\)","",txt_clean)
txt_clean <- gsub("\\\n(Proposition de la minorit\u00E9 I|Proposition de la minorit\u00E9 II)\\s\\([^\\)]+\\)","",txt_clean)
txt_clean <- gsub("\\\n(Minorit\u00E9 I|Minorit\u00E9 II)\\\n\\([^\\)]+\\)","",txt_clean)
txt_clean <- gsub("\\\n(Minorit\u00E9 I|Minorit\u00E9 II)\\s\\([^\\)]+\\)","",txt_clean)
txt_clean <- gsub("\\\n(Minorit\u00E9|Proposition de la minorit\u00E9)\\\n\\([^\\)]+\\)","",txt_clean)
txt_clean <- gsub("\\\n(Minorit\u00E9|Proposition de la minorit\u00E9)\\s\\([^\\)]+\\)","",txt_clean)

txt_clean <- gsub("\\\n\\s*(Proposition de la minorit\u00E9 I|Proposition de la minorit\u00E9 II)\\\n\\([^\\)]+\\)","",txt_clean)
txt_clean <- gsub("\\\n\\s*(Proposition de la minorit\u00E9 I|Proposition de la minorit\u00E9 II)\\s\\([^\\)]+\\)","",txt_clean)
txt_clean <- gsub("\\\n\\s*(Minorit\u00E9 I|Minorit\u00E9 II)\\\n\\([^\\)]+\\)","",txt_clean)
txt_clean <- gsub("\\\n\\s*(Minorit\u00E9 I|Minorit\u00E9 II)\\s\\([^\\)]+\\)","",txt_clean)
txt_clean <- gsub("\\\n\\s*(Minorit\u00E9|Proposition de la minorit\u00E9)\\\n\\([^\\)]+\\)","",txt_clean)
txt_clean <- gsub("\\\n\\s*(Minorit\u00E9|Proposition de la minorit\u00E9)\\s\\([^\\)]+\\)","",txt_clean)

txt_clean <- gsub("\\\n\\s*(Minorit\u00E9:|Proposition de la minorit\u00E9:)\\\n\\([^\\)]+\\)","",txt_clean)
txt_clean <- gsub("\\\n\\s*(Minorit\u00E9:|Proposition de la minorit\u00E9:)\\s\\([^\\)]+\\)","",txt_clean)
txt_clean <- gsub("\\\n(Minorit\u00E9:|Proposition de la minorit\u00E9:)\\\n\\([^\\)]+\\)","",txt_clean)
txt_clean <- gsub("\\\n(Minorit\u00E9:|Proposition de la minorit\u00E9:)\\s\\([^\\)]+\\)","",txt_clean)

txt_clean <- gsub("\\\n(Minorit\u00E9:|Proposition de la minorit\u00E9:)\\s*\\([^\\)]+\\)","",txt_clean)

#2017-10-12
#Remove AF ending
txt_clean <- gsub("\\AF$", "", txt_clean)

#end function
result <- txt_clean
return(result)
}

