#2019-05
#FUNCTION FOR PRE-PROCESSING TEXT IN FRENCH
#load libraries

library(tm)
library(stringr)

txt_prep_ch_fr <- function(txt){
  
  #lower case
  txt_clean <- tolower(txt)
  
  #remove apostrophes and other things
  apostrophes <- c("d'","d’","l'","l’","qu'","qu’","n'","n’","m'","m’","t'","t’","c'","c’","j'",
                  "j’","s’","s'","lorsqu'un","lorsqu’un","lorsqu'une","lorsqu’une",
                   "lorsqu'ils","lorsqu’ils","lorsqu'elles","lorsqu’elles","lorsqu'il","lorsqu’il","lorsqu'elle",
                  "lorsqu’elle","(nouveau)","(ancien)","(nouvelle teneur)","s'il","s’il","s'ils","s’ils")
  txt_clean <- removeWords(txt_clean, apostrophes)
  
  #remove dots between 2 letters (not recognized as "punctuation" by next command")
  txt_clean <- gsub("[\\.](?!\\d*$)"," ",txt_clean,perl = T)
  
  #remove punctuation
  txt_clean <- removePunctuation(txt_clean, preserve_intra_word_dashes = F)
  
  #remove whitespace
  txt_clean <- stripWhitespace(txt_clean)
  
  #remove numbers
  txt_clean <- removeNumbers(txt_clean)
  
  #remove whitespace
  txt_clean <- stripWhitespace(txt_clean)
  
  #trim the string
  txt_clean <- str_trim(txt_clean)
 
  #remove common words
  common_words <- c("al", "let", "art", "lart", "bis", "ter", "quater", "quinquies", "sexies", "septies", "octies", "nonies", "decies",
                    "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x","xi","xiii","xiv","xv","xvi","xvii",
                    "xviii","xix","xx","xxi","xxii","xxiii","xxiv","xxv","xxvi","xxvii","xxviii","xxix","xxx",
                    "dun","dune","nest","où","ni","dont","si","sil","sils","duquel","desquels","desquelles","laquelle",
                    "lesquelles","lequel","lesquels","que","ci","ça","car","parce","chaque","chacun",
                    "chacune","ladite","ledit","ellemême","ellesmêmes","euxmêmes","luimême","celuici","celleci",
                    "ceuxci","cellesci","vu","article","articles","alinéa","alinéas","celle","celles","lors","chap",
                    "rs","ro","ff","chapitre","dès","ch","oui","non","cst","ciaprès","afin","que","afin","tel","telle","telles",
                    "tels","non publié dans","non publie dans","ne concerne que le texte allemand",
                    "ne concerne que le texte italien","n","<",">","titre précédent larticle",
                    "titre précédent article","article premier","ne pas entrer en matière","préambule",
                    "classer linitiative","classer initiative","projet de loi","appendice","annexe","annexes","appendices",
                    "janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août", "septembre", "octobre", "novembre", "décembre")
  
  txt_clean <- removeWords(txt_clean, common_words)
  
  #remove stop words
  txt_clean <- removeWords(txt_clean, stopwords("french"))
  
  #stemming
  #txt_clean <- stemDocument(txt_clean, language = "french")
  
  
  #end function
  result <- txt_clean
  return(result)
}
