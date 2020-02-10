
cleansing_passed <- function(txt_c, k) {
  
  # őű transformation
  txt_c<-gsub("ű","#",txt_c)
  txt_c<-gsub("ő","@",txt_c)
  
  if (k==1) {
    txt_c<-str_replace_all(txt_c,"(?<=[a-züöú#@áéúó]) (?=[#@] )","")
    txt_c<-str_replace_all(txt_c,"(?<=[#@]) (?=[a-z\\.,])","")
    txt_c<-str_replace_all(txt_c,"(?<=[#@])  (?=[a-z\\.,])"," ")
    txt_c<-str_replace_all(txt_c,"(?<=[a-züöú#@áéúó])  (?=[#@])","")
    txt_c<-str_replace_all(txt_c,"(?<=[a-züöú#@áéúó]) (?=[#@])","")
    txt_c<-str_squish(txt_c)
    txt_c<-str_replace_all(txt_c,"kell@(?=[a-z])","kell@ ")
    txt_c<-str_replace_all(txt_c,"szet#(?=[a-z])","szet# ")
    txt_c<-str_replace_all(txt_c,"kör#(?=[a-z])","kör# ")
    txt_c<-str_replace_all(txt_c,"képez@(?=[a-z])","képez@ ")
    txt_c<-str_replace_all(txt_c,"érint@(?=[a-z])","érint@ ")
    txt_c<-str_replace_all(txt_c,"t@(?=[a-z])","t@ ")
    txt_c<-str_replace_all(txt_c,"alapvet@(?=[a-z])","alapvet@ ")
    txt_c<-str_replace_all(txt_c,"történ@(?=[a-z])","történ@ ")
    txt_c<-str_replace_all(txt_c,"el@z@(?=[a-z])","el@z@ ")
    txt_c<-str_replace_all(txt_c,"(?<=[a-z])@riz"," @riz")
    txt_c<-str_replace_all(txt_c,"el@ ter","el@ter")
    txt_c<-str_replace_all(txt_c,"pez@(?=[a-z])","pez@ ")
    #txt_c<-str_replace_all(txt_c,"megfelel@(?=[a-z])","megfelel@ ")
  }
  
  # cleansing
  
  #place a space after ,.;)
  txt_c<-str_replace_all(txt_c,"(?<=[\\)\\.,:;])(?=[a-zöüóúéá@#0-9])"," ") # new line 20190407
  txt_c<-str_replace_all(txt_c,"(?<=[a-zöüóúéá@#0-9])(?=\\()"," ") # new line 20190407
  # idiot paragraphs OCR
  txt_c<-str_replace_all(txt_c,"\\(l(?=[a-z]\\))","\\(1")
  txt_c<-str_replace_all(txt_c,"\\(l\\)","\\(1\\)")
  txt_c<-str_replace_all(txt_c,"•§","\\.§")
  txt_c<-str_replace_all(txt_c,"l[\\.\\-]§","1\\.§")
  txt_c<-str_replace_all(txt_c,"l\\. §","1\\.§")
  txt_c<-str_replace_all(txt_c,"l§","1\\.§")
  txt_c<-str_replace_all(txt_c,"\\!\\.§","1\\.§")
  txt_c<-str_replace_all(txt_c,"(?<=[0-9]{1,3})(|\\-| \\-|\\- )§","\\.§")

  txt_c<-str_replace_all(txt_c,"\017","")
  txt_c<-str_replace_all(txt_c,"\025","§")
  txt_c<-str_replace_all(txt_c,"\n|\t", " ")
  txt_c<-str_replace_all(txt_c,"\r", "")
  
  txt_c<-str_replace_all(txt_c,"”", "")
  txt_c<-str_replace_all(txt_c,"“", "")
  txt_c<-str_replace_all(txt_c,"…", "")

  txt_c<-str_replace_all(txt_c," a z "," az ")
  txt_c<-str_replace_all(txt_c," dr "," ")
  
  # dátumok, ilyesmi # maradhat
  #txt_c<-str_replace_all(txt_c,"\\-(a|i|sal|hoz) ","")
  #txt_c<-str_replace_all(txt_c,"\\-(á|é)n ","")
  #txt_c<-str_replace_all(txt_c,"(\\-ig |\\- ig )","")
  #txt_c<-str_replace_all(txt_c,"(\\-t(ó|@)l |\\- t(ó|@)l )","")
  #txt_c<-str_replace_all(txt_c,"([0-9]{1,2}\\-i|[0-9]{1,2} \\-i)","")
  
  # romans out! # maradhat
  #txt_c<-str_replace_all(txt_c,"( |\\()[clxvi]{1,8}( \\.|\\.|\\))"," ")
  
  # a - előtt ha van space, akkor kiszedni!
  #txt_c<-str_replace_all(txt_c," (?=[\\-]) ","")
  # a - után ha van space, akkor kiszedni!
  #txt_c<-str_replace_all(txt_c,"(?<=[a-zéá#@öüóú][\\-]) ","")
  
  # pont és vessző eltávolítása

  # számokat, kivéve százalék, forint millió ezer miliárd, stb., de a paragrafusszámot eltávolítjuk
  txt_c<-str_replace_all(txt_c,"(?<=[0-9]{3,3})[ ,\\.](?=[0-9]{3,3})","") #pont beszúrva #2019 03. 29.
  #txt_c<-str_replace_all(txt_c,"[0-9]{1,10}+(?! évi|\\)| társ| mft| m ft| milliárd| millió| ezer| forint| százalék| %| f@| ft| nap| év| hónap)","")
  #txt_c<-str_squish(txt_c)
  txt_c<-str_replace_all(txt_c,"[\\.,] "," ")
  txt_c<-str_squish(txt_c)
  txt_c<-str_replace_all(txt_c,"[–\\–\\*\\-\\„\\¦¦\\|\\,\\\\_=\\*\\.\\[\\]„/\\/,:;~…\\*\\^'\\!\\?\\°\\$\\©\\`\\&\\!!\\<\\>\\«\\‘\\+\\•\\¾]","")
  
}
