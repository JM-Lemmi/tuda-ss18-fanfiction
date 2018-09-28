# Scraper angepasst an Fanfcition.net
# Julian Lemmerich
# 17.07.2018 20:14 / Frankfurt Airport

library(rvest)

# 27.09.2018 / Home

# overviewscraper

titel_scrapen <- function(x){  
  html_text(html_nodes(read_html(x), css = ".stitle"))
}
Autor_scrapen <- function(x){  
  html_text(html_nodes(read_html(x), css = ".zpointer a:nth-child(3)"))
}
Meta_scrapen <- function(x){  
  html_text(html_nodes(read_html(x), css = ".xgray"))
}
link_scrapen <- function(x){
  paste("http://www.fanfiction.net",html_attr(html_nodes(read_html(x), css = ".stitle"), "href"), sep="" )
}

# Der typische fanfiction.net Overview-Seitenlink: https://www.fanfiction.net/book/Harry-Potter/?&srt=4&r=10&p=2
# wert p ist die seitennummer, kann also ziemlich einfach verwendet werden.

Seitenzahl <- 20 #hier gibt man an, von wie vielen overview seiten die Daten gesrapt werden sollen.

Titelliste <- vector()
Autorliste <- vector()
Metaliste <- vector()
Linkliste <- vector()

for (i in 1:Seitenzahl) {
  Titelliste <- c(Titelliste,titel_scrapen(paste0("http://www.fanfiction.net/book/Harry-Potter/?&srt=4&r=10&p=", i)))
  Autorliste <- c(Autorliste,Autor_scrapen(paste0("http://www.fanfiction.net/book/Harry-Potter/?&srt=4&r=10&p=", i)))
  Metaliste <- c(Metaliste,Meta_scrapen(paste0("http://www.fanfiction.net/book/Harry-Potter/?&srt=4&r=10&p=", i)))
  Linkliste  <- c(Linkliste, link_scrapen(paste0("http://www.fanfiction.net/book/Harry-Potter/?&srt=4&r=10&p=", i)))
  Sys.sleep(5)
}

# Metaliste muss noch aufgetrennt werden nach dem scrapen, da auf fanficiton.net alles in einem css zusammengefasst ist.

Chapter_extract <- function(x){
  x <- gsub(".*Chapters: ", "", x)
  gsub(" - Words.*", "", x)
}
Fav_extract <- function(x){
  x <- gsub(".*Favs: ", "", x)
  gsub(" - Follows.*", "", x)
}
Wordcount_extract <- function(x){
  x <- gsub(".*Words: ", "", x)
  gsub(" - Reviews.*", "", x)
}

Chapterliste <- vector()
Favliste <- vector()
Wordliste <- vector()

for (i in 1:500){
  Chapterliste <- c(Chapterliste,Chapter_extract(Metaliste[i]))
  Favliste <- c(Favliste,Fav_extract(Metaliste[i]))
  Wordliste <- c(Wordliste,Wordcount_extract(Metaliste[i]))
}

Tabelle <- data.frame(Titelliste, Autorliste, Metaliste, Chapterliste, Favliste, Wordliste, Linkliste)

# textscraper

text_extract <- function(x){  
  html_text(html_nodes(read_html(x), css = "#storytextp"))
}


#dieser loop kann von einer ganzen linkliste alle chapter herungerladen
#umbedint das richtige WD setzen vor diesem loop! sonst landen die dateien sonstwo
#setwd("C:/Users/julia/User Data/Uni/semester 2/Fanfiction/Hausarbeit")

j <- 0

for (u in Linkliste){
  j <- j + 1
  x <- strsplit(u, "/1/")
  u.1 <- unlist(lapply(x, "[[", 1))
  u.2 <- unlist(lapply(x, "[[", 2))
  
  v <- paste0(u.1, "/",  1:Chapter_extract(Metaliste[i]), "/", u.2) # v ist die Liste der Chapterlinks für eine Geschichte
  
  for (i in 1:length(v)){
    texte <- text_extract(v[i])
    write.table(texte,
                paste0(j, "_", u.2, ".txt"),
                append = TRUE,
                row.names = F,
                col.names = F,
                fileEncoding = "UTF-8")
    Sys.sleep(3)
  }
}


# nach 31 timeout, warum auch immer. Also nochmal

Linkliste2 <- Linkliste[34:50]

j <- 33
for (u in Linkliste2){
  j <- j + 1
  x <- strsplit(u, "/1/")
  u.1 <- unlist(lapply(x, "[[", 1))
  u.2 <- unlist(lapply(x, "[[", 2))
  
  v <- paste0(u.1, "/",  1:Chapter_extract(Metaliste[i]), "/", u.2) # v ist die Liste der Chapterlinks für eine Geschichte
  
  for (i in 1:length(v)){
    texte <- text_extract(v[i])
    write.table(texte,
                paste0(j, "_", u.2, ".txt"),
                append = TRUE,
                row.names = F,
                col.names = F,
                fileEncoding = "UTF-8")
    Sys.sleep(3)
  }
}


# 28.09.2018

# Character List

wikicharI_scrapen <- function(x){  
  html_text(html_nodes(read_html(x), css = ".mw-headline"))
}
wikicharII_scrapen <- function(x){  
  html_text(html_nodes(read_html(x), css = ".toctext"))
}

wikicharI <- wikicharI_scrapen("https://de.wikipedia.org/wiki/Figuren_der_Harry-Potter-Romane")
wikicharII <- wikicharII_scrapen("https://de.wikipedia.org/wiki/Figuren_der_Harry-Potter-Romane")
wikicharI %in% wikicharII
# es gibt bestimmt eine bessere Methode, aber ich mach das einfach Händisch.



# bessere Methode:

wikicharIII_scrapen <- function(x){html_text(html_nodes(read_html(x), css = ".mw-parser-output > ul li"))}
wikicharIII <- wikicharIII_scrapen("https://en.wikipedia.org/wiki/List_of_Harry_Potter_characters")

wiki.list_cleanup <- function(x){gsub(" – .*", "", x)} #vorischt, besonderer bindestrich!
Charliste <- vector()
for (i in 1:length(wikicharIII)){Charliste <- c(Charliste,wiki.list_cleanup(wikicharIII[i]))}

Charliste <- Charliste[1:197] #damit die unnötigen Wikipedia Teile rauskommen

#Trennung aller Namen in Vorname und Nachname
Charliste.trenn <- vector()
Charliste.trenn <- strsplit(Charliste, " ")
# dies erstellt allerdings einen Dataframe und ist daher nicht geeignet

for (i in 1:length(Charliste)){
  x <- strsplit(Charliste[i], " ")
  
  Charliste.trenn <- c(Charliste.trenn, x[[1]])
}

# Word counter

#first load all .txt files into a variable

corpus.orig_1 <- read_file("C:\\Users\\julia\\OneDrive\\User Data\\Uni\\Semester 2\\Fanfiction\\Hausarbeit\\corpus\\original\\Harry Potter and the Chamber of Secrets.txt")

length(grep("Harry", corpus.orig_1))








