
##L'objectif dans ce script est de créer une fonction pour ajouter des stades à la base de données
##Cependant il faudrait ajouter une recherche google pour ensuite récupérer le bon lien pour la recherche du stade

#deux foncitons sont mise, la première avec un problème sur la lecture des coordonnées et la second sans la lecture des coordonnées


##FONCTION FINAL POUR LIRE LE LIENS WIKIPEDIA
stade = "AKA Arena"
stade= "Mart Stadium"
x <- "AKA Arena"
y <- scraping(x)
y[1]  
read=y[1]





library(readxl)




Stadium <- read_excel("Stadium.xlsx")

stade = "Stade_Francis-Le_Blé"

# fonction scraping en utilisant une table 
# problème avec la recherche de l'adresse

scraping <- function(stade){
  library(rlang)
  library(dplyr)
  library(stringr)
  library(ggmap)
  library(stringr)
  library(readxl)
  library(rintrojs)
  library(shinyalert)
  library(shinythemes)
  library(shinycssloaders)
  library(shinyjs)
  library(shinyWidgets)
  library(shiny.i18n)
  library(esquisse)
  library(golem)
  library(suncalc)
  library(xml2)
  library(dplyr)
  library(rvest)
  library(readxl)
  library(tidyverse)
  library(stringr)
  library(qdapRegex)
  library(maps)
  library(tidyr)
  library(stringr)
  library(tidygeocoder)
  library(readr)
  
  Stadium <- read_excel("C:/Users/William/OneDrive/Bureau/Stadium.xlsx")
  
  extract = data.frame(
    stade= "stade",
    opened ="opened", 
    renovated="renovated",
    capacity="capacity",
    latitude="latitude",
    longitude="longitude", 
    country="country",
    tenant="tenant"
  )
  View(extract)
  
  lien = paste0("https://en.wikipedia.org/wiki/",stade)
  lien = str_replace_all(lien, " ", "_")
  
  read_txt = function(lien){
    read_html(lien) %>%
      html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table/tbody') %>%  
      html_text()
  }
  
  
  read_txt2 = function(lien){
    read_html(lien) %>%
      html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]/tbody') %>%
      html_text()             
  }
  
  
  read_txt3 = function (lien){
    read_html(lien) %>%
      html_node(xpath ='//*[@id="mw-content-text"]/div[1]') %>%
      html_text()
  }
  
  
  read_txt=possibly(read_txt,otherwise="Erreur dans le lien")
  read_txt2=possibly(read_txt2,otherwise="Erreur dans le lien")
  read_txt3=possibly(read_txt3,otherwise="Erreur dans le lien")
  
  read = read_txt(lien)
  
  if (startsWith(read, "This article")){
    read = read_txt2(lien)
  } else if (read == "Erreur" ) {
    read = read_txt3(lien)
  } else {
    read = read_txt(lien)
  }
  
  extract$opened <- gsub('^.*Opened\\s*|\\s*Renovated.*$', '', read)
  extract$opened <- regmatches(extract$opened, gregexpr("\\d{4}", extract$opened))
  extract$opened <- extract$opened[1]
  
  extract$tenants <- gsub('.*Tenants', '', read)
  
  extract$renovated <- gsub('^.*Renovated\\s*|\\s*Tenants.*$', '', read)
  extract$renovated = substr(extract$renovated, 1, 20)
  extract$renovated <- regmatches(extract$renovated, gregexpr("\\d{4}", extract$renovated))
  
  extract$renovated <- toString(extract$renovated, witdh = FALSE)
  
  extract$renovated <- str_replace_all(extract$renovated, "[c(]", "")
  extract$renovated <- str_replace_all(extract$renovated, "[)]", "")
  extract$renovated <- str_replace_all(extract$renovated, '["]', "")
  
  {
    if(extract$renovated == "harater0"){
      extract$renovated = "No renovated"
    }
  }
  
  
  extract$capacity <- gsub('.*Capacity', '', read)
  extract$capacity = substr(extract$capacity , 1, 20) 
  extract$capacity <- sub("\\,", "", extract$capacity)
  extract$capacity <- sub("\\.", "", extract$capacity)
  
  numextract <- function(string){ 
    str_extract(extract$capacity, "\\-*\\d+\\.*\\d*")
  } 
  extract$capacity<-numextract(extract$capacity)
  
  
  read_coord = function(lien){
    read_html( paste(lien)) %>%
      html_node(xpath = '//*[@id="coordinates"]') %>%
      html_text()             
  }
  
  read_coord=possibly(read_coord,otherwise="Erreur dans le lien")
  
  extract$coordinate = read_coord(lien) 
  if (extract$coordinate == "Erreur dans le lien"){
    extract$coordinate = "Unknow"
    extract$longitude = "Unknow"
    extract$latitude = "Unknow"
    extract$country = "Unknow"
  } else {  
    extract$coordinate <- gsub('^.*/\\s*|\\s*/.*$', '', extract$coordinate)
    
    extract$longitude <- sub(".*;", "", extract$coordinate)
    extract$latitude <- sub(";.*", "", extract$coordinate)  
    
    
    res <- lapply( paste(extract$latitude, extract$longitude, sep = ","), geocode, output = "more")
    
    city = sapply(res, "[[", "address")  # "[[" signifie extraire le premier index (de la partie adress)
    
    adress = city[1]
    
    city = adress
    city <- str_to_title(adress) #On met la première lettre en majuscule
    
    city <- str_replace_all(city, ",", "")  #On supprime les ","
    city <- str_replace_all(city, "/", "")  #On supprime les "/"
    city <- gsub('[0-9]+', "", city)  #On supprime les chiffres
    
    extract$country = word(city, -1)
    
    
    {  
      if(extract$country == "" ) {
        extract$country = word(city , -2)
      } else {
        extract$country = word(city, -1)
      }
    }
  }
  
  
  
}



































# scraping sans l'extraction des coordonnées car il y a un problème à régler



scraping <- function(stade){
  library(rlang)
  library(dplyr)
  library(stringr)
  library(ggmap)
  library(stringr)
  library(readxl)
  library(rintrojs)
  library(shinyalert)
  library(shinythemes)
  library(shinycssloaders)
  library(shinyjs)
  library(shinyWidgets)
  library(shiny.i18n)
  library(esquisse)
  library(golem)
  library(suncalc)
  library(xml2)
  library(dplyr)
  library(rvest)
  library(readxl)
  library(tidyverse)
  library(stringr)
  library(qdapRegex)
  library(maps)
  library(tidyr)
  library(stringr)
  library(tidygeocoder)
  library(readr)
  
  Stadium <- read_excel("C:/Users/William/OneDrive/Bureau/Stadium.xlsx")
  
  extract = data.frame(
    id = "id",
    stade= "stade",
    opened ="opened", 
    renovated="renovated",
    capacity="capacity",
    latitude="latitude",
    longitude="longitude", 
    country="country",
    tenant="tenant"
  )
  View(extract)
  
  lien = paste0("https://en.wikipedia.org/wiki/",stade)
  lien = str_replace_all(lien, " ", "_")
  
  read_txt = function(lien){
    read_html(lien) %>%
      html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table/tbody') %>%  
      html_text()
  }
  
  
  read_txt2 = function(lien){
    read_html(lien) %>%
      html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]/tbody') %>%
      html_text()             
  }
  
  
  read_txt3 = function (lien){
    read_html(lien) %>%
      html_node(xpath ='//*[@id="mw-content-text"]/div[1]') %>%
      html_text()
  }
  
  
  read_txt=possibly(read_txt,otherwise="Erreur dans le lien")
  read_txt2=possibly(read_txt2,otherwise="Erreur dans le lien")
  read_txt3=possibly(read_txt3,otherwise="Erreur dans le lien")
  
  read = read_txt(lien)
  
  if (startsWith(read, "This article")){
    read = read_txt2(lien)
  } else if (read == "Erreur" ) {
    read = read_txt3(lien)
  } else {
    read = read_txt(lien)
  }
  
  extract$opened <- gsub('^.*Opened\\s*|\\s*Renovated.*$', '', read)
  extract$opened <- regmatches(extract$opened, gregexpr("\\d{4}", extract$opened))
  extract$opened <- extract$opened[1]
  
  extract$tenant <- gsub('.*Tenants', '', read)
  
  extract$renovated <- gsub('^.*Renovated\\s*|\\s*Tenants.*$', '', read)
  extract$renovated = substr(extract$renovated, 1, 20)
  extract$renovated <- regmatches(extract$renovated, gregexpr("\\d{4}", extract$renovated))
  
  extract$renovated <- toString(extract$renovated, witdh = FALSE)
  
  extract$renovated <- str_replace_all(extract$renovated, "[c(]", "")
  extract$renovated <- str_replace_all(extract$renovated, "[)]", "")
  extract$renovated <- str_replace_all(extract$renovated, '["]', "")
  
  {
    if(extract$renovated == "harater0"){
      extract$renovated = "No renovated"
    }
  }
  
  
  extract$capacity <- gsub('.*Capacity', '', read)
  extract$capacity = substr(extract$capacity , 1, 20) 
  extract$capacity <- sub("\\,", "", extract$capacity)
  extract$capacity <- sub("\\.", "", extract$capacity)
  
  numextract <- function(string){ 
    str_extract(extract$capacity, "\\-*\\d+\\.*\\d*")
  } 
  extract$capacity<-numextract(extract$capacity)
  
  extract$id = nrow(Stadium) +1
  extract$stade = stade
  
  
  View(extract)
  print(extract)
  Stadium <- rbind(Stadium, extract)
  Stadium <- Stadium %>% distinct( stade, .keep_all = TRUE)
  View(Stadium)
}


# les deux bdd doivent être de même taille pour que le rbind foncitonnne
Stadium <- rbind(Stadium, extract)
Stadium <- Stadium %>% distinct( "la colonnne", .keep_all = TRUE)
library(readxl)
Stadium <- read_excel("C:/Users/William/OneDrive/Bureau/Stadium.xlsx")
View(Stadium)
lien = 'https://en.wikipedia.org/wiki/Est%C3%A1dio_Nacional_Man%C3%A9_Garrincha'
x = 'Estádio Nacional Mané Garrincha'
