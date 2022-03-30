#library intéressante 
library(readxl)
library(dplyr)
library(tidyr)
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
library(rvest)
library(tidyverse)
library(stringr)
library(qdapRegex)
library(maps)
library(tidygeocoder)
library(readr)

# on ouvre les différents excel avec les liens 
# base de données 'Wiki_stade'
Wiki_stade <- read_excel("Lien wiki stade.xlsx") # base de données contenant les liens wikipedia des stades européens (base de données faites automatiquement)
# base de données 'Wiki_stade_plus'
Wiki_stade_plus <- read_excel("C:/Users/William/OneDrive/Bureau/Wiki stade plus.xlsx") # base de données contenant les liens wikipedia des stades manquants

# on s'occupera de la base de données 'Wiki_stade_plus' après 

# création de fonciton permettant de lire la première table des liens

read_txt = function(i){
  read_html( paste(Wiki_stade$Liens[i])) %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table/tbody') %>%  #("//*[@id="mw-content-text"]/div[1]/table[2]/tbody") faire un if
    html_text()
}

# si le lien n'est pas bon alors on sort "Erreur dans le lien"
read_txt=possibly(read_txt,otherwise="Erreur dans le lien")

# on utilise la fonction read sur la base de données pour sortir le text de la table
for(i in seq(from=1, to=nrow(Wiki_stade))){
  Wiki_stade$table[i] =  read_txt(i)
}

# on extrait les liens de la base de données 'Wiki_stade' dans 'extract'
extract <- Wiki_stade %>%
  select(id, stade, Liens, table)

# on supprime les lignes qui ont une "Erreur dans le lien"
extract <- extract[ grep("Erreur dans le lien", extract$table, invert = TRUE) , ]
length(extract$id)

# le wikipedia de chaque stade à une structure html qui se ressemble mais il y a plusieurs types (il y en a surtout 3)

#Fonction permettant de lire le côté gauche de chaque lien selon la ligne "i"

read_txt = function(i){
  read_html( paste(extract$Liens[i])) %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table/tbody') %>%  
    html_text()
}

#Fonction 2 permettant de lire le tableau 2
read_txt2 = function(i){
  read_html( paste(extract$Liens[i])) %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]/tbody') %>%
    html_text()             
}

#Fonction 3 permettant de lire le wiki s'il n'y a pas de tableau
read_txt3 = function (i){
  read_html( paste(extract$Liens[i])) %>%
    html_node(xpath ='//*[@id="mw-content-text"]/div[1]') %>%
    html_text()
}

# addaptation la fonction "read_txt" pour afficher "Erreur dans le lien" si cela ne foncitonne pas
read_txt=possibly(read_txt,otherwise="Erreur dans le lien")
read_txt2=possibly(read_txt2,otherwise="Erreur dans le lien")
read_txt3=possibly(read_txt3,otherwise="Erreur dans le lien")

for(i in seq(from=1, to=length(extract$id))){
  if (is.na(extract$table[i])){
    extract$table[i] = "Erreur"
  }
}

# création d'un 'for' pour utiliser la bonne fonction celon le type de html

start <- Sys.time()
for(i in seq(from=1, to=length(extract$id))){
  if (startsWith(extract$table[i], "This article")){
    extract$table[i] = read_txt2(i)
  } else if (extract$table[i] == "Erreur" ) {
    extract$table[i] = read_txt3(i)
  } else {
    extract$table[i] = read_txt(i)
  }
}
end <- Sys.time()
end - start    # extraction du temps que prend la fonction à lire tout les liens

# maintenant nous avons tout le texte nous intéressant sur la page wikipedia, l'objectif maintenant est d'extraire les différentes données qui nous intéresse

#######################################
# extraction de la date

# création d'un fonction pour extraire le contenu du texte entre "Opened" et "Renovated"
fopened = function(i){
  gsub('^.*Opened\\s*|\\s*Renovated.*$', '', extract$table[i])
}
for(i in seq(from=1, to =length(extract$id))){
  extract$opened[i] = fopened(i)
}

# maintenant notre objectif est d'extraire une année dans ce texte

open <- regmatches(extract$opened, gregexpr("\\d{4}", extract$opened)) # garder seulement les nombres à 4 chiffres
extract$opened <- open

extractopened <- function(i){       # fonction qui récupère la première valeure
  extract$opened[[i]][1]
}
for (i in seq(from=1 , to=length(extract$id))){      
  extract$opened[i] = extractopened(i)
}

# nous avons ainsi réussi à extraire l'année d'ouverture du stade

#######################################
# extraction de la date de rénovation s'il y en a une

# fonction pour extraire le contenu du texte entre "Renovated" et "Tenants"
frenov = function(i){
  gsub('^.*Renovated\\s*|\\s*Tenants.*$', '', extract$table[i])
}
for(i in seq(from=1, to =length(extract$id))){
  extract$renovated[i] = frenov(i)
}

# on extrait maintenant les nombres à 4 chiffres
extract$renovated = substr(extract$renovated, 1, 20)
ren <- regmatches(extract$renovated, gregexpr("\\d{4}", extract$renovated))
extract$renovated <- ren



extract$renov <- extract$renovated
for (i in seq(from=1, to = length(extract$id))){
  extract$renov[i] <- toString(extract$renovated[i], witdh = FALSE)
}

library(stringr)

extract$renov <- str_replace_all(extract$renov, "[c(]", "") # on enlève les différents pattern que l'on veut enlever
extract$renov <- str_replace_all(extract$renov, "[)]", "")
extract$renov <- str_replace_all(extract$renov, '["]', "")

for (i in seq (from=1, to=length(extract$id))){
  if(extract$renov[i] == "harater0"){
    extract$renov[i] = "No renovated"
  }
}

extract$renovated = extract$renov  # on ramène tout dans la colonne 'renovated'


#######################################
# extraction de la capacité

# création d'une fonction qui extrait le contenu du texte après 'Capacity'
fcap = function(i){
  gsub('.*Capacity', '', extract$table[i])
}
for(i in seq(from=1, to =length(extract$id))){
  extract$capacity[i] = fcap(i)
}

extract$capacity = substr(extract$capacity , 1, 20)  # on récupère les 20 premiers characters

extract$capacity <- sub("\\,", "", extract$capacity) # on enlève certains patterns
extract$capacity <- sub("\\.", "", extract$capacity)

numextract <- function(string){ 
  str_extract(extract$capacity, "\\-*\\d+\\.*\\d*")
} 
extract$capacity<-numextract(extract$capacity)

extract$capacity = as.numeric(extract$capacity)

#######################################
# fonction qui extrait l'occupant du stade (l'équipe qui occupe le stade)
ftenant = function(i){
  gsub('.*Tenants', '', extract$table[i])
}
for(i in seq(from=1, to =length(extract$id))){
  extract$tenant[i] = ftenant(i)
}

#######################################
# extraction des coordonées

# création d'une fonction qui récupère le texte du html du wikipedia qui contient les coordonnées
read_coord = function(i){
  read_html( paste(extract$Liens[i])) %>%
    html_node(xpath = '//*[@id="coordinates"]') %>%
    html_text()             
}
read_coord=possibly(read_coord,otherwise="Erreur dans le lien")
for(i in seq(from=1, to=length(extract$id))){
  extract$cordinate[i] = read_coord(i)  
}

# on sépare les coordonnées que l'on vient d'extraire pour avoir les latitudes et les longitudes en colonne
fcoor = function(i){
  gsub('^.*/\\s*|\\s*/.*$', '', read_coord(i))
}
for(i in seq(from=1, to =length(extract$id))){
  extract$cordinate[i] = fcoor(i)
}
extract <- extract %>% 
  separate(cordinate, into = c("latitude", "longitude"), sep = ";" )

# on supprime les stade sans coordonnées
extract <- extract %>% 
  drop_na(longitude)

library(ggmap)
res <- lapply(with(extract, paste(latitude, longitude, sep = ",")), geocode, output = "more")  # récupère l'adresse exact des coordonnées
city = sapply(res, "[[", "address")  # "[[" signifie extraire le premier index (de la partie adress)
for (i in seq(from=1, to=length(extract$id))){
  extract$adress[i] = city[[i]][1]
}

#Extraction (de la ville et) du pays
library(rlang)
library(dplyr)
library(stringr)
extract$city = extract$adress
extract$city <- str_to_title(extract$adress) #On met la première lettre en majuscule
extract$city <- str_replace_all(extract$city, ",", "")  #On supprime les ","
extract$city <- str_replace_all(extract$city, "/", "")  #On supprime les "/"
extract$city <- gsub('[0-9]+', "", extract$city)  #On supprime les chiffres
for (i in seq(from=1, to=length(extract$id))){
  extract$country[i] = word(extract$city[i], -1)
}
for (i in seq(from= 1, to =length(extract$id))){  
  if(extract$country[i] == "" ) {
    extract$country[i] = word(extract$city[i] , -2)
  } else {
    extract$country[i] = word(extract$city[i], -1)
  }
}





#####################################################################################################

#Créer un nouveaux tableaux avec les données que l'on veut (base de données Wiki_stade)

Stadium = extract %>%
  select(stade, opened, renovated, capacity, latitude, longitude, country, tenant)
View(Stadium)


#Enregister le tableau
write.csv2(Stadium,file= 'Stadium.csv')


data("world.cities")
check <- Stadium$city %in% world.cities$name
Stadium$city[check == TRUE]

Stadium <- Stadium %>%
  select(-c(opened, op))


write.csv2(Stadium,file= 'Stadium.csv')
attach(Stadium)


#####################################################################################################


#Créer un nouveaux tableaux avec les données que l'on veut (base de données Wiki_stade_plus)

#Il faut extraire les stades des Liens
extract$stade <- gsub('.*wiki/', '', extract$Liens)
extract$stade <- str_replace_all(extract$stade, '[_]', " ")

Stadium_plus = extract %>%
  select(stade, opened, renovated, capacity, latitude, longitude, country, tenant)
View(Stadium_plus)







##################################################################################

Stadium <- rbind(Stadium, Stadium_plus)
View(Stadium)


Stadium<- apply(Stadium,2,as.character)


write.csv2(Stadium,file= 'Stadium.csv')


