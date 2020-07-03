# Paramètre de connexion de la documentation en ligne. Cette partie doit être modifiée en fonction de l'emplacement du dépôt du projet.
#repmaster <- "/home/jb/Documents/SI_SNOT/sie-sno-tourbiere/Doc_snot/" # A modifier
source("F_importparametres.R") # Ne pas modifier, chargement de la fonction
importparametres(repmaster=getwd()) # Ne pas modifier, lancement de la fonction pour générer l'ensemble des paramètres

# Mettre en place une fonction pour les URL
library(htmltools)
linkAlertCSS <- function(text,url){
	HTML(paste0("<a href=",url," style=\"color: white;text-decoration:underline;font-weight:bold\">",text,"</a>"))
}
