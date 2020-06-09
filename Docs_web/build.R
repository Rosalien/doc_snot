# Suppression des fichiers HTML
for (f in  list.files(pattern = "html$"))
  unlink(f)

# Vérification des dépendances
source("verification_installation_dependances.R")

# Recréer tous les chapitres
#for (f in  list.files(pattern = "Rmd$")) {
  #set.seed(100);
  #rmarkdown::render(f, encoding = "UTF-8")
  #rmarkdown::render(f, output_format = "all",encoding = "UTF-8")
#}


# Génénation d'un pdf pour chaque html
#for (f in  list.files(pattern = "Rmd$")) {
 # set.seed(100);
  #system(paste("prince ",f," --javascript",sep=""))
#}

rmarkdown::render_site()