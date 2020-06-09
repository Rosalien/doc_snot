#' @title importparametres
#'
#' @description Fonction pour charger les libraries, et les variables du projet
#'
#' @param dsn Paramètres de connexion vers la base de données (ex. "PG:dbname='sol_elevage' host='localhost' port='5432' user='jb'")
#' @param repmaster Chemin vers la racine de la copie du dépôt Git en local (XX/XX/)
#'
#' @author Jean-Baptiste Paroissien
#' @keywords 
#' @seealso 
#' @export
#' @examples
#' ## Ne fonctionne pas 
# importparametres(repmaster="/media/sf_GIS_ED/Dev/Scripts/master/",dsn="PG:dbname='sol_elevage' host='localhost' port='5432' user='jb'")


importparametres <- function(dsn,
						repmaster)
{

ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# Chargement des librairies
listpaquets <- c("knitr","pander","kableExtra","DT")#"RPostgreSQL","RODBC"
ipak(listpaquets)
#new.packages <- listpaquets[!(listpaquets %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

knitr::opts_chunk$set(echo = TRUE)

# knit_hooks,fig : Fonctions pour générer la référence des figures et des tableaux (selon https://rstudio-pubs-static.s3.amazonaws.com/98310_b44bc54001af49d98a7b891d204652e2.html#five_to_one)
# A function for generating captions and cross-references
fig <- local({
    i <- 0
    list(
        cap=function(refName, text, center=FALSE, col="black", inline=FALSE) {
            i <<- i + 1
            ref[[refName]] <<- i
            css_ctr <- ""
            if (center) css_ctr <- "text-align:center; display:inline-block; width:100%;"
            cap_txt <- paste0("<span style=\"color:", col, "; ", css_ctr, "\">Figure ", i, ": ", text , "</span>")
            anchor <- paste0("<a name=\"", refName, "\"></a>")
            if (inline) {
                paste0(anchor, cap_txt)    
            } else {
                list(anchor=anchor, cap_txt=cap_txt)
            }
        },
        
        ref=function(refName, link=FALSE, checkRef=TRUE) {
            
            ## This function puts in a cross reference to a caption. You refer to the
            ## caption with the refName that was passed to fig$cap() (not the code chunk name).
            ## The cross reference can be hyperlinked.
            
            if (checkRef && !refName %in% names(ref)) stop(paste0("fig$ref() error: ", refName, " not found"))
            if (link) {
                paste0("<A HREF=\"#", refName, "\">", ref[[refName]], "</A>")
            } else {
                paste0(ref[[refName]])
            }
        },
        
        ref_all=function(){
            ## For debugging
            ref
        })
})
assign("fig",fig,.GlobalEnv)

library(knitr)
knit_hooks$set(plot = function(x, options) {
    sty <- ""
    if (options$fig.align == 'default') {
        sty <- ""
    } else {
        sty <- paste0(" style=\"text-align:", options$fig.align, ";\"")
    }
    
    if (is.list(options$fig.cap)) {
        ## options$fig.cap is a list returned by the function fig$cap()
        str_caption <- options$fig.cap$cap_txt
        str_anchr <- options$fig.cap$anchor
    } else {
        ## options$fig.cap is a character object (hard coded, no anchor)
        str_caption <- options$fig.cap
        str_anchr <- ""
    }
    
    paste('<figure', sty, '>', str_anchr, '<img src="',
        opts_knit$get('base.url'), paste(x, collapse = '.'),
        '"><figcaption>', str_caption, '</figcaption></figure>',
        sep = '')
    
})

# Définition des principaux répertoires de travail #####################################

##
#assign("repmetadonnees",paste(repmaster,"Documentation/Metadonnees/",sep=""),.GlobalEnv)
#assign("repfonctions",paste(repmaster,"Fonctions/",sep=""),.GlobalEnv)
assign("reptables",paste(repmaster,"Tables/",sep=""),.GlobalEnv)
assign("repfigures",paste(repmaster,"Docs_web/Fig/",sep=""),.GlobalEnv)
#########################################
#https://sourcesup.renater.fr/plugins/scmgit/cgi-bin/gitweb.cgi?p=si-snot.git;a=blob_plain;f=Documentation/Tables/donnees_references_snot/jeu.csv;hb=HEAD
#https://sourcesup.renater.fr/plugins/scmgit/cgi-bin/gitweb.cgi?p=si-snot.git;a=blob_plain;f=Documentation/Tables/donnees_references_snot/information_complementaire_par_stdt_variable.csv;hb=HEAD
# 
forge_url <- "https://forge-osuc.cnrs-orleans.fr/projects/sie-sno-tourbiere/repository/revisions/master/entry/"
assign("forge_url",forge_url,.GlobalEnv) #url du dépôt git dans la forge
assign("mo_url",paste(forge_url,"Documentation/Modes_operatoires/",sep=""),.GlobalEnv) #url des modes opératoires


# Mise en place de la connexion ODBC
# assign("loc",odbcConnect("solelevage",case="postgresql", believeNRows=FALSE),.GlobalEnv)

# Paramètres de connexion de la BDD
# assign("dsn",dsn,.GlobalEnv)

# Connexion avec RPostgreSQL
# assign("m",dbDriver("PostgreSQL"),.GlobalEnv)
# assign("con",dbConnect(m, dbname="sol_elevage"),.GlobalEnv)


# Fonction très pratique pour remplacer une suite de charactères par une autre
gsub2 <- function(pattern, replacement, x, ...) {
  for(i in 1:length(pattern))
    x <- gsub(pattern[i], replacement[i], x, ...)
  x
}
assign("gsub2",gsub2,.GlobalEnv)

# Fonction pour effectuer une requête sql avant d'importer un postgis (selon https://geospatial.commons.gc.cuny.edu/2013/12/31/subsetting-in-readogr/)
readOgrSql = function (dsn, sql, ...) {
   # check dsn starts "PG:" and strip
  if (str_sub(dsn, 1, 3) != "PG:") {
    stop("readOgrSql only works with PostgreSQL DSNs")
  }
  dsnParamList = str_trim(str_split(dsn, ":")[[1]][2])

  # Build dbConnect expression, quote DSN parameter values 
  # if not already quoted
  if (str_count(dsnParamList, "=") 
      == str_count(dsnParamList, "='[[:alnum:]]+'")) {
    strExpression = str_c(
      "dbConnect(dbDriver('PostgreSQL'), ", 
      str_replace_all(dsnParamList, " ", ", "), 
      ")"
      )
  }
  else {
    dsnArgs = word(str_split(dsnParamList, " ")[[1]], 1, sep="=")
    dsnVals = sapply(
      word(str_split(dsnParamList, " ")[[1]], 2, sep="="), 
      function(x) str_c("'", str_replace_all(x, "'", ""), "'")
      )
    strExpression = str_c(
      "dbConnect(dbDriver('PostgreSQL'), ", 
      str_c(dsnArgs, "=", dsnVals, collapse=", "), 
      ")"
      )
  }

  # Connect, create spatial view, read spatial view, drop spatial view
  conn = eval(parse(text=strExpression))
  print(dbSendQuery(conn, "DROP VIEW if exists vw_tmp_read_ogr;"))
  strCreateView = paste("CREATE VIEW vw_tmp_read_ogr AS", sql)
  dbSendQuery(conn, strCreateView)
  temp = readOGR(dsn = dsn, layer = "vw_tmp_read_ogr", ...)
  print(dbDisconnect(conn))
  return(temp)
}
assign("readOgrSql",readOgrSql,.GlobalEnv)

#return(list(grid_arrange_shared_legend,gsub2,fig=fig))
}#Fin fonction

