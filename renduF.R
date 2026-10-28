# Préparation des données
data = read.table("Desktop/ProjetMSRO/dataMSRO2.csv", sep=";", dec=".", header=TRUE, as.is=TRUE)
data$Date <- as.POSIXct(data$Date, format="%Y-%m-%d %H:%M") #convertit la colonne Date (qui était du texte chr) en date + heure
data$Jour <- as.Date(data$Date) #créé une nouvelle colonne Jour, enlève l’heure et garde seulement la date

# Quelques fonctions utiles

"H2J" = function(data, stat="max", arrondi.entier = T)
{
  nbcol <- ncol(data)
  if(is.null(nbcol))
    v <- statloc(matrix(data, nrow = 24.), stat=stat)
  else {
    v <- NULL
    for(ic in 1.:nbcol)
      v <- cbind(v, statloc(matrix(data[, ic], nrow = 24.), stat=stat))
  }
  if(arrondi.entier) {
    nbcol <- ncol(v)
    if(is.null(nbcol)) {
      ind.demi <- (v %% 1.) == 0.5
      v[ind.demi & !is.na(v)] <- 1. + (v[ind.demi & !is.na(v)] %/% 1.)
      v[!ind.demi & !is.na(v)] <- round(v[!ind.demi & !is.na(v)])
    }
    else {
      for(ic in 1.:nbcol) {
        ind.demi <- (v[, ic] %% 1.) == 0.5
        v[ind.demi & !is.na(v[, ic]), ic] <- 1. + (v[ind.demi & !is.na(v[, ic]), ic] %/% 1.)
        v[!ind.demi & !is.na(v[, ic]), ic] <- round(v[!ind.demi & !is.na(v[, ic]), ic])
      }
    }
  }
  
}

"statloc" = function(x, p = 0.75, stat = "mean", ...)
{
  if(is.null(dim(x)))
    v <- ifelse(sum(is.na(x)) > ((1. - p) * length(x)), NA, apply(matrix(x, ncol = 1.), 2., stat, na.rm = T, ...))
  else {
    ind <- apply(is.na(x), 2., sum)
    v <- apply(x, 2., stat, na.rm = T, ...)
    v[ind > ((1. - p) * dim(x)[1.])] <- NA
  }
  v
}

#######################################################################################################
# I - ETUDE GLOBALE
#######################################################################################################

# Création de la série chronologique
moyennes <- H2J(data$HRI_T_P10, stat="mean")
tail(moyennes) #affichage des dernières valeurs
head (moyennes) #affichage des premières valeurs

#jours <- seq(from=min(as.Date(data$Date)), #on recréé les jours à partir des valeurs du vecteur moyennes
#             by="day",
#            length.out=length(moyennes))

# Affichage de la série chronologique
plot(moyennes,type="l", main="Série temporelle des concentrations moyennes (2018-2023)", xlab="Jour",
ylab="Concentration horaire de poussières en µg/m3")
X=data[, 2]

#Histogramme
hist(moyennes,breaks=20)


#######################################################################################################
# II - ETUDE DE L'ACTIVITÉ HUMAINE
#######################################################################################################

#Comparaison par jour 

# Algorithme de création des vecteurs jours.

Lundi <- c(moyennes[1]) # On initialise le premier jour 
Mardi <- c(moyennes[2])
Mercredi <- c(moyennes[3])
Jeudi <- c(moyennes[4])
Vendredi <- c(moyennes[5])
Samedi <- c(moyennes[6])
Dimanche <- c(moyennes[7])

# Lundi
i <- 8

while(i<2191){   # on parcourt toutes les données du vecteur "moyennes"
  Lundi <- append(Lundi, moyennes[i])
  i <- i+7      # saut de 7 jours pour ajouter le lundi suivant  
}

# Mardi
i <- 9            # on repart du 2ème mardi

while(i<2191){
  Mardi <- append(Mardi, moyennes[i])
  i <- i+7
}

# Mercredi
i <- 10

while(i<2191){
  Mercredi <- append(Mercredi, moyennes[i])
  i <- i+7
}

# Jeudi
i <- 11

while(i<2191){
  Jeudi <- append(Jeudi, moyennes[i])
  i <- i+7
}

# Vendredi
i<-12

while(i<2191){
  Vendredi <- append(Vendredi, moyennes[i])
  i <- i+7
}

# Samedi
i<-13

while(i<2191){
  Samedi <- append(Samedi, moyennes[i])
  i <- i+7
}

# Dimanche
i<-14

while(i<2191){
  Dimanche <- append(Dimanche, moyennes[i])
  i <- i+7
}

# Représentation en boîtes à moustaches de chaque jour de la semaine 

boxplot(Lundi , Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche, names = c("L", "M", "M", "J", "V", "S",
        "D"),xlab = "Jour de la semaine", ylab="Concentration horaire moyenne en µg/m3"
        ,main="Distribution des concentrations journalières selon le jour de la semaine (2018–2023)")
        abline(h=17.72, col="red")
        
summary(moyennes)
      

# Comparaison par mois

# Algorithme de création des vecteurs mois

Janvier   <- c()
Fevrier   <- c()
Mars      <- c()
Avril     <- c()
Mai       <- c()
Juin      <- c()
Juillet   <- c()
Aout      <- c()
Septembre <- c()
Octobre   <- c()
Novembre  <- c()
Decembre  <- c()

# Nombre de jours par mois (année non bissextile)
jours_mois <- c(31,28,31,30,31,30,31,31,30,31,30,31)

# Indice de départ
i <- 1

while(i <= length(moyennes)){
  
  # Janvier
  Janvier <- append(Janvier, moyennes[i:(i+30)])
  i <- i + 31
  
  # Février
  Fevrier <- append(Fevrier, moyennes[i:(i+27)])
  i <- i + 28
  
  # Mars
  Mars <- append(Mars, moyennes[i:(i+30)])
  i <- i + 31
  
  # Avril
  Avril <- append(Avril, moyennes[i:(i+29)])
  i <- i + 30
  
  # Mai
  Mai <- append(Mai, moyennes[i:(i+30)])
  i <- i + 31
  
  # Juin
  Juin <- append(Juin, moyennes[i:(i+29)])
  i <- i + 30
  
  # Juillet
  Juillet <- append(Juillet, moyennes[i:(i+30)])
  i <- i + 31
  
  # Août
  Aout <- append(Aout, moyennes[i:(i+30)])
  i <- i + 31
  
  # Septembre
  Septembre <- append(Septembre, moyennes[i:(i+29)])
  i <- i + 30
  
  # Octobre
  Octobre <- append(Octobre, moyennes[i:(i+30)])
  i <- i + 31
  
  # Novembre
  Novembre <- append(Novembre, moyennes[i:(i+29)])
  i <- i + 30
  
  # Décembre
  Decembre <- append(Decembre, moyennes[i:(i+30)])
  i <- i + 31
}

# Représentation en boîtes à moustaches

boxplot(Janvier, Fevrier, Mars, Avril, Mai, Juin,
        Juillet, Aout, Septembre, Octobre, Novembre, Decembre,
        names = c("Jan","Fev","Mar","Avr","Mai","Jun",
                  "Jul","Aou","Sep","Oct","Nov", "Dec"),
        xlab = "Mois de l'année",
        ylab = "Concentration journalière moyenne en µg/m3",
        main = "Distribution des concentrations journalières selon le mois (2018–2023)")

abline(h = 17.72, col = "red")
