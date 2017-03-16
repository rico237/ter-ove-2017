#Install and use packages 
library(ggplot2)

# Mettre la racin du projet
setwd("/Users/Rico/GitHubs_Repo/ter-ove-2017/")

donne <- read.csv(file="data.csv",head=TRUE,sep="\t")
# Generer des données de gens qui ont un boulot vs d'autre non
# ensembleDeDonnee <- sample(0:1, size=100, replace= TRUE)

# Afficher des plots
#barplot(table(donne$isDiplome))
#barplot(table(donne$sexe))

# Travailler directement sur un tableau
#x<- subset(donne, isDiplome == 0 & sexe == "M")
#print(x)

# Moyenne des diplomés
#insertionDip <- mean(donne$isDiplome)