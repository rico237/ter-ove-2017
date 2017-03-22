#Install and use packages 
library(ggplot2)

# Mettre la racin du projet
setwd("/Users/Rico/GitHubs_Repo/ter-ove-2017/")

#donne <- read.csv(file="data.csv",head=TRUE,sep="\t")
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

q6_5 <- sample(1:13, size=40, replace= TRUE)
q4_3 <- sample(1:3, size=40, replace= TRUE)
general <- data.frame(q4_3, q6_5)
print(general)
#general$q4_3=ifelse(general$q4_3==1, "oui", "non") 
#head(general)
t <- aggregate(x=general, by=list(general$q4_3), FUN =mean)
print(t)

moyenne<- function(tableau, sub){
  mo<-(nrow(sub)*100)/nrow(tableau)
  return(mo)
}
insertion<-subset(general, q4_3 == 1)
moy<-moyenne(general, insertion)
print(moy)

#moy2 <- aggregate(x=general, by= list(general$q4_3), mean) 
#print(moy2)


tableauFinal <- cbind("taux d'insertion 18 mois", Universite=moy)
print(tableauFinal)
