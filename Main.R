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



moyenne<- function(tableau, sub){
  mo<-(nrow(sub)*100)/nrow(tableau)
  return(mo)
}
q4_3 <- sample(1:3, size=200, replace= TRUE)
q6_5 <- sample(1:13, size=200, replace= TRUE)
q7_1 <- sample(1:3, size=200, replace= TRUE)
q8_1 <- sample(1:13, size=200, replace= TRUE)
universite <- data.frame(q4_3, q6_5, q7_1, q8_1)
insertion<-subset(universite, q4_3 == 1)
insertion2<-subset(universite, q7_1 == 1)
stable<-subset(insertion, q6_5 == 1 | q6_5 == 2 | q6_5 == 3)
stable2<-subset(insertion2, q8_1 == 1 | q8_1 == 2 | q8_1 == 3)

# taux d'insertion 18
#print(moyenne(universite, insertion))
# taux d'insertion 30
#print(moyenne(universite, insertion2))
# emploi stable 18
#print(moyenne(insertion, stable))
# emploi stable 30
#print(moyenne(insertion2, stable2))

emplois<-c(moyenne(universite, insertion), moyenne(insertion, stable) )
emploiStable<-c(moyenne(universite, insertion2) ,moyenne(insertion2, stable2))
df<-data.frame(emplois, emploiStable, row.names = c("Taux d'insertion","Part des emplois stable"))
colnames(df) <-c("18 mois", "30 mois")
print(df)
