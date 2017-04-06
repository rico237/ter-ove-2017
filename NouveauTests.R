
library(ggplot2)
#library(shiny)
#setwd("/Users/Rico/GitHubs_Repo/ter-ove-2017/")
#donne <- read.csv(file="data.csv",head=TRUE,sep="\t")

#on crée une fonction pourcentage afin de faciliter le calcule de pourcentage
pourcentage<- function(tableau, sub){
  #  pr<- paste(round((nrow(sub)*100)/nrow(tableau), digits = 2), "%")
  pr<- round((nrow(sub)*100)/nrow(tableau), digits = 2)
  return(pr)
}

salaireBrut<-function(salaireNet){
  salaire<-salaireNet*1.299
  return(salaire)
}

#Premièrement on génére des données aleatoires
n<-200 #taille des donees a generer

# 18 mois
q4_3 <- sample(1:3, size=n,prob=c(0.84, 0.05, 0.11), replace= TRUE) # Possède un emploi ou pas
q6_5 <- sample(1:13, size=n, replace= TRUE) # Type d'emploi
q6_6r<- sample(2:7, size=n, replace = TRUE) # Niveau d'emploi (ex: Cadre, Ingénieur, etc)
q6_7 <- sample(1:2, size = n,prob=c(0.82, 0.18), replace = TRUE) # Temps partiel ou pleins
q6_14a<-sample(1:99, size=n, replace= TRUE) # Départements
q6_9<-sample(13*10:20*10, size = n, replace = TRUE) #Salaire Net
q2_2<- sample(1:3, size = n, prob = c(0.1, 0.08, 0.82), replace = TRUE) #boursier
# 30 mois
q7_1 <- sample(1:3, size=n, prob=c(0.75, 0.20, 0.05),replace= TRUE) # Possède un emploi ou non
q8_1 <- sample(1:13, size=n, replace= TRUE) # Type d'emploi
q8_2r<- sample(2:7, size= n, replace = TRUE) # Niveau d'emploi (ex: Cadre, Ingénieur, etc)
q8_3<- sample(1:2, size = n,prob=c(0.71, 0.29), replace = TRUE) # Temps partiel ou pleins
dep30 <- sample(1:99, size = n, replace = TRUE) # Non présent dans le questionaire
q8_5<-sample(13*10:20*10, size = n, replace = TRUE) # Salaire Net
boursier30<-sample(1:3, size = n, prob = c(0.1, 0.08, 0.82), replace = TRUE) # Boursier Non present dans le questionnaire

c_sexe<-sample(c("h", "f"), n, prob=c(0.54, 0.46), replace=TRUE) #Definition de sexes

# Data frame avec l'ensemble des donnees
universite <- data.frame(q4_3 , q6_5, q6_6r, q6_7, q7_1, q8_1, q8_2r, q8_3, c_sexe, q6_14a, dep30 , q6_9, q8_5, q2_2, boursier30)
head(universite, n=2)

#On utilise subset afin de filtrer les données et travailler sur ceux qui ont un emploi

insertion<-subset(universite, q4_3 == 1)
insertion2<-subset(universite, q7_1 == 1)
stable<-subset(insertion, q6_5 == 1 | q6_5 == 2 | q6_5 == 3)
stable2<-subset(insertion2, q8_1 == 1 | q8_1 == 2 | q8_1 == 3)
niveauEmploi<-subset(insertion, q6_6r == 2 | q6_6r == 4)
niveauEmploi2<-subset(insertion2, q8_2r == 2 | q8_2r == 4)
tempsPlein<-subset(insertion, q6_7 == 1)
tempsPlein2<-subset(insertion2, q8_3 == 1)

salBrut<-with(tempsPlein, tempsPlein$q6_9) # On recupère uniquement la colonne des salaire net 
salBrut<-salaireBrut(salBrut) #on applique la fonciton de transformation en salaire brut
salBrut30<-salaireBrut(with(tempsPlein2, tempsPlein2$q8_5)) # code du haut en imbriqué 

#On formatte les données en data frame et on affiche les données

departement<-75 # On choisit un numéro de département

pourcentage18<-c(pourcentage(universite, insertion), pourcentage(insertion, stable), pourcentage(insertion, tempsPlein), pourcentage(insertion, niveauEmploi), pourcentage(insertion, subset(insertion, q6_6r == 2)), pourcentage(insertion, subset(insertion, insertion$q6_14a != departement)), median(tempsPlein$q6_9), median(salBrut))
pourcentage30<-c(pourcentage(universite, insertion2) ,pourcentage(insertion2, stable2),  pourcentage(insertion2, tempsPlein2), pourcentage(insertion2, niveauEmploi2), pourcentage(insertion, subset(insertion2, q8_2r == 2)), pourcentage(insertion2, subset(insertion2, insertion2$dep30 != departement)), median(tempsPlein2$q8_5), median(salBrut30))
df<-data.frame(pourcentage18, pourcentage30, row.names = c("Taux d'insertion","Part des emplois stable", "Part des emplois a temps plein", "Part des emplois de niveau cadre ou profession intermédiaire", "Part des emplois de niveau cadre", "Part des emplois en dehors de la région", "Salaire Net Median des emplois à Temps Plein", "Salaire brut annuel médian estimé"))
colnames(df) <-c("18 mois", "30 mois")
print(df)
# t(df) # Si on veut transposer la matrice (colonnes deviennent lignes, lignes deviennent colonnes)

#On essaye de plot certaines données

#Afin d'avoir un affichage plus clair pour le plot (oui ou non)
universite$insertion18 <-ifelse(universite$q4_3 ==1, "oui", "non")

plotInsertion<-ggplot(universite, aes(x=universite$insertion18, fill=universite$c_sexe)) 
plotInsertion<- plotInsertion + geom_bar(position=position_dodge()) 
plotInsertion<- plotInsertion + xlab("") 
plotInsertion<- plotInsertion + ylab("Total") 
plotInsertion<- plotInsertion + ylim(c(0, n))
plotInsertion<- plotInsertion + ggtitle("Possède un emploi à 18 mois. Population = 200 personnes.") 
plotInsertion<- plotInsertion + scale_fill_discrete(name = "")
plotInsertion<- plotInsertion + theme_minimal()
plotInsertion

universite$insertion30 <-ifelse(universite$q7_1 ==1, "oui", "non")
plotInsertion2<-ggplot(universite, aes(x=universite$insertion30, fill=universite$insertion30)) 
plotInsertion2<- plotInsertion2 + geom_bar(position=position_dodge()) 
plotInsertion2<- plotInsertion2 + xlab("") 
plotInsertion2<- plotInsertion2 + ylab("Total") 
plotInsertion2<- plotInsertion2 + ylim(c(0,n))
plotInsertion2<- plotInsertion2 + ggtitle("Possède un emploi à 30 mois. Population = 200 personnes.") 
plotInsertion2<- plotInsertion2 + scale_fill_discrete(name = "")
plotInsertion2<- plotInsertion2 + theme_minimal()
plotInsertion2

#qplot(universite$q6_5, data = universite, bins=30, size = universite$q6_5)
#ggplot(universite, aes(x = universite$q4_3)) + geom_bar()

#qplot(x=df$`18 mois`,data= df, main="Univerité données a 18 mois", ylim=c(0, 50), xlim = c(0, 100), bins=30)

#a<-ggplot(data = df, aes(x = df$`18 mois`, y = sum(df$`18 mois`)), bins=30)
#a<- a+geom_count()
#a<-a+xlab("Duree des emplois")+ylab("Total")+ggtitle("Test de plots des emploi stables a 18 mois")+scale_color_discrete(name = "Légende")
#print(a)