library(ggplot2)
library(ggthemes)
library(plyr)

GetPercentLabels <-function(val, digits = 0){
  pr<- paste(round(val, digits), "%")
  return(pr)
}

#on crée une fonction pourcentage afin de faciliter le calcule de pourcentage
pourcentage<- function(tableau, sub){
  pr<- round((nrow(sub)*100)/nrow(tableau), digits = 2)
  return(pr)
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

#On utilise subset afin de filtrer les données et travailler sur ceux qui ont un emploi
insertion<-subset(universite, q4_3 == 1)
insertion2<-subset(universite, q7_1 == 1)
stable<-subset(insertion, q6_5 == 1 | q6_5 == 2 | q6_5 == 3)
stable2<-subset(insertion2, q8_1 == 1 | q8_1 == 2 | q8_1 == 3)
niveauEmploi<-subset(insertion, q6_6r == 2 | q6_6r == 4)
niveauEmploi2<-subset(insertion2, q8_2r == 2 | q8_2r == 4)
tempsPlein<-subset(insertion, q6_7 == 1)
tempsPlein2<-subset(insertion2, q8_3 == 1)


x<-c(1, 1, 1, 2, 2, 2)
y<-c(pourcentage(insertion, stable),
     pourcentage(insertion, tempsPlein),
     pourcentage(insertion, niveauEmploi),
     pourcentage(insertion2, stable2),
     pourcentage(insertion2, tempsPlein2),
     pourcentage(insertion2, niveauEmploi2))
fill<-c("Emploi stable","Emploi à temps plein","Emploi cadre ou intermédiaires","Emploi stable","Emploi à temps plein","Emploi cadre ou intermédiaires")
dfPlot<-data.frame(x,y,fill)
dfPlot$date<-ifelse(dfPlot$x == 1, "18 mois après le diplome", "30 mois après le diplome")
dfPlot$freq<-GetPercentLabels(dfPlot$y)
dfPlot

plot <- ggplot(dfPlot, aes(x=factor(dfPlot$date), y=dfPlot$y, fill=factor(dfPlot$fill)))+ 
  geom_bar(stat = "identity", position=position_dodge()) + 
  geom_text(aes(label = dfPlot$freq, y = dfPlot$y + 0.15),position = position_dodge(0.9), vjust = -1)+
  #geom_text(aes(label=dfPlot$y))+
  labs(title="Progression des conditions d’emploi (en %)", x="", y="Pourcentage", fill="")+
  ylim(c(0,100))
plot<- plot + theme_calc() + scale_color_calc()
plot




