library(ggplot2)
library(ggthemes)

GetPercentLabels <-function(val, digits = 0){
  pr<- paste(round(val, digits), "%")
  return(pr)
}
#on crée une fonction pourcentage afin de faciliter le calcule de pourcentage
pourcentage<- function(tableau, sub){
  pr<- round((nrow(sub)*100)/nrow(tableau), digits = 2)
  return(pr)
}

n<-200 #taille des donees a generer

# 18 mois
q4_3 <- sample(1:3, size=n,prob=c(0.84, 0.05, 0.11), replace= TRUE) # Possède un emploi ou pas
q6_5 <- sample(1:13, size=n, replace= TRUE) # Type d'emploi
q6_6r<- sample(2:7, size=n, replace = TRUE) # Niveau d'emploi (ex: Cadre, Ingénieur, etc)
q6_7 <- sample(1:2, size = n,prob=c(0.82, 0.18), replace = TRUE) # Temps partiel ou pleins
q6_14a<-sample(1:99, size=n, replace= TRUE) # Départements
q6_9 <- sample(13*10:20*10, size = n, replace = TRUE) #Salaire Net
q2_2<-  sample(1:3, size = n, prob = c(0.1, 0.08, 0.82), replace = TRUE) #boursier

code_diplome<-sample(c("Licence Pro", "Master"),prob = c(0.4, 0.6), size = n, replace= TRUE)
# consernant Q3
# Poursuite etude 1 = oui (master ou doctorat), 2 = formation autre et 3 = non
q3_1_1 <-sample(1:3, prob=c(0.1,0.05, 0.85), size = n, replace = TRUE) #Poursuite etude 2013-14
q3_1_2 <-sample(1:3, prob = c(0.2, 0.02, 0.78) ,size = n, replace = TRUE) #Poursuite etude 2014-15
#q4_1   <-sample(1:3, prob = c(0.3, 0.1, 0.6), size = n, replace = TRUE) #Poursuite etude 2015-16

#Type de parcour scolaire (j'imagine)
q2_4_1 <-sample(1:2, size = n, replace= TRUE) #ec commerce-gestion / BTS
q2_4_2 <-sample(1:2, size = n, replace= TRUE) #ec inge / DUT
q2_4_3 <-sample(1:2, size = n, replace= TRUE) #droit / Licence general
q2_4_4 <-sample(1:2, size = n, replace= TRUE) #medecine etc / Autre LP
q2_4_5 <-sample(1:2, size = n, replace= TRUE) #IEP / Master
q2_4_6 <-sample(1:2, size = n, replace= TRUE) #M2 different du M1 / Autre
q2_4_7 <-sample(1:2, size = n, replace= TRUE) #autre diplome / aucun



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
universite <- data.frame(q4_3 , q6_5, q6_6r, q6_7, q7_1, q8_1, q3_1_1, q3_1_2, q8_2r, code_diplome, q8_3, c_sexe, q6_14a, dep30 , q6_9, q8_5, q2_2, boursier30, q2_4_1, q2_4_2, q2_4_3, q2_4_4, q2_4_5, q2_4_6, q2_4_7)

#On utilise subset afin de filtrer les données et travailler sur ceux qui ont un emploi
insertion<-subset(universite, q4_3 == 1)
insertion2<-subset(universite, q7_1 == 1)
stable<-subset(insertion, q6_5 == 1 | q6_5 == 2 | q6_5 == 3)
stable2<-subset(insertion2, q8_1 == 1 | q8_1 == 2 | q8_1 == 3)
niveauEmploi<-subset(insertion, q6_6r == 2 | q6_6r == 4)
niveauEmploi2<-subset(insertion2, q8_2r == 2 | q8_2r == 4)
tempsPlein<-subset(insertion, q6_7 == 1)
tempsPlein2<-subset(insertion2, q8_3 == 1)

emploiMaster<-subset(insertion, insertion$code_diplome == "Master")
emploiLP<- subset(insertion, insertion$code_diplome == "Licence Pro")

dipMaster  <- subset(universite, code_diplome=="Master")
dipLP <- subset(universite, code_diplome=="Licence Pro")

poursuiteMaster <-subset(dipMaster, q3_1_1 != 3) #13-14
poursuiteMaster2 <-subset(dipMaster, q3_1_2 != 3) #14-15

poursuiteLic <-subset(dipLP, q3_1_1 != 3) #13-14
poursuiteLic2 <-subset(dipLP, q3_1_2 != 3) #14-15


x<-c("Master","Licence Pro")
y<-(c(pourcentage(insertion, emploiMaster), pourcentage(insertion, emploiLP)))
ne<- data.frame(x,y)
plot<- ggplot(ne, aes(x=factor(x), y = y/sum(y)*100)) + geom_bar(stat="identity")+
  labs(title="Type de diplomes possédant un emploi (en %)", x="", y="Pourcentage")+
  ylim(c(0,100)) + theme_calc() + scale_color_calc()
plot

a<-c(1,1,2,2)
b<-c(pourcentage(dipMaster, poursuiteMaster),
     pourcentage(dipMaster, poursuiteMaster2),
     pourcentage(dipLP, poursuiteLic),
     pourcentage(dipLP, poursuiteLic2))
c<-c("2013-2014","2014-2015","2013-2014", "2014-2015")
poursuiteDF<-data.frame(a,b,c)
poursuiteDF$labelX<-ifelse(poursuiteDF$a ==1, "Master", "Licence Pro")
poursuiteDF$labelY<-GetPercentLabels(poursuiteDF$b)
print(poursuiteDF)
plotPoursuite<- ggplot(poursuiteDF, aes(x=factor(labelX), y = b, fill = c)) + geom_bar(stat="identity", position = position_dodge())+
  labs(title="Evolution de la poursuite d'étude (en %)", x="", y="Pourcentage", fill="Par année")+
  geom_text(aes(label = poursuiteDF$labelY, y = b + 0.15),position = position_dodge(0.9), vjust = -1)+
  ylim(c(0,100)) + theme_calc() + scale_color_calc()
plotPoursuite

plot <- ggplot(dfPlot, aes(x=factor(dfPlot$date), y=dfPlot$y, fill=factor(dfPlot$fill)))+ 
  geom_bar(stat = "identity", position=position_dodge()) + 
  geom_text(aes(label = dfPlot$freq, y = dfPlot$y + 0.15),position = position_dodge(0.9), vjust = -1)+
  labs(title="Progression des conditions d’emploi (en %)", x="", y="Pourcentage", fill="")+
  ylim(c(0,100))
plot<- plot + theme_calc() + scale_color_calc()
plot

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