

library(shiny)
## library(DT)
library(ggplot2)
library(ggthemes)
library(plyr)
library(gridExtra)
library(grid)
## TODO Use interactive charts ?
## library(plotly)

## Run once when the app is launched

## Options de debuggage
options(shiny.trace=FALSE)

## Load database IP
source('ReadIP.R', local = TRUE)
data <- ReadIP('data.csv')
# print("######################### READ FINISHED#################################")

###################################### FONCTIONS ##########################################

## Rename sum function for addmargins
Total <- function(x) sum(x)

#on crée une fonction pourcentage afin de faciliter le calcule de pourcentage
pourcentage<- function(tableau, sub){
  pr<- round((nrow(sub)*100)/nrow(tableau), digits = 2)
  return(pr)
}

## 
GetPercentLabels2 <-function(val, digits = 0){
  pr<- paste(round(val, digits), "%")
  return(pr)
}

## Compute percentage labels of bar plots
GetPercentLabels <- function(x, threshold = 1, digits = 1) {
  ind <- x >= threshold
  r <- rep("", length(x))
  r[ind] <- sprintf(paste0("%.", digits, "f%%"), x[ind])
  return(r)
}

## x compris en 0 et 1. Transforme x en un pourcentage en 0 et 100.
ToHundredPercent <- function(x){
  ans <- x
  if(x < 1 && x >= 0){
    ans <- x * 100
  }
  return(ans)
}

## http://stackoverflow.com/questions/21236229/stacked-bar-chart
## http://rstudio-pubs-static.s3.amazonaws.com/4305_8df3611f69fa48c2ba6bbca9a8367895.html
## http://www.sthda.com/french/wiki/ggplot2-barplots-guide-de-demarrage-rapide-logiciel-r-et-visualisation-de-donnees
BarPlot <- function(x, threshold = 5, digits = 0) {
  x <- as.data.frame(table(x[drop=TRUE], useNA = "ifany"))
  x$label <- GetPercentLabels(100*x$Freq/sum(x$Freq), threshold, digits)
  pos <- x$Freq / 2
  ggplot(x, aes(x = Var1, y = Freq)) + geom_bar(stat="identity", position="dodge", fill = ptol_pal()(1)) + coord_flip() + geom_text(aes(y = pos, label=label), color = "white", size=6) +  theme_gdocs() 
}

BarStackedPlot2 <- function(df, aesX, aesF, legend.title = NULL, labelX = TRUE, labelF = TRUE) {
  x <- as.data.frame(ftable(df[ , c(aesX, aesF), drop=TRUE]))
  totFreq <- sum(x$Freq)
  ## Percentage label
  x$percentage <- 100 * x$Freq / totFreq
  x$percentage <-  GetPercentLabels(x$percentage, threshold = 2, digits = 0)
  x <- ddply(x, aesX, transform, pos = cumsum(Freq) - (0.5 * Freq), top = cumsum(Freq))
  x$toplab <-  GetPercentLabels(100 * x$top / totFreq, threshold = 0)
  m <- length(unique(x[,aesF]))
  ## exploit recycling
  x$toplab[ append(rep(TRUE,m-1), FALSE) ] <- "" 

    p <- ggplot(x, aes_string(x = aesX, y = "Freq", fill = aesF)) + geom_bar(stat="identity") + coord_flip() + theme_gdocs()
  
  if(labelF) {
    p <- p + geom_text(aes(y = pos, label = x$percentage, size = 6, position = "stack"), show.legend = FALSE)
  }
  
  if(labelX) {
    p <- p + geom_text(aes(y = x$top, label = x$toplab, size = 6, hjust = -0.25, vjust = -0.5, position = "stack", fontface = 2), show.legend = FALSE) + expand_limits( y = c(0,round(max(x$top)*1.05)))
  }
  
  if(is.null(legend.title)) {
    p <- p + scale_fill_ptol()
  } else {
    p <- p + scale_fill_ptol(name=legend.title) 
  }
  p <- p + theme(legend.position="bottom", legend.direction="horizontal")

  return(p)
}

salaireBrut<-function(salaireNet){
  salaire<-salaireNet*1.299
  return(salaire)
}

########################################################################################################
########################################################################################################


## TEAM STRANGE
## Calul pour le graph situation de l'emploi
##
situationEmploi <- data$situationProN30 == "En emploi "
sexe <- data$sexe

tab<-table(situationEmploi, sexe)
das<-data.frame(tab/sum(tab))
das$Freq <- round(das$Freq,2)

## Calcul Pour la progression des conditions d'emplois
## Data frame avec l'ensemble des donnees


universiteTemp <- data.frame(data$situationProN30,
                         data$statutEmploiN30,
                         data$statutEmploiN18,
                         data$niveauEmploiN30,
                         data$tempsTravail2015,
                         data$situationProN18,
                         data$niveauEmploiN18,
                         data$tempsTravail2014,
                         data$sexe,
                         data$departements,
                         data$salaireMensuel,
                         data$salaireMensuel2014,
                         data$boursier,
                         data$codeDiplome,
                         data$etudeN6,
                         data$etudeN18
                         )
universite <- setNames(universiteTemp,c("situationProN30",
                            "statutEmploiN30",
                            "statutEmploiN18",
                            "niveauEmploiN30",
                            "tempsTravail2015",
                            "situationProN18",
                            "niveauEmploiN18",
                            "tempsTravail2014",
                            "sexe",
                            "departements",
                            "salaireMensuel",
                            "salaireMensuel2014",
                            "boursier",
                            "codeDiplome",
                            "etudeN6",
                            "etudeN18"))

DATA_SIZE <- nrow((universite))
# print("universite size : ")
# print(DATA_SIZE)

# print("############str(universite)#############")
# str(universite)
##On utilise subset afin de filtrer les données et travailler sur ceux
##qui ont un emploi
# print("############str(data)#############")
# str(data)
# print("############str(data$situationProN30)#############")
# str(data$situationProN30)
insertion<-subset(universite, situationProN30 == "En emploi ")
# print("############str(insertion)#############")
# str(insertion)
# print("############colnames(insertion)#############")
# print(colnames(insertion))
# print("############nrow(insertion)#############")
# nrow(insertion)
insertion2<-subset(universite, situationProN18 ==  "En emploi ")
# print("############str(insertion2)#############")
# str(insertion2)
stable<-subset(insertion, statutEmploiN30 == "Prof. libérale, indépendant,\n chef d’entreprise, auto-entrepreneur" | statutEmploiN30 == "Fonctionnaire\n(y compris fonctionnaire stagiaire ou élève fonctionnaire)" | statutEmploiN30 == "CDI")
# print("############str(stable)#############")
# str(stable)
stable2<-subset(insertion2, statutEmploiN18 == "Prof. libérale, indépendant,\n chef d’entreprise, auto-entrepreneur" | statutEmploiN18 == "Fonctionnaire\n(y compris fonctionnaire stagiaire ou élève fonctionnaire)" | statutEmploiN18 == "CDI")
# print("############str(stable2)#############")
# str(stable2)
niveauEmploi<-subset(insertion, niveauEmploiN30 == "ingénieur, cadre, professions libérales, professions intellectuelles supérieures" | niveauEmploiN30 == "emploi de niveau intermédiaire : technicien, agent de maîtrise, maîtrise administrative et commerciale, VRP")
# print("############str(niveauEmploi)#############")
# str(niveauEmploi)
niveauEmploi2<-subset(insertion2, niveauEmploiN18 == "ingénieur, cadre, professions libérales, professions intellectuelles supérieures" | niveauEmploiN18 == "emploi de niveau intermédiaire : technicien, agent de maîtrise, maîtrise administrative et commerciale, VRP")
# print("############str(niveauEmploi2)#############")
# str(niveauEmploi2)
tempsPlein<-subset(insertion, tempsTravail2015 == "temps plein")
# print("############str(tempsPlein)#############")
# str(tempsPlein)
# print("############str(tempsPlein$salaireMensuel)#############")
# str(tempsPlein$salaireMensuel)
tempsPlein2<-subset(insertion2, tempsTravail2015 == "temps plein")
# print("############str(tempsPlein2)#############")
# str(tempsPlein2)

# print("############str(insertion$boursier)#############")
# str(insertion$boursier)
boursierAvecEmploi <- subset(insertion, boursier == "Oui")
# print("############str(boursierAvecEmploi)#############")
# str(boursierAvecEmploi)
nonBoursierAvecEmploi <- subset(insertion, boursier == "Non")
# print("############str(nonBoursierAvecEmploi)#############")
# str(nonBoursierAvecEmploi)

sansEmploi <- subset(
  universite,
  situationProN18 ==  "En recherche d'emploi" | situationProN18 == "Ne recherche pas d'emploi")
# print("############str(sansEmploi)#############")
# str(sansEmploi)

boursierSansEmploi <- subset(sansEmploi, boursier == "Oui")
# print("############str(boursierSansEmploi)#############")
# str(boursierSansEmploi)

######### TEAM STRANGE #############
## La bourse a t elle faciliter l'obtention du diplome
a<-c(1,1,2,2)
b<-c(pourcentage(insertion, boursierAvecEmploi),
     100 - pourcentage(insertion, boursierAvecEmploi),
     pourcentage(universite, boursierSansEmploi),
     100 - pourcentage(universite, boursierSansEmploi))
c<-c("boursier", "non boursier", "boursier", "non boursier")
dfBoursier<-data.frame(a,b,c)
colnames(dfBoursier)<-c("x", "y", "fill")
dfBoursier$xLabel<-ifelse(dfBoursier$x == 1, "Possède un emploi", "Ne possède pas d'emploi")
dfBoursier$freq<-GetPercentLabels(dfBoursier$y)
# print("############str(dfBoursier)#############")
# str(dfBoursier)

salBrut<-with(tempsPlein, tempsPlein$salaireMensuel) # On recupère uniquement la colonne des salaire net 
# print("############print(salBrut))#############")
# print(salBrut)
salBrutNumeric <- as.numeric(salBrut) # parce que ce sont des facteurs
salBrut<-salaireBrut(salBrutNumeric * 12) #on applique la fonciton de transformation en salaire brut
# print("############print(salBrut) après fonction salaireBrut()#############")
# print(salBrut)

salBrut30<-salaireBrut(as.numeric(with(tempsPlein2, tempsPlein2$salaireMensuel2014)) * 12) # code du haut en imbriqué (30 mois)
# print("############print(salBrut30)#############")
# print(salBrut30)


###################################
## 
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
dfPlot$freq<-GetPercentLabels2(dfPlot$y)

###################################
## Lieu de l'emlois
alpesMaritimes <- subset(insertion, departements == "Alpes-Maritimes")
alpesMaritimes <- alpesMaritimes["departements"]
alpesMaritimes <- length(alpesMaritimes$departements)

pacaHORSAlpesMaritimes <- subset(insertion, departements == "Bouches-du-Rhône" | departements == "Vaucluse" | departements == "Drôme" | departements == "Var" | departements == "Alpes-de-Haute-Provence" | departements == "Hautes-Alpes")
pacaHORSAlpesMaritimes <- pacaHORSAlpesMaritimes["departements"]
pacaHORSAlpesMaritimes <- length(pacaHORSAlpesMaritimes$departements)

HORSpaca <- subset(insertion, departements != "Bouches-du-Rhône" | departements != "Vaucluse" | departements != "Drôme" | departements != "Var" | departements != "Alpes-de-Haute-Provence" | departements != "Hautes-Alpes")
HORSpaca <- subset(HORSpaca, departements != "Alpes-Maritimes")
HORSpaca <- length(HORSpaca$departements)
# print("######### HERE #########")
# print(HORSpaca)


SIZE_INSERTION <- na.omit(insertion["departements"])
SIZE_INSERTION <- length(SIZE_INSERTION$departements)
# print(SIZE_INSERTION)

# print("########### HERE ###########")
# print(ToHundredPercent(alpesMaritimes / SIZE_INSERTION))
# print(ToHundredPercent(pacaHORSAlpesMaritimes / SIZE_INSERTION))
# print(HORSpaca / SIZE_INSERTION)
# print(ToHundredPercent(HORSpaca / SIZE_INSERTION))

x <- c("alpes-maritimes", "paca hors alpes-maritimes", "hors paca")
y <- (c(
  ToHundredPercent(alpesMaritimes / SIZE_INSERTION),
  ToHundredPercent(pacaHORSAlpesMaritimes / SIZE_INSERTION),
  ToHundredPercent(HORSpaca / SIZE_INSERTION)))
lieuEmplois <- data.frame(x, y)
print("######## HERE ##########")
print(lieuEmplois)

## Possible choices for the dynamic UI
choices.annee <- sort(unique(data$annee))
choices.grade <- sort(unique(data$libdip1))
choices.diplome <- list("Mention" = sort(unique(data$libdip2)), "Spécialité" = sort(unique(data$libdip3)), "Code SISE" = sort(unique(data$code_diplome)))

############ TODO ################
## Intégrer le code_diplome dans ReadIP
## du variable code_diplome, sur 7 caractères : exemple : 2215128

## Type de diplome possedant un emploi en master et LicencePro
emploiMaster<-subset(insertion, codeDiplome == "Master")
# print("############str(emploiMaster)#############")
# str(emploiMaster)
emploiLicencePro<- subset(insertion, codeDiplome == "LicencePro")
# print("############str(emploiLicencePro)#############")
# str(emploiLicencePro)

x<-c("Master","Licence Pro")
y<-(c(
  pourcentage(insertion, emploiMaster),
  pourcentage(insertion, emploiLicencePro)))
ne<- data.frame(x,y)
# print("############str(ne)#############")
# str(ne)

##############
## Evolution de la poursuite d'études ######

## Type de diplome avec ou sans emplois avec Master ou LicencePro
diplomeMaster <- subset(universite, codeDiplome == "Master")
# print("############str(diplomeMaster)#############")
# str(diplomeMaster)
diplomeLicencePro <- subset(universite, codeDiplome == "LicencePro")
# print("############str(diplomeLicencePro)#############")
# str(diplomeLicencePro)

## Fais des études en master en 2013-2014
poursuiteMaster <-subset(diplomeMaster, etudeN6 != "Non")
# print("############str(poursuiteMaster)#############")
# str(poursuiteMaster)
## Fais des études en master en 2014-2015
poursuiteMaster2 <-subset(diplomeMaster, etudeN18 != "Non") 
# print("############str(poursuiteMaster2)#############")
# str(poursuiteMaster2)

## Fais des études en LicencePro en 2013-2014
poursuiteLic <-subset(diplomeLicencePro, etudeN6 != "Non")
# print("############str(poursuiteLic)#############")
# str(poursuiteLic)
## Fais des études en LicencePro en 2014-2015
poursuiteLic2 <-subset(diplomeLicencePro, etudeN18 != "Non")
# print("############str(poursuiteLic2)#############")
# str(poursuiteLic2)

a<-c(1,1,2,2)
b<-c(pourcentage(diplomeMaster, poursuiteMaster),
     pourcentage(diplomeMaster, poursuiteMaster2),
     pourcentage(diplomeLicencePro, poursuiteLic),
     pourcentage(diplomeLicencePro, poursuiteLic2))
c<-c("2013-2014","2014-2015","2013-2014", "2014-2015")
poursuiteDF<-data.frame(a,b,c)
poursuiteDF$labelX<-ifelse(poursuiteDF$a ==1, "Master", "Licence Pro")
poursuiteDF$labelY<-GetPercentLabels(poursuiteDF$b)
# print(poursuiteDF)


######### Poursuite d'études à 18 mois###############
#####################################################
universite$insertionDixHuitsMois <-ifelse(universite$situationProN18 == "En emploi ", "oui", "non")
# print("############str(insertionDixHuitsMois)#############")
# str(universite$insertionDixHuitsMois)

universiteToPlot <- na.omit(universite[c("insertionDixHuitsMois", "sexe")])
# print("############str(universiteToPlot)#############")
# str(universiteToPlot)
# print("############head(universiteToPlot)#############")
# print(head(universiteToPlot))

insertionDixHuitsMoisToPlot <- na.omit(universite$insertionDixHuitsMois)
# print("############str(insertionDixHuitsMoisToPlot)#############")
# str(insertionDixHuitsMoisToPlot)

sexeToPlot <- na.omit(universiteToPlot$sexe)
# print("############length(sexeToPlot)#############")
# print(length(sexeToPlot))

######### Poursuite d'études à 30 mois ##############
#####################################################
universite$insertionTrenteMois <-ifelse(universite$situationProN30 == "En emploi ", "oui", "non")
# print("############str(insertionTrenteMois)#############")
# str(universite$insertionTrenteMois)

universiteToPlotTrenteMois <- na.omit(universite[c("insertionTrenteMois", "sexe")])
# print("############str(universiteToPlotTrenteMois)#############")
# str(universiteToPlotTrenteMois)
# print("############head(universiteToPlotTrenteMois)#############")
# print(head(universiteToPlotTrenteMois))

insertionTrenteMoisToPlot <- na.omit(universite$insertionTrenteMois)
# print("############str(insertionTrenteMoisToPlot)#############")
# str(insertionTrenteMoisToPlot)

sexeToPlotTrenteMois <- na.omit(universiteToPlotTrenteMois$sexe)
# print("############length(sexeToPlotTrenteMois)#############")
# print(length(sexeToPlotTrenteMois))
 
## https://gist.github.com/jimhester/18b9116a415633e5a73d
## IDEA https://rstudio.github.io/shinythemes/
shinyServer(
  ## Define server logic 
  function(input, output, session) {
    ## Run once each time a user visits the app
    
    # output$Debug <- renderPrint({
    #   "Pourcentage 18:"
    #   str(pourcentage18)
    #   "Pourcentage 30:"
    #   str(pourcentage30)
    # })

    ## ####################################
    ## Generate dynamic UI (not reactive)
    output$checkboxAnnee <- renderUI( {
      checkboxGroupInput("annee", "Année(s)", choices.annee, choices.annee)
    })
    
    output$checkboxGrade <- renderUI( {
      checkboxGroupInput("grade", "Grade(s)", choices.grade, choices.grade)
    })
    
    output$selectizeDiplome <- renderUI( {
      selectizeInput(
        'diplome', 'Sélectionner une ou plusieurs mentions, spécialités ou codes SISE : ', choices.diplome, multiple = TRUE, 
        options = list(
          placeholder = "Taper la sélection ici.",
          onInitialize = I('function() { this.setValue(""); }')
        ), width = "800px"
      )
    })
    ## ###########
    ## BEGIN DEBUG
    ## output$annee <- renderPrint({ input$annee })
    ## output$grade <- renderPrint({ input$grade })
    ## output$diplome <- renderPrint({ input$diplome })
    ## output$sexe <- renderPrint({ input$sexe })
    ## END DEBUG

    #######################################
    ## Sequential reactive updates
    ## recompute widgets in a fixed order (grade, sexe, diplome).
    ## TODO Watch if the selection is reduced or extended and react properly
    ## Memorize the previous length of the selection
    rdataA <- reactive( {
      ##  It is "reactive" and therefore should be automatically
      ##  re-executed when inputs change
      rdataA <- data
      if( length(input$annee) < length(choices.annee) ) {
        ## Sélection active : certains grades ne sont pas sélectionnés.
        rdataA <- rdataA[ rdataA$annee %in% input$annee, ]
      }
      rdataA
    })

    rdataG <- reactive( {
      ##  It is "reactive" and therefore should be automatically
      ##  re-executed when inputs change
      rdataG <- rdataA()
      if( length(input$grade) < length(choices.grade) ) {
        ## Sélection active : certains grades ne sont pas sélectionnés.
        rdataG <- rdataG[ rdataG$libdip1 %in% input$grade, ]
      }
      rdataG
    })

    rdataS <- reactive( {
      ##  It is "reactive" and therefore should be automatically
      ##  re-executed when inputs change
      
      rdataS <- rdataG()
      if( length(input$sexe) < 2) {
        ## Sélection active : un seul genre est sélectionné.
        rdataS <- rdataS[ rdataS$sexe %in% input$sexe, ]
      }
      rdataS
    })
    
    rdata <- reactive( {
      ##  It is "reactive" and therefore should be automatically
      ##  re-executed when inputs change
      
      rdata <- rdataS()
      if(! is.null(input$diplome) ) {
        logInd <- rdata$libdip2 %in% input$diplome | rdata$libdip3 %in% input$diplome | rdata$code_diplome %in% input$diplome
        rdata <- rdata[logInd, ]
      }
      rdata
    })

    population <- reactive({
      as.matrix(ftable(rdata()[, c("sexe", "boursier")], exclude = NULL))
    })

    intituleEmploi <- reactive({
      df <- rdata()
      jobs <- as.character(df$q6_4 [ df$q6_4 != ""])
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(jobs)
      })
    })    
        
    ## ##########################
    ## Résultats de l'enquêtes
    output$recapReponse <- renderTable( {
      n <- nrow(rdata())
      q <- sum(rdata()$repondant)
      data.frame(
        c("Nombre de diplômés (sauf VAE)", "Questionnaires exploités", "Taux de réponse"),
        c(n, q, sprintf("%.1f%%", 100*q/n))
      )
    }, colnames = FALSE
    )
    
    output$statutReponse <- renderTable({
      table(rdata()[,"statutReponse"][drop=TRUE], useNA = "ifany", dnn= "Réponse")
    })

    ## Situation des diplomés
    situationDiplomePlot <- reactive(
    {
      BarStackedPlot2(rdata(), "situationProN30", "etudeN30", "Poursuite d'étude") + ggtitle("Situation des diplômés à N + 30 mois") + labs(x="Situation professionnelle", y="Effectifs")
    })
    
    output$situationDiplome <- renderPlot({
      if(nrow(rdata()) > 0) {
        ## Avoid to run Before loading of UI and to cause Null parameters
        situationDiplomePlot()
      }
    })

   
    ## ###############################################################
    ## Caractéristiques socio-démographiques (ensemble des diplômés)
    output$populationEffectifs <- renderTable(addmargins(population(), FUN = Total, quiet = TRUE), rownames = TRUE, digits = 0)
    output$populationPourcents <- renderTable({
      x <- population()
      x <- 100*x /sum(x)
      addmargins(x, FUN = Total, quiet = TRUE)
    }, rownames = TRUE, digits = 1)


    serieBacPlot <- reactive( {
      BarStackedPlot2(rdata(), "serieBac", "regionBac", "Région d'obtention du bac") + ggtitle("Bac obtenu") + labs(x="Bac obtenu", y="Effectifs")
    })
    

  output$serieBac2 <- renderPlot(
    if(nrow(rdata()) > 0) {
      serieBacPlot()
    }
  )
  output$serieBac <- renderPlot(
      BarPlot(rdata()$serieBac) + ggtitle("Bac obtenu") + labs(x="Bac obtenu", y="Effectifs") 
    )

    output$regionBac <- renderPlot(
      BarPlot(rdata()$regionBac) + ggtitle("Région d'obtention du bac") + labs(x="Région d'obtention du bac", y="Effectifs") 
    )
    

    ## #####################
    ## Diplômés en emploi

    ## Diplômés en emploi 
    remploye <- reactive( {
      rdata()[rdata()$employe, ] 
    })

    output$nbEmploye <- renderText(paste("Il y a", nrow(remploye()), "répondants en emploi"))
    
    output$salaireParSexe <- renderTable( {
      x <- as.matrix(aggregate(remploye()$q6_9, list(remploye()$sexe), summary))
      colnames(x) <- c( "Sexe", substring(colnames(x)[-1], 3))
      x
    })
    
    output$salaire <- renderPlot( {
      salary <- as.numeric(remploye()$q6_9)
      ggplot() + aes(salary) + geom_histogram(binwidth = 250,  fill = ptol_pal()(1)) + ggtitle("Niveau de rémunération (salaire mensuel net hors primes)") +  theme_gdocs() + labs(x="Salaire", y="Effectifs")
    })


    output$regionEmploi <- renderPlot({
      BarStackedPlot2(remploye(), "regionEmploi", "regionBac", "Région d'obtention du bac") +
      labs(x="Région d'emploi", y="Effectifs") +
      ggtitle("Localisation de l'emploi et mobilité des diplomés")
    })

    niveauEmploiPlot <- reactive({
      BarStackedPlot2(remploye(), "statutEmploiN30","niveauEmploiN30", "Niveau de l'emploi") + ggtitle("Statut de l'emploi") + labs(x="Niveau de l'emploi", y="Effectifs") 
    })

    output$niveauEmploi2 <- renderPlot({
      niveauEmploiPlot()
    })
    output$statutEmploi <- renderPlot({
      BarPlot(remploye()$statutEmploiN30) + ggtitle("Statut de l'emploi") + labs(x="Statut de l'emploi", y="Effectifs") 
    })
    output$niveauEmploi <- renderPlot({
        BarPlot(remploye()$niveauEmploiN30) + ggtitle("Niveau de l'emploi") + labs(x="Niveau de l'emploi", y="Effectifs") 
    })

    ## output$typeEmployeur <- renderPlot({
    ##   BarPlot(remploye()$typeEmployeur) + ggtitle("Type d'employeur") + labs(x="Type d'employeur", y="Effectifs") 
    ## })
    output$typeEmployeur <- renderPlot({
      BarStackedPlot2(remploye(), "typeEmployeur", "effectifsEmployeur", legend.title = "Effectifs de l'employeur") +
        labs(x="Type d'employeur", y="Effectifs") +
        ggtitle("Type d'employeur")
    })
    output$activiteEcoEmployeur <- renderPlot({
      BarPlot(remploye()$activiteEcoEmployeur) + ggtitle("Activité économique de l'entreprise") + labs(x="Secteur d'activité", y="Effectifs") 
    })

    ## #############################################
    ## Generate job word cloud
    ## http://shiny.rstudio.com/gallery/word-cloud.html
    ## Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    
    output$nuageEmploi <- renderPlot({
      v <- intituleEmploi()
      wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = 2, max.words=100,
                  colors=brewer.pal(8, "Dark2"))
    })

    ## #########################################################
    ## Automatically stop a Shiny app when closing the browser tab
    ## session$onSessionEnded(stopApp)
    
    ## #########################################
    ## pass parameters to a shiny app via URL
    ## http://stackoverflow.com/questions/32872222/how-do-you-pass-parameters-to-a-shiny-app-via-url
    observe({
      query <- parseQueryString(session$clientData$url_search)
      
      nameval = "grade"
      valuetoupdate <- query[[nameval]]
      if (!is.null(query[[nameval]])) {
        valuetoupdate <- unlist(strsplit(valuetoupdate, ","))
        updateCheckboxGroupInput(session, nameval, selected = valuetoupdate)
      }

      nameval = "diplome"
      valuetoupdate <- query[[nameval]]
      if (!is.null(query[[nameval]])) {
        valuetoupdate <- unlist(strsplit(valuetoupdate, ","))
        updateSelectizeInput(session, nameval, selected = valuetoupdate)
      }
      
      nameval = "sexe"
      valuetoupdate <- query[[nameval]]
      if (!is.null(query[[nameval]])) {
        valuetoupdate <- unlist(strsplit(valuetoupdate, ","))
        updateCheckboxGroupInput(session, nameval, selected = valuetoupdate)
      }
    
    })
    
    ## generate the URL for the current selection
    url <- reactive({
      ##url <- "http://127.0.0.1:3141/?"
      url <- "http://unicepro-ove.shinyapps.io/oveshinyip/?"
       if( length(input$grade) < length(levels(data$libdip1)) ) {
         url <- paste0(
           url, "grade=", 
           paste0(input$grade, collapse = ","), "&"
         )
       }
      if(! is.null(input$diplome) ) {
        url <- paste0(
          url, "diplome=", 
          paste0(input$diplome, collapse = ","), "&"
         )
      }
      if( length(input$sexe) < 2) {
        url <- paste0(url,"sexe=",input$sexe,"&")
      }
      #3 remove last character: ? or &.
      substring(url, 0, nchar(url)-1)
    })
    output$url <- renderText(url())
    ## A reactive data source, based on the input$species
    observeEvent(input$copyButton, {
      clipr::write_clip(url())
    })

    
    ## #################################
    ## Generating downloadable reports
    ## http://shiny.rstudio.com/articles/generating-reports.html
     output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(n = 10, situationDiplome = situationDiplomePlot)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
     )

    ############################### TEAM STRANGE ############################### 
    
    ## OUTPUT : pHFEmploi : Pourcentage des hommes et femmes possédant un emploi
    output$pHFEmploi <- renderPlot({
      plot <- ggplot(das, aes(x=das$sexe, y=ToHundredPercent(round(das$Freq,2)), fill=factor(das$situationEmploi)))+ geom_bar(stat = "identity", position=position_dodge()) + 
        geom_text(aes(label=ToHundredPercent(round(das$Freq,2)), y = ToHundredPercent(round(das$Freq,2))+2),position = position_dodge(0.9), size=3.5)+
        labs(title="Pourcentage des hommes et femmes possédant un emploi", x="Sexe", y="Pourcentage", fill="")+
        ylim(c(0,100))
      plot
    })
    
    ## OUTPUT : progressionCondEmploi : Progression des conditions d'emplois (en %)
    output$progressionConditionEmploi <- renderPlot({
      plot <- ggplot(dfPlot, aes(x=factor(dfPlot$date), y=dfPlot$y, fill=factor(dfPlot$fill)))+ 
        geom_bar(stat = "identity", position=position_dodge()) + 
        geom_text(aes(label = dfPlot$freq, y = dfPlot$y + 0.15),position = position_dodge(0.9), vjust = -1)+
        labs(title="Progression des conditions d’emploi (en %)", x="", y="Pourcentage", fill="")+
        ylim(c(0,100))
      plot<- plot + theme_calc() + scale_color_calc()
      plot
    })
    
    ## data frame du tableau
    dataDepartement <- reactive({
      pourcentage18 <- c(
        pourcentage(universite, insertion),
        pourcentage(insertion, stable),
        pourcentage(insertion, tempsPlein),
        pourcentage(insertion, niveauEmploi),
        pourcentage(insertion, subset(insertion, niveauEmploiN30 == "ingénieur, cadre, professions libérales, professions intellectuelles supérieures")),
        pourcentage(insertion, subset(insertion, departements == input$Departement)),
        median(as.numeric(tempsPlein$salaireMensuel), na.rm = TRUE),
        median(salBrut, na.rm = TRUE)
      )
      pourcentage30 <- c(
        pourcentage(universite, insertion2),
        pourcentage(insertion2, stable2),
        pourcentage(insertion2, tempsPlein2),
        pourcentage(insertion2, niveauEmploi2),
        pourcentage(insertion, subset(insertion2, niveauEmploiN18 == "ingénieur, cadre, professions libérales, professions intellectuelles supérieures")),
        pourcentage(insertion2, subset(insertion2, insertion2$departement == input$Departement)),
        median(as.numeric(tempsPlein2$salaireMensuel2014), na.rm = TRUE),
        median(salBrut30, na.rm = TRUE)
      )
      df<-data.frame(
        pourcentage18, 
        pourcentage30, 
        row.names = c(
          "Taux d'insertion",
          "Part des emplois stable", 
          "Part des emplois a temps plein", 
          "Part des emplois de niveau cadre ou profession intermédiaire", 
          "Part des emplois de niveau cadre", 
          "Part des emplois en dehors de la région", 
          "Salaire Net Median des emplois à Temps Plein", 
          "Salaire brut annuel médian estimé"))
      colnames(df) <-c("18 mois", "30 mois")
      df
    })

    # TauxInsertionTable    
    output$TauxInsertion <- renderPlot({
      grid.table(dataDepartement())
      })
    
      ## TODO rendre dfBoursier reactive par rapport aux départements???
    output$bourseFaciliter <- renderPlot({
      plotBourse <- ggplot(dfBoursier, aes(x=factor(dfBoursier$xLabel), y=dfBoursier$y, fill=factor(dfBoursier$fill)))+
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_text(aes(label = dfBoursier$freq, y = dfBoursier$y + 0.15), position = position_dodge(0.9), vjust = -1) +
        labs(title="La bourse faciliter", x="", y="Pourcentage", fill="")+
        ylim(c(0,100))
      plotBourse<- plotBourse + theme_calc() + scale_color_calc()
      plotBourse
    })
    
    ##
    output$typeDiplomePossedantEmploi <- renderPlot({
      plot<- ggplot(ne, aes(x=factor(x), y = y/sum(y)*100)) + geom_bar(stat="identity")+
        labs(title="Type de diplomes possédant un emploi (en %)", x="", y="Pourcentage")+
        ylim(c(0,100)) + theme_calc() + scale_color_calc()
      plot
    })
    
    ## Évolution de la Poursuite d'études
    output$evolutionPoursuiteEtude <- renderPlot({
      plotPoursuite<- ggplot(poursuiteDF, aes(x=factor(labelX), y = b, fill = c)) + geom_bar(stat="identity", position = position_dodge())+
        labs(title="Evolution de la poursuite d'étude (en %)", x="", y="Pourcentage", fill="Par année")+
        geom_text(aes(label = poursuiteDF$labelY, y = b + 0.15),position = position_dodge(0.9), vjust = -1)+
        ylim(c(0,100)) + theme_calc() + scale_color_calc()
      plotPoursuite
    })
    
    ## Possède un emplois à 18 mois
    output$possedeEmploisDixHuitsMois <- renderPlot({
      population <- length(universiteToPlot$insertionDixHuitsMois)
      plotInsertion<-ggplot(universiteToPlot, aes(x=universiteToPlot$insertionDixHuitsMois, fill=universiteToPlot$sexe)) 
      plotInsertion<- plotInsertion + geom_bar() 
      plotInsertion<- plotInsertion + xlab("") 
      plotInsertion<- plotInsertion + ylab("Total") 
      plotInsertion<- plotInsertion + ylim(c(0, DATA_SIZE))
      plotInsertion<- plotInsertion + ggtitle(sprintf("Possède un emplois à 18 mois. Population est de %d personnes", population)) 
      plotInsertion<- plotInsertion + scale_fill_discrete(name = "")
      plotInsertion<- plotInsertion + theme_minimal()
      plotInsertion
    })
    ## Possède un emplois à 30 mois
    output$possedeEmploisTrenteMois <- renderPlot({
      population <- length(universiteToPlotTrenteMois$insertionTrenteMois)
      plotInsertion2<-ggplot(universiteToPlotTrenteMois, aes(x=universiteToPlotTrenteMois$insertionTrenteMois, fill=universiteToPlotTrenteMois$sexe)) 
      plotInsertion2<- plotInsertion2 + geom_bar(position=position_dodge()) 
      plotInsertion2<- plotInsertion2 + xlab("") 
      plotInsertion2<- plotInsertion2 + ylab("Total") 
      plotInsertion2<- plotInsertion2 + ylim(c(0,DATA_SIZE))
      plotInsertion2<- plotInsertion2 + ggtitle(sprintf("Possède un emploi à 30 mois. Population = %d personnes", population)) 
      plotInsertion2<- plotInsertion2 + scale_fill_discrete(name = "")
      plotInsertion2<- plotInsertion2 + theme_minimal()
      plotInsertion2
    })
    
    ## Lieu de l'emploi
    output$lieuDeLemploi <- renderPlot({
      plot <- ggplot(lieuEmplois, aes(x=x, y = lieuEmplois$y, fill=x))+ 
        geom_bar(stat = "identity", position=position_dodge()) + 
        geom_text(aes(label = round(y,2), y = lieuEmplois$y + 0.15),position = position_dodge(0.9), vjust = -1)+
        labs(title="Lieu d'emplois (en %)", x="", y="Pourcentage", fill="")+
        ylim(c(0,100))
      plot<- plot + theme_calc() + scale_color_calc()
      plot
    })
  }
)