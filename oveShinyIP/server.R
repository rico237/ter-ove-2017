 library(shiny)
## library(DT)
library(ggplot2)
library(ggthemes)
library(plyr)
## TODO Use interactive charts ?
## library(plotly)

## Run once when the app is launched

## Load database IP
source('ReadIP.R', local = TRUE)
data <- ReadIP('data.csv')


## Rename sum function for addmargins
Total <- function(x) sum(x)

## Compute percentage labels of bar plots
GetPercentLabels <- function(x, threshold = 1, digits = 1) {
  ind <- x >= threshold
  r <- rep("", length(x))
  r[ind] <- sprintf(paste0("%.", digits, "f%%"), x[ind])
  return(r)
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


## Possible choices for the dynamic UI
choices.annee <- sort(unique(data$annee))
choices.grade <- sort(unique(data$libdip1))
choices.diplome <- list("Mention" = sort(unique(data$libdip2)), "Spécialité" = sort(unique(data$libdip3)), "Code SISE" = sort(unique(data$code_diplome)))


## https://gist.github.com/jimhester/18b9116a415633e5a73d
## IDEA https://rstudio.github.io/shinythemes/
shinyServer(
  ## Define server logic 
  function(input, output, session) {
    ## Run once each time a user visits the app

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

    
  }
  )







