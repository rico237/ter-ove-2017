# http://www.ats.ucla.edu/stat/r/faq/barplotplus.htm

library(shiny)
## library(plotly)

## TODO http://deanattali.com/blog/advanced-shiny-tips/
## http://zevross.com/blog/2016/04/19/r-powered-web-applications-with-shiny-a-tutorial-and-cheat-sheet-with-40-example-apps/
## https://github.com/aoles/shinyURL

## Define UI
fluidPage( theme = "bootstrap.min.css",
  titlePanel("Le butineur 2.0 de l'OVE"),
  fluidRow(
    column(1, uiOutput("checkboxAnnee")),
    column(2, uiOutput("checkboxGrade")),
    column(6,
     uiOutput("selectizeDiplome"),
     column(6,
      actionButton("copyButton", "Copier l'URL dans le presse-papier."),
      verbatimTextOutput("url"),
      align="center" 
      ),
     column(6,
      downloadButton("report", "Télecharger un rapport PDF"),
      align="center" 
      )
     ),
    column(1, checkboxGroupInput("sexe", "Sexe(s)", c("Femme", "Homme"), c("Femme", "Homme")))
    ),
  ## ###########
  ## BEGIN DEBUG
  ## fluidRow(
  ##   column(1, verbatimTextOutput("annee")),
  ##   column(2, verbatimTextOutput("grade")),
  ##   column(6, verbatimTextOutput("diplome")),
  ##   column(2, verbatimTextOutput("sexe"))
  ## ),
  ## END DEBUG
  fluidRow(
    tabsetPanel(
      tabPanel("Résultats",
       h2("Résultats de l'enquête"),
       fluidRow(
         column(4,
          tableOutput("recapReponse"),
          tableOutput("statutReponse")
          ),
         column(8, plotOutput("situationDiplome"))
         )),
      tabPanel("Population",
       h2("Caractéristiques socio-démographiques (ensemble des diplômés)"),
       fluidRow(
         column(3, h3("Tableau des effectifs"), tableOutput("populationEffectifs")),
         column(3, h3("En pourcentage"),tableOutput("populationPourcents")),
         column(6, plotOutput("serieBac2"))                
         ),
       h3("Baccalauréat"),
       fluidRow(
         column(6, plotOutput("serieBac")),
         column(6, plotOutput("regionBac"))
         )),
      ## h2("Diplômés sur le marché du travail"),
      ## fluidRow(
      ##   column(5,plotOutput("plot0"))
      ## ),
      tabPanel("Emploi",
       h2("Diplômés en emploi"),
       fluidRow(
         column(6, textOutput("nbEmploye")),
         column(6, tableOutput("salaireParSexe"))
         ),
       fluidRow(
         column(6, plotOutput("regionEmploi")),
         column(6, plotOutput("salaire"))
         ),
       fluidRow(
         column(6,plotOutput("statutEmploi")),
         column(6,plotOutput("niveauEmploi"))
         ),
       fluidRow(
        column(12,plotOutput("niveauEmploi2"))
        ),
       fluidRow(
         column(6,plotOutput("typeEmployeur")),
         column(6,plotOutput("activiteEcoEmployeur"))
         )),
      tabPanel("Nuage d'emplois", plotOutput("nuageEmploi")),


      ## Debut du travail de la team strange
      tabPanel("Team Strange",
        h2("Travaux effectué par la team strange depuis le debut du TER"),
        fluidRow(
          column(6, h2("Merc"))
          ))
      
      )
    )
  )

