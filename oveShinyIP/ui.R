# http://www.ats.ucla.edu/stat/r/faq/barplotplus.htm

library(shiny)
## library(plotly)

## TODO http://deanattali.com/blog/advanced-shiny-tips/
## http://zevross.com/blog/2016/04/19/r-powered-web-applications-with-shiny-a-tutorial-and-cheat-sheet-with-40-example-apps/
## https://github.com/aoles/shinyURL

departements <- c("Ain",
                 "Aisne",
                 "Allier",
                 "Hautes-Alpes",
                 "Alpes-de-Haute-Provence",
                 "Alpes-Maritimes",
                 "Ardèche",
                 "Ardennes",
                 "Ariège",
                 "Aube",
                 "Aude",
                 "Aveyron",
                 "Bouches-du-Rhône",
                 "Calvados",
                 "Cantal",
                 "Charente",
                 "Charente-Maritime",
                 "Cher",
                 "Corrèze",
                 "Corse-du-sud",
                 "Haute-corse",
                 "Côte-d'or",
                 "Côtes-d'armor",
                 "Creuse",
                 "Dordogne",
                 "Doubs",
                 "Drôme",
                 "Eure",
                 "Eure-et-Loir",
                 "Finistère",
                 "Gard",
                 "Haute-Garonne",
                 "Gers",
                 "Gironde",
                 "Hérault",
                 "Ile-et-Vilaine",
                 "Indre",
                 "Indre-et-Loire",
                 "Isère",
                 "Jura",
                 "Landes",
                 "Loir-et-Cher",
                 "Loire",
                 "Haute-Loire",
                 "Loire-Atlantique",
                 "Loiret",
                 "Lot",
                 "Lot-et-Garonne",
                 "Lozère",
                 "Maine-et-Loire",
                 "Manche",
                 "Marne",
                 "Haute-Marne",
                 "Mayenne",
                 "Meurthe-et-Moselle",
                 "Meuse",
                 "Morbihan",
                 "Moselle",
                 "Nièvre",
                 "Nord",
                 "Oise",
                 "Orne",
                 "Pas-de-Calais",
                 "Puy-de-Dôme",
                 "Pyrénées-Atlantiques",
                 "Hautes-Pyrénées",
                 "Pyrénées-Orientales",
                 "Bas-Rhin",
                 "Haut-Rhin",
                 "Rhône",
                 "Haute-Saône",
                 "Saône-et-Loire",
                 "Sarthe",
                 "Savoie",
                 "Haute-Savoie",
                 "Paris",
                 "Seine-Maritime",
                 "Seine-et-Marne",
                 "Yvelines",
                 "Deux-Sèvres",
                 "Somme",
                 "Tarn",
                 "Tarn-et-Garonne",
                 "Var",
                 "Vaucluse",
                 "Vendée",
                 "Vienne",
                 "Haute-Vienne",
                 "Vosges",
                 "Yonne",
                 "Territoire de Belfort",
                 "Essonne",
                 "Hauts-de-Seine",
                 "Seine-Saint-Denis",
                 "Val-de-Marne",
                 "Val-d'oise",
                 "Mayotte",
                 "Guadeloupe",
                 "Guyane",
                 "Martinique",
                 "Réunion"
)

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
                 column(6,
                        textOutput("nbEmploye")
                        ),
                 column(6,
                        tableOutput("salaireParSexe")
                        )
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
                 column(8,plotOutput("niveauEmploi2"))
               ),
               fluidRow(
                 column(6,plotOutput("typeEmployeur")),
                 column(6,plotOutput("activiteEcoEmployeur"))
               )),
      tabPanel("TeamStrange", 
               fluidRow(
                 column(4,
                        tags$p("Ce graphique montre une égalité entre le pourcentage d’homme et de femme possédant un emploi.
")),
                 column(8,offset = 6,
                        plotOutput("pHFEmploi")))
                        
               ,
               fluidRow(
                 column(3,
                        fluidRow(
                          column(12,
                                 selectInput("Departement", "departements:", departements))),
                        tags$p("Le pourcentage d'insertion après 18 mois et 30 mois. Ce tableau résume, pour 18 et 30 mois, les différentes taux d’insertion.
")),
                 column(9,
                        plotOutput("TauxInsertion"))
               ),
               fluidRow(
                 column(4,
                        tags$p("Quel pourcentage possède le diplôme Licence Pro par rapport au diplôme de Master. On note clairement un taux d’emplois supérieur chez les étudiant possédant un Master que ceux qui
possèdent une Licence Pro.")),
                 column(8,
                        plotOutput("typeDiplomePossedantEmploi"))
               ),
               fluidRow(
                 column(4,
                        tags$p("Près de 60% des diplômés en licence professionnelle poursuivent des études. Le taux de poursuite
d'étude est nettement inférieur en master où seulement 33% ont poursuivis des études en 2013-2014 et
seulement 20% en 2014-2015. L'information importante est qu'il y a eu 13% d'étudiant de master qui
n'ont pas poursuivis leur études après leur master en 2014-2015 comparé à l'année précédente.
")),
                 column(8,
                        plotOutput("evolutionPoursuiteEtude"))
               ),
               fluidRow(
                 column(4,
                        tags$p("Il y a une égalité entre les deux sexes. Il y a presque autant d’homme et de femme qui possède un
emploi après 18 mois.")),
                 column(8,
                        plotOutput("possedeEmploisDixHuitsMois"))
               ),
               fluidRow(
                 column(4,
                        tags$p("En comparant ce graphe à celui des 18 mois, on peut noter qu’il y a un peu plus de femme sans emplois
à 30 mois. Cependant le nombre d’homme et de femme avec un emplois est similaire.")),
                 column(8,
                        plotOutput("possedeEmploisTrenteMois"))
               ),
               fluidRow(
                 column(4,
                        tags$p("On note qu’il y a une majorité qui sont employé à temps plein. Ce pourcentage diminue légèrement à 30
mois mais reste néanmoins au dessus de 80%. C’est un élément exploitable pour l’OVE. Cependant, le
pourcentage d’emploi stable qui est de 77% après 18 mois tombe à 65% après 30 mois. Les emplois
cadres ou intermédiaires restent au dessus des 50% pour 18 et 30 mois. Notons également que ces
données proviennent du ministère de l’intérieur.")),
                 column(8,
                        plotOutput("progressionConditionEmploi"))
               ),
               fluidRow(
                 column(4,
                        tags$p("Lieu de l'emploi. On peut remarquer que la majorité (52,29%) des étudiants ayant obtenu leur diplôme dans les Alpes
Maritimes trouve un travaille dans la région. Et très peux de de diplômés dans la région paca hors
Alpes-Maritimes (3,73%) ce qui signifie que presque l’autre moitié des étudiants diplômés, quitte la
région paca (47,71%).")),
                 column(8,
                        plotOutput("lieuDeLemploi"))
               )
               
      ),
      tabPanel("En Plus",
               fluidRow(
                 column(4,
                        tags$p("Parmis ceux qui ne possède pas d'emplois, la majorité ne sont pas boursier de l'état. Hors parmi ceux
qui possède un emplois 23% sont boursier.
")),
                 column(8,
                        plotOutput("bourseFaciliter"))
               )
              )
               
    )
  ),
  h2("The End !")
)

