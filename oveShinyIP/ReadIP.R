
ReadIP <- function(file) {
  ## Read the IP database and preprocess the data.
  ## Columns must be given as MNESR codes.
  ## Visualization uses mostly columns created by this function.
  ## The survey model and charts model are separated.
  df <- read.csv(file = file, sep =";", row.names = NULL, na.strings = c("NA", ""))
  
  ColToFactor <- function(fromCol, toCol, labels, levels = seq_along(labels)) {
    df[, toCol] <<- factor(df[, fromCol], levels = levels, labels = labels)
  }
  
  ColToFactor("c_sexe", "sexe", c("Femme", "Homme"), c("f", "h"))
  ## Detailed classification

  bacs <- read.csv(file = "baccalaureats.csv", na.strings = c("NA", "N/A", ""))
  bacs <- aggregate(bacs$bac, by = list(bacs$categorie), paste)
  x <- bacs[,2]
  names(x) <- bacs[,1]
  bacs <- x
  df$serieBac <- df$c_serie_bac
  levels(df$serieBac) <- bacs

  
  GetRegion <- function(x) {
    y <- c(rep(3, 98), 4)
    y[6] <- 1
    y[c(4,5,13,83,84)] <- 2
    z <- factor(y[x], levels = 1:4, labels = c("Alpes-Maritimes", "PACA hors\nAlpes-Maritimes", "Hors PACA", "Étranger"))
  }
  
  df$regionBac <- GetRegion(df$region_bac)
  df$regionEmploi <- GetRegion(df$q6_14a)

  df$repondant <- df$statut_reponse %in% 4:6
  df$employe <- df$repondant & df$q4_3 == 1
  
  
  ColToFactor(
    "statut_reponse", "statutReponse",
    c(
      "Deuxième diplôme dans le cas d'un double diplôme",
      "Décédé",
      "Erreur sur le diplôme : diplômé n’ayant pas validé le diplôme",
      "téléphone",
      "internet",
      "papier",
      "Diplômé issu d'une formation délocalisée (pour les diplômés étrangers uniquement)",
      "Autre"
    )
  )

  ## TODO add parameter
  ## ColToFactor("q2_2", "boursier", c("Oui sur critères sociaux", "Oui sur d’autres critères", "Non"))
  #ColToFactor("q2_2", "boursier", list(Oui = 1:2, Non=3))
  df$boursier <- as.factor(df$q2_2)
  levels(df$boursier) <- list(Oui=1:2, Non=3)
  
  poursuiteEtude <- c(
    "En doctorat (Master) / en Master (LP)",
    "Dans une autre formation",
    "Non"
  )
  
  ColToFactor("q3_1_1", "etudeN6", poursuiteEtude)
  ColToFactor("q3_1_2", "etudeN18", poursuiteEtude)
  ColToFactor("q4_1", "etudeN30", poursuiteEtude)
  
  
  ## situationPro <- c(
  ##   "Vous avez un emploi ",
  ##   "Vous n’avez pas d’emploi et\n vous recherchez du travail ou\n vous êtes en attente d’un contrat",
  ##   "Vous n’avez pas d’emploi et\n vous ne cherchez pas de travail"
  ## )
  situationPro <- c(
    "En emploi ",
    "En recherche d'emploi",
    "Ne recherche pas d'emploi"
  )
  ColToFactor("q4_3", "situationProN30", situationPro)
  ColToFactor("q7_1", "situationProN18", situationPro)

  
  statutEmploi <- c( 
    "Prof. libérale, indépendant,\n chef d’entreprise, auto-entrepreneur",
    "Fonctionnaire\n(y compris fonctionnaire stagiaire ou élève fonctionnaire)",
    "CDI",
    "Contrat spécifique au doctorat\n(contrat doctoral, allocation recherche, CIFRE….)",
    "CDD (hors contrat spécifique au doctorat et \ny compris contractuel de la fonction publique...)",
    "Vacataire",
    "Intérimaire",
    "Intermittent du spectacle, pigiste",
    "Contrat d’apprentissage",
    "Contrat de professionnalisation",
    "Emplois aidés (Contrat Initiative Emploi…)",
    "Volontariat international",
    "Service civique")

  ColToFactor("q6_5", "statutEmploiN30", statutEmploi)
  ColToFactor("q8_1", "statutEmploiN18", statutEmploi)

  
  niveauEmploi <- c(
    "personnel de catégorie A de la fonction publique",
    "ingénieur, cadre, professions libérales, professions intellectuelles supérieures",
    "personnel de catégorie B de la fonction publique",
    "emploi de niveau intermédiaire : technicien, agent de maîtrise, maîtrise administrative et commerciale, VRP",
    "personnel de catégorie C de la fonction publique",
    "manœuvre, ouvrier",
    "employé de bureau, de commerce, personnel de service"
  )

  ColToFactor("q6_6r", "niveauEmploiN30", niveauEmploi)
  ColToFactor("q8_2r", "niveauEmploiN18", niveauEmploi)

  typeEmployeur <- c(
    "vous-même",
    "la fonction publique\n (d'Etat, territoriale ou hospitalière)",
    "une entreprise publique",
    "une entreprise privée",
    "un organisme à but non lucratif ou\n une association",
    "une personne exerçant une profession libérale ou\n un indépendant (cabinet, étude notariale…)",
    "un particulier"
  )
  ColToFactor("q6_12", "typeEmployeur", typeEmployeur)

  activiteEcoEmployeur <- c( 
    "Agriculture, sylviculture et pêche",
    "Industries (manufacturières, extractives et autres)",
    "Construction",
    "Commerce, transports, hébergement et restauration",
    "Information et communication",
    "Activités financières et d’assurance",
    "Activités spécialisées, scientifiques et techniques",
    "Activités de services administratifs et de soutien",
    "Enseignement",
    "Administration publique (hors enseignement)",
    "Santé humaine et action sociale",
    "Arts, spectacles et activités récréatives",
    "Autres activités de service"
  )
  ColToFactor("q6_13", "activiteEcoEmployeur", activiteEcoEmployeur)
  
  return(df)
}
