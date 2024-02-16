#' Title calcul_tableau_amortissement
#' 
#' Cette fonction calcule un tableau d'amortissement pour un 
#' prêt immobilier en se basant sur les paramètres suivants :
#' 
#' @param emprunt Le montant du crédit
#' @param taux_interet Le taux annuel d'intérêt
#' @param duree La Durée du crédit en années
#' @param assurance Le taux annuel d'assurance
#' @param apport Le montant d'apport personnel
#'
#' @return Un dataframe contenant:
#' Mois : Le mois en cours
#' Capital restant dû : Le capital restant à rembourser
#' Interets : Les intérêts payés pour le mois en cours
#' Principal : Le montant du capital remboursé pour le mois en cours
#' Assurance : Le montant de l'assurance mensuelle
#' Mensualite : Le montant total de la mensualité
#' 
#' @export
#' @examples
#' tab_amor <- calcul_tableau_amortissement(emprunt = 100000, taux_interet = 4.5,
#'  duree = 20, assurance = 0.5, apport = 20000)
#' print(tab_amor)

calcul_tableau_amortissement <-
  function(emprunt,taux_interet,duree,assurance,apport) {
    
    if (emprunt <= 0) {
      stop("L'emprunt doit être superieur à 0.")
    }
    
    if (taux_interet < 0 || taux_interet > 100) {
      stop("Le taux d'intérêt doit être entre 0 et 100.")
    }
    
    if (assurance < 0 || assurance > 100) {
      stop("Le taux d'assurance doit être entre 0 et 100.")
    }
    
    if (duree <= 0) {
      stop("L'emprunt doit être superieur à 0.")
    }
    
    if (apport < 0) {
      stop("L'apport doit être ≥ 0.")
    }
    
    # Initialisation du tableau
    tableau_amortissement <- data.frame(
      Numero = numeric(0),
      Capital_restant_du = numeric(0),
      Interets = numeric(0),
      Principal = numeric(0),
      Assurance = numeric(0),
      Mensualite = numeric(0)
    )
    
    taux_mensuel <- taux_interet / 12 / 100
    nombre_mensualites <- duree * 12
    capital_restant <- emprunt - apport
    mensualite_sans_assurance <-
      (capital_restant * taux_mensuel) / (1 - (1 + taux_mensuel) ** -nombre_mensualites)
    assurance_mensuel <- (assurance / 100) * capital_restant / 12
    
    for (mois in 1:nombre_mensualites) {
      # Calcul du montant de l'intérêt au mois en cours
      interets_mois <- capital_restant * taux_mensuel
      
      # Calcul du capital remboursé au mois en cours
      principal_mois <- mensualite_sans_assurance - interets_mois
      
      mensualite_totale <-
        mensualite_sans_assurance + assurance_mensuel
      
      tableau_amortissement <- rbind(tableau_amortissement,
                                     c(
                                       mois,
                                       round(capital_restant, 2),
                                       round(interets_mois, 2),
                                       round(principal_mois, 2),
                                       round(assurance_mensuel, 2),
                                       round(mensualite_totale, 2)
                                     ))
      
      # Calcul du capital restant dû au mois en cours
      capital_restant <- capital_restant - principal_mois
      
    }
    
    colnames(tableau_amortissement) <-
      c("Mois",
        "Capital restant dû",
        "Interets",
        "Principal",
        "Assurance",
        "Mensualite")
    return(tableau_amortissement)
}