#' Title calcul_mensualite
#' 
#' Cette fonction calcule la mensualité d'un prêt immobilier
#' en se basant sur les paramètres suivants :
#' 
#' @param emprunt Le montant du crédit
#' @param taux_interet Le taux annuel d'intérêt
#' @param duree La durée du crédit en années
#' @param frais Les frais et tous les autres frais bancaires
#' @param assurance Le taux annuel d'assurance
#' @param apport Le montant de l'apport personnel
#' @param revenus1 Le revenu mensuel net du premier emprunteur
#' @param revenus2 Le revenu mensuel net du deuxième emprunteur (égal à 0 si un seul)
#'
#' @return Une liste qui contient :
#'  mensualite_totale : Le montant total de la mensualité
#'  mensualite_sans_assurance : Le montant de la mensualité sans l'assurance
#'  cout_assurance_mensuel : Le coût de l'assurance mensuelle
#'  taux_endettement : Le taux d'endettement
#'  
#' @export
#' @examples
#' resultat <- calcul_mensualite(emprunt = 100000, taux_interet = 4.5,
#' duree = 20, frais = 2000, assurance = 0.5, apport = 20000,
#' revenus1 = 5000, revenus2 = 4000)
#' print(resultat)

calcul_mensualite <-
  function(emprunt,taux_interet,duree,frais,assurance,apport,revenus1,revenus2) {
    
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
      stop("La durée doit être superieur à 0.")
    }
    
    if (revenus1 < 0 || revenus2 < 0) {
      stop("Les revenus ne peuvent pas être négatifs.")
    }
    
    if (frais < 0) {
      stop("Les frais doivent être ≥ 0.")
    }
    
    if (apport < 0) {
      stop("L'apport doit être ≥ 0.")
    }
    
    # Ajustement du montant de crédit
    emprunt_sans_frais <- emprunt - apport
    emprunt_sans_frais <- emprunt_sans_frais 
    
    # Calcul des taux mensuel
    taux_mensuel <- taux_interet / 12 / 100
    assurance_mensuel <- assurance / 12 / 100
    cout_assurance_mensuel <-emprunt_sans_frais * assurance_mensuel
    
    nombre_mensualites <- duree * 12
    
    # Calcul des mensualités
    mensualite_sans_assurance <-
      (emprunt_sans_frais * taux_mensuel) / (1 - (1 + taux_mensuel) ^ -nombre_mensualites)
    mensualite_totale <-
      mensualite_sans_assurance + cout_assurance_mensuel
    
    # Taux d'endettement
    revenus_annuels <- (revenus1 + revenus2) * 12
    taux_endettement <-(mensualite_totale * 12 / revenus_annuels) * 100
    
    # Sorties
    return(
      list(
        mensualite_totale = mensualite_totale,
        mensualite_sans_assurance = mensualite_sans_assurance,
        cout_assurance_mensuel = cout_assurance_mensuel,
        taux_endettement = taux_endettement
      )
    )
}