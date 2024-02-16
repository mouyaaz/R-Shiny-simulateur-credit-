#' Title capacite_endettement
#' 
#' Cette fonction calcule la capacité d'endettement en fonction des revenus,
#' du taux d'endettement et de la durée du crédit immobilier.
#' Le taux d'intérêt varie en fonction de la durée du crédit(meilleurtaux.com)
#' Le taux d'assurance annuelle est fixé à 0.2%
#' 
#' @param revenus Le revenus mensuel
#' @param taux_dendettement Le taux d'endettement souhaité
#' @param duree L durée du crédit
#'
#' @return Une liste qui contient :
#' Durée : La durée du crédit en années
#' endettement : Le taux d'endettement maximal souhaité
#' Interet : Le taux d'intérêt annuel en fonction de la durée du crédit
#' assurance : Le taux d'assurance mensuelle (fixe de 0.2%)
#' Mensualite : La mensualité maximale autorisée (assurance inclue)
#' Capacite : La capacité d'emprunt calculée en fonction des paramètres donnés
#' @export
#'
#' @examples
#' Capcite <- capacite_endettement(revenus = 5000, taux_dendettement = 30, duree = 20)
#' print(Capcite)

capacite_endettement <- function(revenus, taux_dendettement, duree) {
  
  if (revenus < 0) {
    stop("Les revenus ne peuvent pas être négatifs.")
  }
  
  if (taux_dendettement < 0 || taux_dendettement > 100) {
    stop("Le taux d'endettement doit être entre 0 et 100(réellement entre 0 et 30).")
  }
  
  if (duree <= 0 || duree > 25) {
    stop("La durée doit être entre 0 et 25 ans.")
  }
  
  
  
  # Taux d'intérêt en fonction de la durée du crédit
  # selon meilleurtaux.com (très bon taux)
  if (duree <= 7) {
    taux_interet_annuel <- 3.46
    
  } else if (duree <= 10) {
    taux_interet_annuel <- 3.55
    
  } else if (duree <= 15) {
    taux_interet_annuel <- 4.13
    
  } else if (duree <= 20) {
    taux_interet_annuel <- 4.24
    
  } else if (duree > 20) {
    taux_interet_annuel <- 4.37
  }
  
  # Calcul des taux mensuels
  taux_interet <- taux_interet_annuel / 12 / 100
  taux_assurance <- 0.2
  taux_assurance_mens <- taux_assurance / 12 / 100
  
  # Calcul des mensualités maximales
  mensualite_maximale_avec_assurance <-
    revenus * (taux_dendettement / 100)
  mensualite_maximale_apres_assurance <-
    mensualite_maximale_avec_assurance - (mensualite_maximale_avec_assurance * taux_assurance)
  
  # Capacité d'emprunt
  capacite_emprunt <-
    mensualite_maximale_apres_assurance * (1 - (1 + taux_interet) ^ (-duree * 12)) / taux_interet
  
  sortie <- list(
    "Durée" = duree,
    "endettement" = taux_dendettement,
    "Interet" = as.numeric(taux_interet_annuel),
    "assurance" = taux_assurance,
    "Mensualite" = mensualite_maximale_avec_assurance,
    "Capacite" = capacite_emprunt
  )
  
  return(sortie)
}