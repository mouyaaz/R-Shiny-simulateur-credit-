
# Appel des fonctions utilisées
source("calcul_mensualite.R", encoding = "UTF-8")
source("calcul_tableau_amortissement.R", encoding = "UTF-8")
source("capacite_endettement.R", encoding = "UTF-8")



server <- function(input, output) {
  
  # Résumé ------------------------------------------------------------------
  
  
  donnees <- reactive({
    calcul_mensualite(
      emprunt = input$emprunt,
      taux_interet = input$taux_interet,
      duree = input$duree,
      frais = input$frais,
      assurance = input$assurance,
      apport = input$apport,
      revenus1 = input$emprunteur1,
      revenus2 = input$emprunteur2
    )
  })
  
  ## Affichage du taux d'intérêt ##
  output$taux_interet <- renderValueBox({
    valueBox(format(paste0(input$taux_interet, " %")),i18n$t("Taux d'intérêt (%)"),
             icon = icon("percent"),color = "purple")
  })
  
  ## Affichage de la mensualité totale ##
  output$mensualite <- renderValueBox({
    valueBox(format(paste0(
      round(donnees()$mensualite_totale, 2), " €")),i18n$t("Mensualité Totale (€)"),
      icon = icon("euro"),color = "green")
  })
  
  ## Affichage de la mensualité sans assurance ##
  output$mensualite_sans_assurance <- renderValueBox({
    valueBox(format(paste0(
      round(donnees()$mensualite_sans_assurance, 2), " €")),
      i18n$t("Mensualité sans Assurance (€)"),
      icon = icon("euro"),color = "green")
  })
  
  ## Affichage du coût mensuel de l'assurance ##
  output$cout_assurances <- renderValueBox({
    valueBox(format(paste0(round(donnees()$cout_assurance_mensuel, 2), " €")),
             i18n$t("Coût Assurance Mensuel (€)"),
             icon = icon("shield"),color = "blue")
  })
  
  ## Affichage du coût total de l'assurance ##
  output$cout_total_assurance <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_amortissement()$cout_total_assurance, 2)," €")),
      i18n$t("Coût total des Assurances (€)"),
      icon = icon("shield"),color = "blue")
  })
  
  ## Affichage du coût total des intérêt ##
  output$cout_interets <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_amortissement()$cout_total_interet, 2)," €")),
      i18n$t("Coût Total des Intérêts (€)"),
      icon = icon("euro"),color = "green")
  })
  
  ## Affichage du coût total ##
  output$cout_total <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_amortissement()$cout_total_credit, 2)," €")),
      i18n$t("Coût Total du Crédit (€)"),
      icon = icon("euro"),color = "green")
  })
  
  ## Affichage du TAEG ##
  output$taeg <- renderValueBox({
    valueBox(format(paste0(round(donnees_amortissement()$taeg, 3), " %")),i18n$t("TAEG (%)"),
             icon = icon("percent"),color = "red")
  })
  
  ## Affichage du taux d'endettement ##
  output$taux_endettement <- renderValueBox({
    valueBox(format(paste0(round(donnees()$taux_endettement, 3), " %")),
             i18n$t("Taux d'Endettement (%)"),
             icon = icon("percent"),
             color <- ifelse(donnees()$taux_endettement <= 30, "green", "red"))
  })
  
  
  # Tableau d'amortissement -------------------------------------------------
  
  ## Calcul du tableau d'amortissement ##
  donnees_amortissement <- reactive({
    tab_amor <- calcul_tableau_amortissement(
      emprunt = input$emprunt,
      taux_interet = input$taux_interet,
      duree = input$duree,
      assurance = input$assurance,
      apport = input$apport
    )
    
    # Calcul des coûts totaux du crédit, assurance et des intérêt
    cout_total_credit <- sum(as.numeric(tab_amor$Interets), 
                             as.numeric(tab_amor$Assurance),
                             input$emprunt,
                             input$frais) 
    cout_total_assurance <- sum(as.numeric(tab_amor$Assurance))
    cout_total_interet <- sum(as.numeric(tab_amor$Interets))
    
    # Calcul du TAEG
    taeg <- ((cout_total_credit / input$emprunt)**(1/input$duree) - 1) * 100
    
    return(
      list(
        cout_total_credit = round(cout_total_credit, 2),
        cout_total_assurance = round(cout_total_assurance, 2),
        cout_total_interet = round(cout_total_interet, 2),
        taeg = round(taeg, 3)
      )
    )
  })
  
  # Affichage du tableau d'amortissement
  output$amortissement <- renderDT({
   tableau <- calcul_tableau_amortissement(
      emprunt = input$emprunt,
      taux_interet = input$taux_interet,
      duree = input$duree,
      assurance = input$assurance,
      apport = input$apport
    )
   colnames(tableau) <- c(i18n$t("Mois"),
                          i18n$t("Capital restant dû"),
                          i18n$t("Interets"),
                          i18n$t("Principal"),
                          i18n$t("Assurance"),
                          i18n$t("Mensualite"))
   return(tableau)
  }, extensions = "Buttons", options = list(
    lengthChange = TRUE,
    dom = "Blrtip",
    buttons = c("copy", "csv", "excel", "pdf", "print"),
    lengthMenu = list(c(12, 25, 50, 100, -1),
                      c("12", "25", "50", "100", "All"))
  ))
  
  ## Affichage du coût total ##
  output$cout_total_ <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_amortissement()$cout_total_credit, 2)," €")),
      i18n$t("Coût Total du Crédit (€)"),
      icon = icon("euro"))
  })
  
  ## Affichage du coût total des interets##
  output$cout_interets_ <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_amortissement()$cout_total_interet, 2)," €")),
      i18n$t("Coût Total des Intérêts (€)"),
      icon = icon("euro"))
  })
  
  ## Affichage du coût total de l'assurance ##
  output$cout_total_assurance_ <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_amortissement()$cout_total_assurance, 2)," €")),
      i18n$t("Coût total des Assurances (€)"),
      icon = icon("euro"))
  })
  
  # Capacité d'emprunt ------------------------------------------------------
  
  ## Calcul des indicateurs de lancapacité d'emprunt ##
  donnees_capacite <- reactive({
    capacite_endettement(input$revenus,
                         input$taux_endettement,
                         input$duree_credit)
  })
  
  ## Affichage de la durée du crédit ##
  output$duree_credit <- renderValueBox({
    valueBox(format(paste0(input$duree_credit, i18n$t(" Années"))),
             i18n$t("Durée du crédit"),
             icon = icon("calendar"))
  })
  
  ## Affichage du taux d'endettement ##
  output$taux_endettement_cap <- renderValueBox({
    valueBox(format(paste0(input$taux_endettement, " %")),
             i18n$t("Taux d'endettement (%)"),
             icon = icon("percent"),
             color = "purple")
  })
  
  ## Affichage du taux d'interet ##
  output$interet <- renderValueBox({
    valueBox(format(paste0(donnees_capacite()$Interet, " %")),
             i18n$t("Taux d'intérêt (%)"),
             icon = icon("percent"))
  })
  
  ## Affichage du taux d'assurance ##
  output$taux_assurance_cap <- renderValueBox({
    valueBox(format(paste0(
      donnees_capacite()$assurance, " %")),
      i18n$t("Taux d'assurance (%)"),
      icon = icon("percent"))
  })
  
  ## Affichage de la mensualité totale ##
  output$mensualite_tot <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_capacite()$Mensualite, 2), " €")),
      i18n$t("Mensualité totale (€)"),
      icon = icon("euro"))
  })
  
  ## Affichage de la capacité d'emprunt ##
  output$capacite <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_capacite()$Capacite, 2), " €")),
      i18n$t("Capcité d'emprunt (€)"),
      icon = icon("euro"),
      color = "teal")
  })
}
