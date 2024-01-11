library(shiny)
library(shinydashboard)
library(DT)
library(caret)
library(ggplot2)



# Interface ---------------------------------------------------------------

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Crédit immobilier"),  # Titre
  dashboardSidebar(
    
    # Entrées
    numericInput("emprunt", "Le montant du projet (€):",value = 100000,min = 0),
    sliderInput("duree", "Durée du crédit (années):", min = 0, max = 30, value = 15, step = 1),
    numericInput("taux_interet", "Taux d'intérêt (%):",min = 1, max = 100, value = 15 , step = 0.01 ), 
    numericInput("apport", "Le montant de l’apport personnel (€):",  step = 50,value = 10000,min = 0 ),
    numericInput("emprunteur1", "Revenu mensuel net emprunteur 1 (€):", step = 50,value = 10000,min = 0 ),
    numericInput("emprunteur2", "Revenu mensuel net emprunteur 2 (€):", step = 50, value = 10000,min = 0 ),
    numericInput("assurance", "Le taux d’assurance (%):", value = 0.25,min = 0),
    numericInput("frais", "Frais bancaires (€):", value = 1500,min = 0 ),
    numericInput("nombre_mois", "Nombre de mois:", min = 1, max = 12, value = 12),
    actionButton("calculate_btn", "Calculer"),
               
                 
   
    
    # Onglets
    sidebarMenu(
      menuItem("simulation", tabName = "simulation", icon = icon("list-alt")),
      menuItem("Tableau d'amortissement", tabName = "amortissement", icon = icon("table")),
      menuItem("Capacité d'Emprunt", tabName = "capacite_emprunt",icon = icon("calculator"),
      menuItem("Histogramme", tabName = "histogramme"))
      
      )
    
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName = "simulation",
      fluidRow(
        valueBoxOutput("taux_interet"),
        valueBoxOutput("mensualite"),
        valueBoxOutput("mensualite_sans_assurance"),
        valueBoxOutput("cout_assurances"),
        valueBoxOutput("cout_total_assurance"),
        valueBoxOutput("cout_interets"),
        valueBoxOutput("cout_total"),
        valueBoxOutput("taeg"),
        valueBoxOutput("taux_endettement")
      )
    ),
    tabItem(
      "amortissement",
      box(
        width = 12,
        title = "Tableau d'amortissement",
        DTOutput("amortissement"),
        valueBoxOutput("cout_total_"),
        valueBoxOutput("cout_interets_"),
        valueBoxOutput("cout_total_assurance_"),
        downloadButton("telecharger", "Télécharger")
      )
    ),
    tabItem(
      tabName = "capacite_emprunt",
      numericInput("revenus", "Revenus mensuel:",value = 3000, min = 0),
      numericInput("taux_endettement", "Taux d'Endettement:",value = 30,min = 1 ),
      numericInput("duree_credit", "Durée du Crédit en années (≤ 25):",value = 10,min = 1 ),
   fluidRow(
        valueBoxOutput("duree_credit"),
        valueBoxOutput("taux_endettement_cap"),
        valueBoxOutput("interet"),
        valueBoxOutput("taux_assurance_cap"),
        valueBoxOutput("mensualite_tot"),
        valueBoxOutput("capacite")
      )
    ),
   tabItem(
     tabName = "histogramme",
     box(
       width = 12,title = "Histogramme du solde restant dû",  background = "red",
       status = "primary", solidHeader = TRUE, collapsible = TRUE,
     plotOutput("distPlot"))
  )
  )
)
)
# Server ------------------------------------------------------------------

server <- function(input, output) {
  # Résumé ------------------------------------------------------------------
  
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
      
      # # Calcul du montant total à rembourser
      # montant_total_a_rembourser <- frais + (nombre_mensualites * mensualite_totale)
      # 
      # # Nouveau calcul du TAEG
      # taeg <- ((montant_total_a_rembourser - emprunt) / emprunt) * duree*12
      
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
          # taeg = 0
        )
      )
    }
  
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
  
  
  
  output$taux_interet <- renderValueBox({
    valueBox(format(paste0(input$taux_interet, " %")),"Taux d'intérêt (%)",
             icon = icon("percent"),color = "purple")
  })
  

  
  output$mensualite <- renderValueBox({
    mensualite_value <- round(donnees()$mensualite_totale, 2)
    valueBox(
      format(paste0(mensualite_value, " €")),      # Formatted monthly payment with euro symbol
      "Mensualité Totale (€)",                      # Title
      icon = icon("euro"),                          # Icon set to euro
      color = "green"                                # Color based on the threshold
    )
  })
  
  
  # 
  # output$mensualite <- renderValueBox({
  #   valueBox(format(paste0(
  #     round(donnees()$mensualite_totale, 2), " €")),"Mensualité Totale (€)",
  #     icon = icon("euro"))
  # })
  
  
  output$mensualite_sans_assurance <- renderValueBox({
    valueBox(format(paste0(
      round(donnees()$mensualite_sans_assurance, 2), " €")),
      "Mensualité sans Assurance (€)",
      icon = icon("euro"),color = "teal")
  })
  
  output$cout_assurances <- renderValueBox({
    valueBox(format(paste0(round(donnees()$cout_assurance_mensuel, 2), " €")),
             "Coût Assurance Mensuel (€)",
             icon = icon("euro"), color = "orange")
  })
  
  output$cout_total_assurance <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_amortissement()$cout_total_assurance, 2)," €")),
      "Coût total des Assurances (€)",
      icon = icon("shield"),color = "blue")
  })
  
  output$cout_interets <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_amortissement()$cout_total_interet, 2)," €")),
      "Coût Total des Intérêts (€)",
      icon = icon("euro"),color = "fuchsia")
  })
  
  output$cout_total <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_amortissement()$cout_total_credit, 2)," €")),
      "Coût Total du Crédit (€)",
      icon = icon("line-chart"))
  })
  
  output$taeg <- renderValueBox({
    taeg_value <- round(donnees_amortissement()$taeg, 3)
    
    color <- ifelse(taeg_value >= 0, "green", "red")
    
    valueBox(
      format(paste0(taeg_value, " %")),
      "TAEG (%)",
      icon = icon("balance-scale"),
      color = color
    )
  })
  
  
  output$taux_endettement <- renderValueBox({
    valueBox(format(paste0(round(donnees()$taux_endettement, 3), " %")),
             "Taux d'Endettement (%)",
             icon = icon("percent"))
  })
  
 
  
  
  # Tableau d'amortissement -------------------------------------------------
  
  #' Title calcul_tableau_amortissement
  #' 
  #' Cette fonction calcule un tableau d'amortissement pour un 
  #' prêt immobilier en se basant sur les paramètres suivants :
  #' 
  #' @param emprunt Le montant du crédit
  #' @param taux_interet Le taux annuel d'intérêt
  #' @param duree La Durée du crédit en années
  #' @param frais Les frais et tous les autres frais bancaires
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
  #'  duree = 20, frais = 2000, assurance = 0.5, apport = 20000)
  #' print(tab_amor)
  
  calcul_tableau_amortissement <-
    function(emprunt,taux_interet,duree,assurance,apport) {
      
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
        # Calcul du montamt de l'intérêt au mois en cours
        interets_mois <- capital_restant * taux_mensuel
        
        # Calcul du capital remboursé au mois en cours
        principal_mois <- mensualite_sans_assurance - interets_mois
        
        mensualite_totale <-mensualite_sans_assurance + assurance_mensuel
        
        tableau_amortissement <- rbind(tableau_amortissement,
                                       c(mois,round(capital_restant, 2),round(interets_mois, 2),
                                        round(principal_mois, 2),
                                         round(assurance_mensuel, 2),
                                         round(mensualite_totale, 2)))
                                       
        
        # Calcul du capital restant dû au mois en cours :: 
        
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
                             input$frais) 
    cout_total_assurance <- sum(as.numeric(tab_amor$Assurance))
    cout_total_interet <- sum(as.numeric(tab_amor$Interets))
    
    # Calcul du TAEG
    
    taeg <- ((cout_total_credit+input$frais) / input$emprunt - 1) / input$duree * 100
    
    return(
      list(
        cout_total_credit = round(cout_total_credit, 2),
        cout_total_assurance = round(cout_total_assurance, 2),
        cout_total_interet = round(cout_total_interet, 2),
        taeg = round(taeg, 3)
      )
    )
  })
 
  output$amortissement <- renderDT({
    calcul_tableau_amortissement(
    emprunt = input$emprunt,
    taux_interet = input$taux_interet,
    duree = input$duree,
    assurance = input$assurance,
    apport = input$apport
  )
}, extensions = "Buttons", options = list(
  lengthChange = TRUE,
  dom = "Blrtip",
  buttons = c("copy", "csv", "excel", "pdf", "print"),
  lengthMenu = list(c(-1, 10, 12, 15, 25, 50, 100), c("All", "10", "12", "15", "25", "50", "100"))
))

 
  output$cout_total_ <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_amortissement()$cout_total_credit, 2)," €")),
      "Coût Total du Crédit (€)",
      icon = icon("euro"))
  })
  
  output$cout_interets_ <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_amortissement()$cout_total_interet, 2)," €")),
      "Coût Total des Intérêts (€)",
      icon = icon("euro"))
  })
  
  output$cout_total_assurance_ <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_amortissement()$cout_total_assurance, 2)," €")),
      "Coût total des Assurances (€)",
      icon = icon("euro"))
  })
  
  # Téléchargement ----------------------------------------------------------
  
  output$telecharger <- downloadHandler(
    filename = function() {
      paste("Tableau d'amortissement-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      tab_amor <- calcul_tableau_amortissement(
        emprunt = input$emprunt,
        taux_interet = input$taux_interet,
        duree = input$duree,
        assurance = input$assurance,
        apport = input$apport
      )
      write.csv(tab_amor, file, row.names = FALSE)
    }
  )
  
  # Capacité d'emprunt ------------------------------------------------------
  
  #' Title capacite_endettement
  #' 
  #' Cette fonction calcule la capacité d'endettement en fonction des revenus,
  #' du taux d'endettement et de la durée du crédit immobilier.
  #' Le taux d'intérêt varie en fonction de la durée du crédit(meilleurtaux.com)
  #' Le taux d'assurance annuelle est fixe à 0.2%
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
    
    # Calcul des taux mensuels :
    
    taux_interet <- taux_interet_annuel / 12 / 100
    taux_assurance <- 0.2
    taux_assurance_mens <- taux_assurance / 12 / 100
    
    # Calcul des mensualités maximales :
    
    mensualite_maximale_avec_assurance <-
      revenus * (taux_dendettement / 100)
    mensualite_maximale_apres_assurance <-
      mensualite_maximale_avec_assurance - (mensualite_maximale_avec_assurance * taux_assurance)
    
    # Capacité d'emprunt : 
    
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
  
  donnees_capacite <- reactive({
    capacite_endettement(input$revenus,
                         input$taux_endettement,
                         input$duree_credit)
  })
  
  output$duree_credit <- renderValueBox({
    valueBox(format(paste0(input$duree_credit, " Années")),
             "Durée du crédit",
             icon = icon("calendar"))
  })
  
  output$taux_endettement_cap <- renderValueBox({
    valueBox(format(paste0(input$taux_endettement, " %")),
             "Taux d'endettement (%)",
             icon = icon("percent"))
  })
  
  output$interet <- renderValueBox({
    valueBox(format(paste0(donnees_capacite()$Interet, " %")),
             "Taux d'intérêt (%)",
             icon = icon("percent"))
  })
  
  output$taux_assurance_cap <- renderValueBox({
    valueBox(format(paste0(
      donnees_capacite()$assurance, " %")),
      "Taux d'assurance (%)",
      icon = icon("percent"))
  })
  
  output$mensualite_tot <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_capacite()$Mensualite, 2), " €")),
      "Mensualité totale (€)",
      icon = icon("euro"))
  })
  
  output$capacite <- renderValueBox({
    valueBox(format(paste0(
      round(donnees_capacite()$Capacite, 2), " €")),
      "Capcité d'emprunt (€)",
      icon = icon("euro"))
  })

  
  output$distPlot <- renderPlot({
    data <- calcul_tableau_amortissement(emprunt,taux_interet,duree,assurance,apport) 
    plot <- ggplot(data, aes(x = duree, y = emprunt, fill = variable)) +
      geom_bar(stat = "identity") +
      labs(title = "Distribution Plot",
           x = "X-axis Label",
           y = "Y-axis Label") +
      
      theme_minimal()  # Use your preferred theme or customize further
    
    # Print the plot
    print(plot)
  })
  
  
  
  
  
  
  
  
   
}
  
  
  
  
  
  
  


shinyApp(ui = ui, server = server)
