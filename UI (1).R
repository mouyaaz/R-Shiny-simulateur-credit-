ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = i18n$t("Crédit immobilier")),  # Titre
                    dashboardSidebar(
                      
                      h4(tags$a(href = "https://github.com/mouyaaz", "Mustapha OUYAAZ")),
                      h4(tags$a(href = "https://github.com/taphakh", "Moustapha KHATTARY")),
                      
# Onglets -----------------------------------------------------------------
                      
                      
                      sidebarMenu(
                        menuItem(i18n$t("Résumé"), tabName = "resume",
                                 icon = icon("list-alt")),
                        
                        menuItem(i18n$t("Tableau d'amortissement"),
                                 tabName = "amortissement",
                                 icon = icon("table")),
                        
                        menuItem(i18n$t("Capacité d'emprunt"),
                                 tabName = "capacite_emprunt",
                                 icon = icon("calculator"))
                        
                      ),

# Entrées -----------------------------------------------------------------

                      
                      numericInput("emprunt", i18n$t("Montant de l'emprunt:"),
                                   value = 10000,
                                   min = 0
                                   
                      ),
                      sliderInput("duree", i18n$t("Durée du crédit en années:"),
                                  value = 20,
                                  min = 1,
                                  max = 30
                      ),
                      numericInput("taux_interet", i18n$t("Taux d'intérêt (%):"),
                                   value = 1,
                                   min = 0,
                                   max=100,
                                   step = 0.01
                      ),
                      numericInput("assurance", i18n$t("Taux d'assurance (%):"),
                                   value = 0.25,
                                   min = 0,
                                   max=100,
                                   step = 0.01
                      ),
                      numericInput("frais", i18n$t("Montant des frais:"),
                                   value = 2000,
                                   min = 0
                      ),
                      numericInput("apport", i18n$t("Montant de l'apport personnel:"),
                                   value = 0,
                                   min = 0
                      ),
                      numericInput("emprunteur1", i18n$t("Les revenus de l'emprunteur 1:"),
                                   value = 3000,
                                   min = 0
                      ),
                      numericInput("emprunteur2", i18n$t("Les revenus de l'emprunteur 2:"),
                                   value = 3000,
                                   min = 0
                      )
                    ),



# Résumé ------------------------------------------------------------------


                    dashboardBody(tabItems(
                      tabItem(
                        tabName = "resume",
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

# Tableau d'amortissement -------------------------------------------------

                      
                      tabItem(
                        "amortissement",
                        box(
                          width = 12,
                          title = i18n$t("Tableau d'amortissement"),
                          DTOutput("amortissement"),
                          valueBoxOutput("cout_total_"),
                          valueBoxOutput("cout_interets_"),
                          valueBoxOutput("cout_total_assurance_")
                        )
                      ),

# Capacité d'emprunt ------------------------------------------------------

                      
                      tabItem(
                        tabName = "capacite_emprunt",
                        numericInput("revenus", i18n$t("Revenus mensuel:"),
                                     value = 3000,
                                     min = 0
                        ),
                        numericInput("taux_endettement", i18n$t("Taux d'Endettement:"),
                                     value = 30,
                                     min = 1,
                                     step = 0.01
                        ),
                        numericInput("duree_credit", i18n$t("Durée du Crédit en années (≤ 25):"),
                                     value = 10,
                                     min = 1
                        ),
                        fluidRow(
                          valueBoxOutput("duree_credit"),
                          valueBoxOutput("taux_endettement_cap"),
                          valueBoxOutput("interet"),
                          valueBoxOutput("taux_assurance_cap"),
                          valueBoxOutput("mensualite_tot"),
                          valueBoxOutput("capacite")
                        )
                      )

                     )
                    )
)
