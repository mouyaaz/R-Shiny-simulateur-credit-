# Télécharger le package "shiny.i18n" avant d'exécuter
# install.packages("shiny.i18n")

library(DT)
library(shiny.i18n)
library(shiny)
library(shinydashboard)


# Lapplication est disponible français et anglais

i18n <- Translator$new(translation_json_path = "traduction.json")
i18n$set_translation_language("fr") # fr pour français
#i18n$set_translation_language("en") # en pour anglais


source("UI.R", encoding = "UTF-8")
source("Server.R", encoding = "UTF-8")

shinyApp(ui = ui, server = server)

