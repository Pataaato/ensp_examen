
library(shiny)
library(usethis)
library(dplyr)
library(ggplot2)
library(glue)
library(DT)
library(bslib)
library(thematic)

data("diamonds")
thematic_shiny(font = "auto")
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  h2("Exploration des Diamants"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "couleur_rose", 
        label = "Colorier les points en rose ?",
        choices = c("Oui", "Non"), 
        selected = "Oui", 
        inline = TRUE), 
      selectInput(
        inputId = "filtre_couleurs",
        choices = c("D", "E", "F", "G", "H", "I", "J"),
        label = "Choisir une couleur à filtrer :"), 
      sliderInput(
        inputId = "prix",
        label = "Prix maximum :",
        min = 300,
        max = 20000,
        value = 5000), 
      actionButton(
        inputId = "boutton", 
        label = "Afficher une notification"
      )),
        mainPanel(
           plotOutput("distPlot")
        )
    )
)


server <- function(input, output) {

    output$distPlot <- renderPlot({
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
