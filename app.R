
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
      plotOutput("diamondsPlot"),
      DTOutput("diamonds_table")
        )
    )
)


server <- function(input, output, session) {
  data_filtered <- reactive({
    diamonds %>%
      filter(color == input$filtre_couleurs, price <= input$prix) %>%
      select(carat, cut, color, clarity, depth, table, price)
  })
  output$diamondsPlot <- renderPlot({
    ggplot(data_filtered(), aes(x = carat, y = price)) +
      geom_point(color = ifelse(input$couleur_rose == "Oui", "#f3969a", "#5a5a5a")) +
      labs(
        title = glue("prix : {input$prix} & color: {input$filtre_couleurs}"),
        x = "carat",
        y = "price"
      ) 
})}

# Run the application 
shinyApp(ui = ui, server = server)
