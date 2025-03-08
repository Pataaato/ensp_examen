
library(shiny)
library(usethis)
library(dplyr)
library(ggplot2)
library(glue)
library(DT)
library(bslib)
library(thematic)
library(shinylive)
library(rsconnect)

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
        selected = "Oui"
        ), 
      
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
      geom_point(color = ifelse(input$couleur_rose == "Oui", "pink", "#5a5a5a")) +
      labs(
        title = glue("prix: {input$prix} & color: {input$filtre_couleurs}"),
        x = "carat",
        y = "price") +
      theme_minimal()+
      theme(
        text = element_text(family = "Monaco", size = 12),
        title = element_text(family = "Monaco", color = "#5a5a5a", size = 11),  
        axis.text = element_text(family = "Monaco", color = "#909090", size = 9),  
        axis.title = element_text(family = "Monaco", color = "#5a5a5a", size = 11),  
        axis.ticks = element_line(color = "#7a7a7a"),  
        panel.background = element_rect(fill = "#f3f3f3", color = NA),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        panel.grid.major = element_line(color = "#ffffff", size = 0.5),
        panel.grid.minor = element_line(color = "#ffffff", size = 0.3)) 
        
    
  })
  output$diamonds_table <- renderDT({
    datatable(data_filtered())
  })
  
  observeEvent(input$boutton, {
    showNotification(glue("prix: {input$prix} & color: {input$filtre_couleurs}"), type = "message")
  })
}

shinyApp(ui = ui, server = server)