library(leaflet)
library(shiny)

# Carregar o csv
dados <- read.csv("datasets/2021/datasetFinal.csv")

# UI

ui <- fluidPage(
  titlePanel("Mapa Interativo de Internações"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("mes",
                  "Selecione o mês:",
                  min = min(dados$mes),
                  max = max(dados$mes),
                  value = min(dados$mes),
                  step = 1)
    ),
    mainPanel(
      leafletOutput("mapa", width = "100%", height = "900px")
    )
  )
)

# SERVER 

server <- function(input, output) {
  
  normaliza_raio <- function(diferenca) {
    max_raio <- 20
    min_raio <- 2
    
    if (max(diferenca) == min(diferenca)) {
      return(rep(min_raio, length(diferenca)))
    } else {
      return((diferenca - min(diferenca)) / (max(diferenca) - min(diferenca)) * (max_raio - min_raio) + min_raio)
    }
  }
  
  output$mapa <- renderLeaflet({
    subset_dados <- subset(dados, mes == input$mes)
    
    raios_normalizados <- normaliza_raio(subset_dados[["diferenca"]])
    
    mapa <- leaflet() %>%
      addTiles() %>%
      setView(lng = -53.0, lat = -29.0, zoom = 6)
    
    mapa <- mapa %>%
      addCircleMarkers(
        lng = subset_dados[["longitude"]],
        lat = subset_dados[["latitude"]],
        radius = raios_normalizados,
        popup = paste("Cidade:", subset_dados[["municipio"]], "<br>",
                      "Internações:", subset_dados[["diferenca"]], "<br>",
                      "/1000 habitantes: ", subset_dados[["diferenca_por_1000__habitantes"]])
      )
    
    mapa
  })
}

# RODANDO

shinyApp(ui = ui, server = server)
