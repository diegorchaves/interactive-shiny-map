library(leaflet)
library(shiny)

# Carregar o csv
dados <- read.csv("./datasetDefinitivo.csv")

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
server <- function(input, output, session) {
  
  normaliza_raio <- function(diferenca) {
    max_raio <- 20
    min_raio <- 2
    
    if (max(diferenca) == min(diferenca)) {
      return(rep(min_raio, length(diferenca)))
    } else {
      return((diferenca - min(diferenca)) / (max(diferenca) - min(diferenca)) * (max_raio - min_raio) + min_raio)
    }
  }
  
  # Variável reativa para armazenar o estado do mapa
  estado_mapa <- reactiveValues(lng = -53.0, lat = -29.0, zoom = 6)
  
  output$mapa <- renderLeaflet({
    subset_dados <- subset(dados, mes == input$mes)
    raios_normalizados <- normaliza_raio(subset_dados[["diferenca"]])
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = estado_mapa$lng, lat = estado_mapa$lat, zoom = estado_mapa$zoom) %>%
      addCircleMarkers(
        lng = subset_dados[["longitude"]],
        lat = subset_dados[["latitude"]],
        radius = raios_normalizados,
        popup = paste("Cidade:", subset_dados[["municipio"]], "<br>",
                      "Internações no mês:", subset_dados[["diferenca"]], "<br>",
                      "A cada 1000 habitantes: ", subset_dados[["internacoes_por_1000"]])
      )
  })
  
  observeEvent(input$mes, {
    # Atualizar o estado do mapa
    if (!is.null(input$mapa_center)) {
      estado_mapa$lng <- input$mapa_center$lng
      estado_mapa$lat <- input$mapa_center$lat
      estado_mapa$zoom <- input$mapa_zoom
    }
    
    subset_dados <- subset(dados, mes == input$mes)
    raios_normalizados <- normaliza_raio(subset_dados[["diferenca"]])
    
    leafletProxy("mapa", session) %>%
      clearMarkers() %>%
      setView(lng = estado_mapa$lng, lat = estado_mapa$lat, zoom = estado_mapa$zoom) %>%
      addCircleMarkers(
        lng = subset_dados[["longitude"]],
        lat = subset_dados[["latitude"]],
        radius = raios_normalizados,
        popup = paste("Cidade:", subset_dados[["municipio"]], "<br>",
                      "Internações no mês:", subset_dados[["diferenca"]], "<br>",
                      "A cada 1000 habitantes: ", subset_dados[["internacoes_por_1000"]])
      )
  })
}

# RODANDO
shinyApp(ui = ui, server = server)
