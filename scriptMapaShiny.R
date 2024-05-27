dados <- read.csv("datasets/2021/dataset_merged_2021.csv")
library(leaflet)
library(shiny)

# UI

dados$ano_mes_numerico <- as.numeric(gsub("-", "", dados$ano_mes))

ui <- fluidPage(
  titlePanel("Mapa Interativo de Internações"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("mes",
                  "Selecione o mês:",
                  min = min(dados$ano_mes_numerico),
                  max = max(dados$ano_mes_numerico),
                  value = min(dados$ano_mes_numerico),
                  step = 1)
    ),
    mainPanel(
      leafletOutput("mapa")
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
    subset_dados <- subset(dados, ano_mes_numerico == input$mes)
    
    raios_normalizados <- normaliza_raio(subset_dados[["diferenca"]])
    
    mapa <- leaflet() %>%
      addTiles() %>%
      setView(lng = -53.0, lat = -29.0, zoom = 6)
    
    mapa <- mapa %>%
      addCircleMarkers(lng = subset_dados[["longitude"]],
                       lat = subset_dados[["latitude"]],
                       radius = raios_normalizados,
                       popup = paste("Cidade:", subset_dados[["municipio"]], "<br>",
                                     "Internações:", subset_dados[["diferenca"]]))
    
    mapa
  })
}

# RODANDO

shinyApp(ui = ui, server = server)
