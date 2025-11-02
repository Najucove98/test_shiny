# Librerías para manipular los datos
# library(dplyr)
# library(lubridate)
# library(countrycode)

# Librerías para graficar
# library(ggplot2)
# library(leaflet.extras) # Mapa
# library(echarts4r) # Linea temporal 
# library(plotly) # Gráfico de dispersión y treemap

# Base de datos con los polígonos para hacer mapa
# library(rnaturalearth)
# 
# # Para control de titulos
# library(htmltools)

# Elimina la notación cientifica
options(scipen = 999)

#////////////////////////////////////////////////////////////////////////////////////////////////
# Mismo codigoque en el tp para crear la base para el primer grafico
# Carga de los datos
cookies <- read.delim("cookies.txt") # cookies.txt en la misma carpeta con al app

# Base para el primer objetivo
cookies_paises <- cookies |> 
  dplyr::mutate(country = countrycode::countrycode(sourcevar = pais,
                                                   origin = "country.name.en",
                                                   destination = "iso2c",
                                                   nomatch = NA_character_)) |> 
  dplyr::filter(!is.na(country)) |> 
  dplyr::group_by(country, pais) |> 
  dplyr::summarise(Total_Visitas = dplyr::n(),
                   Total_Usuarios = dplyr::n_distinct(ID))

# Creación de la base con las cordenadas de los poligonos de cada pais
mapa_datos <- rnaturalearth::ne_countries(scale = "medium",returnclass = "sf") |> 
  dplyr::select(iso_a2, geometry) |> 
  dplyr::left_join(cookies_paises, by = c("iso_a2" = "country")) |> 
  dplyr::filter(!is.na(Total_Visitas))

#Libreria para la shiny
# library(shiny)

#///////////////////////////////////////////////////////////////////////////////////////////////////
# Creacion de la app
# Define Interfaz
interfaz <- bslib::page_fluid(
  
  # Titulo de la aplicación
  shiny::titlePanel("Distribución geográfica de las visitas"),

  # Función para crear paneles 
  bslib::layout_sidebar(
    # Panel secundario
    sidebar = bslib::sidebar(
      # Listado de Paises (Widget)
      shinyWidgets::pickerInput(
        inputId = "pais",                          # ID del widget
        label = "País a incluir",                  # Título a mostrar en la app
        choices = sort(unique(mapa_datos$pais)),   # Opciones disponibles
        multiple = TRUE,
        selected = sort(unique(mapa_datos$pais))[1:length(unique(mapa_datos$pais))],
        options = pickerOptions(container = "body",
                                actionsBox = TRUE),
        width = "100%")
      # SE PODRIA PONER OTRO QUE SELECCIONE EL CONTINENTE
    ),
    
    # Panel principal
    leaflet::leafletOutput("map")     # Definicion del tipo de Outputs (map = mapas)
    #PODRIAMOS AGREGAR UNA TABLA TAMBIEN CON ESTADISTICAS PARA ACOMPAÑAR EL MAPA
  )
)

# Define Servidor
servidor <- function(input, output) {
  
  # Objeto reactivo segun la seleccion del usuario
  mapa_datos_reac <- shiny::reactive({
    dplyr::filter(mapa_datos, pais %in% input$pais)
  })
  
  output$map <- leaflet::renderLeaflet({ # Cambia segun el tipo de Outputs
    
    # Paleta de colores a utilizar (Cambia segun cambia la base reactiva)
    paleta_colores <- leaflet::colorNumeric(
      palette = "YlOrRd",
      domain = mapa_datos_reac()$Total_Visitas)
    
    # Generacion del mapa con la libreria leaflet
    # Mismo codigo que en el tp solo agregue librerias para cada funcion y
    # agregue la base de datos que es reactiva en la app
    mapa_datos_reac() |>                # Aca usamos el elemento reactivo
        leaflet::leaflet() |> 
        leaflet::setView(lng = 0, lat = 20, zoom = 1) |> 
        leaflet::addProviderTiles('OpenStreetMap.Mapnik') |>
        leaflet::addPolygons(
          fillColor = ~paleta_colores(Total_Visitas),
          weight = 1,
          opacity = 1,
          color = "grey",
          dashArray = "1",
          fillOpacity = 0.7,
          label = ~lapply(as.list(paste0("País: ", pais, 
                                         "<br>Total Visitas: ", Total_Visitas, 
                                         "<br>Total Usuarios: ", Total_Usuarios)), HTML)) |>
        leaflet::addLegend(
          pal = paleta_colores,
          values = ~Total_Visitas,
          opacity = 0.6,
          title = "Total de Visitas",
          position = "bottomright") |> 
        leaflet::addControl(
          html = htmltools::tags$div(
            style = "font-size: 20px; 
               font-weight: bold; 
               background-color: white; 
               padding: 6px; 
               border-radius: 4px;",
            "Distribución Geográfica del Total de Visitas"), 
          position = "topright")
    })
}

# Run the application 
shinyApp(ui = interfaz, server = servidor)
