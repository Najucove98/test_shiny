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
                   Total_Usuarios = dplyr::n_distinct(ID),
                   .groups = "drop") |> 
  dplyr::ungroup()

# Creación de la base con las cordenadas de los poligonos de cada pais
mapa_datos <- rnaturalearth::ne_countries(scale = "medium",returnclass = "sf") |> 
  dplyr::select(iso_a2, geometry) |> 
  dplyr::left_join(cookies_paises, by = c("iso_a2" = "country")) |> 
  dplyr::filter(!is.na(Total_Visitas))

#Libreria para la shiny
# library(shiny)

#////////////////////////////////////////////////////////////////////////////////////////////////
# Mismo codigoque en el tp para crear la base para el primer grafico
# Creacion de la base para la linea temporal
datos_tiempo <- cookies |>
  dplyr::mutate(grupo = ifelse(pais == "United States", "Estados Unidos", "Otros"),
         Día = lubridate::ymd(as.character(fecha)),
         Semana = lubridate::floor_date(Día, unit = "week"),
         Mes = lubridate::floor_date(Día, unit = "month"),
         Bimestre = lubridate::floor_date(Día, unit = "bimonth"))

#////////////////////////////////////////////////////////////////////////////////////////////////
# Creación de la base de datos por visita donde se realizó una compra de los usuarios
visitas_por_id <- cookies |> 
  dplyr::filter(gasto > 0) |> 
  dplyr::group_by(ID) |> 
  dplyr::summarise(n_visitas_efectivas = dplyr::n(),
            gasto = mean(gasto),
            tiempo = mean(tiempo),
            clicks = mean(clicks),
            paginas = mean(paginas))

#////////////////////////////////////////////////////////////////////////////////////////////////
# Creación de la base de datos sobre navegador y dispositivos
cookies_navegador <- cookies |> 
  dplyr::group_by(browser, dispositivo) |> 
  dplyr::summarise(tiempo_total = sum(tiempo, na.rm = TRUE)/60, .groups = "drop") |> 
  dplyr::mutate(
    dispositivo = dplyr::case_when(
      dispositivo == "desktop" ~ "Escritorio",
      dispositivo == "mobile"  ~ "Móvil",
      dispositivo == "tablet"  ~ "Tablet"))

# Creación de la base de datos para utilizar en el treemap
cookies_treemap <- cookies_navegador |> 
  dplyr::group_by(browser) |> 
  dplyr::summarise(tiempo_total = sum(tiempo_total, na.rm = TRUE)) |> 
  dplyr::ungroup() |> 
  dplyr::rename(dispositivo = browser) |> 
  dplyr::mutate(browser = "Tiempo total (minutos)") |> 
  dplyr::bind_rows(cookies_navegador) |> 
  dplyr::mutate(etiqueta = ifelse(browser != "Tiempo total (minutos)", paste0(dispositivo, " - ", browser), dispositivo))

#///////////////////////////////////////////////////////////////////////////////////////////////////
# Creacion de la app
# Define Interfaz
interfaz <- bslib::page_navbar(
  fillable = FALSE,
  theme = bslib::bs_theme(bootswatch = "minty"), #NO FUNCIONA
  
  # Titulo de la aplicación
  title ="Algo sobre los datos de un tienda online",

  # Paneles principales
  # Pestaña Objetivo 1
  bslib::nav_panel(
    title = "Visitas",
    
    bslib::layout_sidebar(
      # Panel secundario
      sidebar = bslib::sidebar(
        title = "Panel de Control",
        position = "left",
        
        # Listado de Paises (Widget)
        shinyWidgets::pickerInput(
          inputId = "pais",                          # ID del widget
          label = "País a incluir",                  # Título a mostrar en la app
          choices = sort(unique(mapa_datos$pais)),   # Opciones disponibles
          multiple = TRUE,
          selected = sort(unique(mapa_datos$pais))[1:length(unique(mapa_datos$pais))],
          options = shinyWidgets::pickerOptions(container = "body", actionsBox = TRUE),
          width = "100%") 
      ),
      # Definicion de Outputs
      bslib::layout_columns(
        col_widths = c(6, 6),
        # Mapa interactivo
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Distribución Geográfica del Total de Visitas"),
          leaflet::leafletOutput(outputId = "map")
        ),
        # Tabla de datos sobre visitas
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Datos"),
          reactable::reactableOutput(outputId = "tabla")
        )
      )
    ),
  ),
  # Pestaña Objetivo 2
  bslib::nav_panel(
    title = "Línea temporal",
    
    bslib::layout_sidebar(
      # Panel secundario
      sidebar = bslib::sidebar(
        title = "Panel de Control",
        position = "left",
        
        # Listado de tiempo shiny::radioButtons("id9", "Opción única con listado a la vista", LETTERS[1:5])
        shiny::radioButtons(
          inputId = "tiempo",                                    # ID del widget
          label = "Tiempo",                                      # Título a mostrar en la app
          choices = colnames(datos_tiempo)[c(11, 12, 13, 14)],   # Opciones disponibles
          selected = colnames(datos_tiempo)[11])
      ),
      # Definicion de Outputs
      bslib::layout_columns(
        col_widths = c(6, 6),
        # Mapa interactivo
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Evolución del gasto total a traves del tiempo"),
          echarts4r::echarts4rOutput(outputId = "linea"),
        ),
        # Tabla de datos sobre visitas <- podriamos agregar estadisticas sobre las poblaciones
        # bslib::card(
        #   full_screen = TRUE,
        #   bslib::card_header("Datos"),
        #   reactable::reactableOutput(outputId = "tabla")
        # )
      )
    ),
  ),
  # Pestaña Objetivo 3
  bslib::nav_panel(
    title = "Tendencia de compra",
    
    bslib::layout_sidebar(
      # Panel secundario
      sidebar = bslib::sidebar(
        title = "Panel de Control",
        position = "left",
        
        # Listado de tiempo
        shinyWidgets::sliderTextInput(
          inputId = "tiempo",                                    # ID del widget
          label = "Tiempo",                                      # Título a mostrar en la app
          choices = colnames(datos_tiempo)[c(11, 12, 13, 14)],   # Opciones disponibles
          selected = colnames(datos_tiempo)[11])
      ),
      # Definicion de Outputs
      bslib::layout_columns(
        col_widths = c(6, 6),
        # Mapa interactivo
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Relación Gasto Promedio vs. Frecuencia de Compra"),
          plotly::plotlyOutput(outputId = "relacion"),
        )
      )
    )
  ),
  # Pestaña Objetivo 4
  bslib::nav_panel(
    title = "Tiempo",
    
    bslib::layout_sidebar(
      # Panel secundario
      sidebar = bslib::sidebar(
        title = "Panel de Control",
        position = "left",
        
        # Listado de tiempo
        shinyWidgets::sliderTextInput(
          inputId = "tiempo",                                    # ID del widget
          label = "Tiempo",                                      # Título a mostrar en la app
          choices = colnames(datos_tiempo)[c(11, 12, 13, 14)],   # Opciones disponibles
          selected = colnames(datos_tiempo)[11])
      ),
      # Definicion de Outputs
      bslib::layout_columns(
        col_widths = c(6, 6),
        # Mapa interactivo
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Distribución del Tiempo total por Navegador y Dispositivo"),
          plotly::plotlyOutput(outputId = "tiempo_gastado"),
        )
      )
    )
  ),
  bslib::nav_spacer(), # QUE HACE?
  
  bslib::nav_item(shiny::a("Tienda", href = "https://shop.merch.google/", target = "_blank"))
)

# Define Servidor
servidor <- function(input, output) {
  
  # Objetivo 1
  # Objeto reactivo segun la seleccion del usuario
  mapa_datos_reac <- shiny::reactive({
    mapa_datos |> 
    dplyr::filter(pais %in% input$pais)
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
        leaflet::setView(lng = 0, lat = 20, zoom = 1.4) |> 
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
          position = "bottomright")
  })
  
  output$tabla <- reactable::renderReactable({
    mapa_datos_reac()[ , c(2,3,4)] |>
      dplyr::arrange(dplyr::desc(Total_Visitas)) |>
      dplyr::rename("País" = pais,
                    "Total Visitas" = Total_Visitas,
                    "Total Usuarios" = Total_Usuarios) |>
      sf::st_drop_geometry() |> 
      reactable::reactable()
  })
  
  # Objetivo 2
  # Objeto reactivo segun la seleccion del usuario
  datos_tiempo_reac <- shiny::reactive({
    datos_tiempo |> 
    dplyr::group_by(dplyr::sym(input$tiempo), grupo) |>
    dplyr::summarise(gasto = sum(gasto, na.rm = TRUE), .groups = "drop")
  })
  
  
  output$linea <- echarts4r::renderEcharts4r({
    
    datos_tiempo_reac() |> 
      dplyr::group_by(grupo) |>  
      echarts4r::e_charts(x = dplyr::sym(input$tiempo)) |>
      echarts4r::e_line(serie = gasto) |>
      echarts4r::e_title("Evolución del gasto total") |> 
      echarts4r::e_x_axis(name = input$tiempo, serie = dplyr::sym(input$tiempo)) |> 
      echarts4r::e_y_axis(name = "Gasto (en USD)") |> 
      echarts4r::e_legend(orient = "vertical", left = "10%", top = "10%")  |> 
      echarts4r::e_tooltip(trigger = "axis")
  })
  
  # Objetivo 3
  output$relacion <- plotly::renderPlotly({
    
    # Gráfico de dispersión de los valores promedio del gasto por usuario vs número de visitas efectivas
    grafico <- ggplot2::ggplot(data = visitas_por_id) +
      ggplot2::aes(x = n_visitas_efectivas, 
          y = gasto,
          text = paste0(
            "<b>Nº de Visitas Efectivas: </b>", n_visitas_efectivas, "<br>",
            "<b>Gasto Promedio: </b>", round(gasto, 2), " USD <br>",
            "<b>Tiempo promedio: </b>", round(tiempo/60, 2), " min <br>",
            "<b>Clicks promedio: </b>", round(clicks), "<br>",
            "<b>Paginas promedio: </b>", round(paginas))) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Nº de Visitas Efectivas",
           y = "Gasto Promedio por Visita (USD)",
           title = "Relación Gasto Promedio vs. Frecuencia de Compra") +
      ggplot2::theme_bw()
    
    # Gráfico de dispersión dinámico con la librería plotly
    plotly::ggplotly(grafico + 
                       ggplot2::scale_x_continuous(breaks = seq(0, 30, 5)) +
                       ggplot2::scale_y_continuous(breaks = seq(0, 8000, 1000)),
                     tooltip = "text")
  })
  
  # Objetivo 4
  output$tiempo_gastado <- plotly::renderPlotly({
    
    # Creación del treemap
    cookies_treemap |> 
      plotly::plot_ly(
        type = "treemap",
        labels = ~etiqueta,
        parents = ~ifelse(dispositivo == "Tiempo total (minutos)", "", browser),
        values = ~tiempo_total,
        hoverinfo = "label+value+percent parent+percent root",
        textinfo = "label+value+percent parent+percent root") |> 
      plotly::add_trace(branchvalues = "total", name = "")
  })
}

# Run the application 
shinyApp(ui = interfaz, server = servidor)
