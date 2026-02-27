# app/view/dashboard.R
# migrate a future version to bslib: https://appsilon.com/shiny-application-layouts/

# try this for select-able modules: https://stackoverflow.com/questions/62528413/how-to-call-different-modules-based-on-a-select-input-in-shiny
# i MIGHT be able to unify echarts filtering with this but module interaction unclear: https://echarts4r.john-coene.com/articles/connect


box::use(
  dplyr[...],
  shiny[observe, h3, br, moduleServer, NS, fluidPage, fluidRow, reactive, reactiveValues, renderText, textOutput, tagList],
  bslib[navset_card_tab,card,sidebar,page_sidebar,page_navbar,page_fluid,layout_columns,layout_column_wrap,value_box,navset_tab,nav_panel,value_box_theme],
  htmltools[css],
  glue[glue],
  sf[st_as_sf],
  leaflet[leaflet, leafletProxy, leafletOutput, clearMarkers, clearMarkerClusters, clearShapes, addProviderTiles, addPolygons, colorNumeric, colorQuantile, addLegend, renderLeaflet]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  leafletOutput(ns("map"), width="100%", height="100%")

}

#' @export
server <- function(id, dat, params, coords, outline) {

  moduleServer(id, function(input, output, session) {

    dat_i = reactive({
      print(params$date)
      dat() %>% filter(date == params$date) %>% glimpse() %>% as.data.frame() %>% st_as_sf()
    })

    cols = reactive({
      colorNumeric("plasma", dat()$pred)
    })

    # Change to raster later on for performance: https://rstudio.github.io/leaflet/articles/raster.html

    basemap = reactive({
      leaflet() %>%
        addProviderTiles('CartoDB.Positron') %>%
        #setting polygons here will just center the view on their bounds (observeEvent immediately clears them)
        addPolygons(data = dat_i(),
                    stroke = F, smoothFactor = 0.1, fillOpacity = 0.5, fillColor = ~cols()(dat_i()$pred)) %>%
        addPolygons(data = outline,
                    stroke = T, color = 'steelblue', fill = F) %>%
        addLegend('bottomright', pal = cols(), values = dat_i()$pred)
    })


    # Get click coord selection working

    observe({
      click = input$map_click
      if(is.null(click)) {return()}
      coords$lat = click$lat
      coords$long = click$lng
      print(coords)
      print(coords$lat)
      leafletProxy("map")
    })

    output$map = renderLeaflet({
      basemap()
    })

    return(coords)



  })

}
