box::use(
  # LIBRARIES
  dplyr[...],
  shiny[div, moduleServer, NS, includeCSS, renderUI, HTML, h4, tags, tagList, uiOutput, reactive, reactiveValues, absolutePanel, navbarPage, tabPanel],
  shinythemes[shinytheme],
  bslib[bs_theme, page_fluid, page_navbar, card, card_header, card_body, font_google],
  waiter[autoWaiter,useWaiter,waiterShowOnLoad,spin_ellipsis,spin_flower,waiter_hide,waiterPreloader],
  arrow[read_parquet],
  sf[st_as_sf, st_set_crs],
  lubridate[today],
  tictoc[tic, toc],
  systemfonts,
  shinydisconnect,
  # MODULES
  app/view/panel,
  app/view/map,
  app/logic/utils[get_preds, get_dat]
)

# options(shiny.usecairo=TRUE)

systemfonts$scan_local_fonts()

loading_screen = tagList(
  spin_ellipsis(),
  # Other spinners: https://waiter.john-coene.com/#/
  h4(tags$p("Initializing map...", style="font-family: 'Lato' !important; color:white"))
)

#' @export
ui <- function(id) {

  ns <- NS(id)
  page_fluid(

  useWaiter(),
  waiterPreloader(html=loading_screen, color="#325178"),
  shinydisconnect$disconnectMessage(),

  #page_navbar(
  #navbarPage(

    #theme = 'flatly',
    theme = bs_theme(
      version = 5,
      preset = 'flatly'
      # heading_font = font_google('Lexend Deca'),
      # base_font = font_google('Lexend Deca', wght = "300",)
    ),
    collapsible = TRUE,
    #HTML('<a style="text-decoration:none;cursor:default;color:#4682b4;" class="active" href="#">HLB+ mapper</a>'), id="nav",
    windowTitle = "Forecast demo",
    # Navbar page( /page_navbar... or not?

    ## card(   card_body(   leaflet output
    div(class="outer",
        tags$head(includeCSS("app/styles/old_styles.css")),
    map$ui(ns('map')),

    panel$ui(ns('panel'))

    ## absolute panel (layout_sidebar? or no)
    # absolutePanel(id = "controls", class = "panel panel-default",
    #               top = 75, left = 55, width = 250, fixed=TRUE,
    #               draggable = TRUE, height = "auto",
    #
    #
    #
    # )

  )
 # )

  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Import data/model, tiles, tigris shapefiles
    state_list = read_parquet('app/data/state_anchors.parquet') %>% pull(state) %>% sort()
    grid = readRDS('app/data/tiles.rds') %>% st_set_crs(4326)
    outline = readRDS('app/data/outline.rds') %>% st_set_crs(4326)
    default_state = 'iowa'

    # Initialize control panel controls/info
    # base_params = list(date = , key = NA) # get_params(fp)
    params = reactiveValues(
      date = today(),
      state = default_state,
      default_state = default_state,
      state_list = state_list
    )

    # Make it reactive for future-proofing. Keep separate for binding right before predict so future iterations can use different species
    dat = reactive({
      o = get_dat(state = params$state) %>%
        #get_preds() %>%
        left_join(grid) # %>% glimpse()
        # st_as_sf() %>%
      o
    })

    coords = reactiveValues(
      lat = NA,
      long = NA
    )

    # Pass data to map widget
    coords = map$server("map", dat = dat, params = params, coords = coords, outline = outline)

    # Now make them edit-able
    # params = panel$server("panel", params)
    panel$server("panel", dat = dat, params = params, coords = coords, grid = grid)

  })
}
