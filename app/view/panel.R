# app/view/panel.R

box::use(
  shiny[span, icon, dateInput, req, tags, tabsetPanel, fluidRow, tabPanel, absolutePanel, h4, h5, h6, br, textOutput, renderPlot, plotOutput, moduleServer, NS, tagList, selectInput, updateSelectInput, observeEvent, downloadButton, downloadHandler, fileInput, reactiveValues, reactive, renderText, observe],
  bslib[tooltip, popover, navset_underline, nav_panel],
  bsicons[bs_icon],
  magrittr[`%>%`],
  dplyr[filter, pull],
  ggplot2[...],
  plotly[...],
  scales[percent],
  lubridate[today, ymd],
  sf[st_as_sf, st_join],
  glue[glue]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  absolutePanel(id = "controls", class = "panel panel-default",
                              top = 75, left = 55, width = 250, fixed=TRUE,
                              draggable = TRUE, height = "auto",

                navset_underline(

                  nav_panel('Controls',
                    tagList(

                      br(),

                      span(h5("Risk Mapping Demo"), style="color:#045a8d"),

                      span(h6("This demo showcases AI-powered risk mapping using a common agricultural pest as an example, Soybean Aphid.")),

                      span(h6("Higher risk indicates a higher likelihood of pest presence at a particular place and time.")),

                      span(h6("Select a state"), style="color:#045a8d"),

                      selectInput(
                        inputId = ns('statepicker'),
                        label = NULL,
                        choices = 'iowa',
                        selected = 'iowa'
                      ),

                      # h6(textOutput(ns("reactive_coords")), align = "left"),
                      # h6(textOutput(ns("reactive_key")), align = "left"),

                      span(h6("Select a date"), style="color:#045a8d"),

                      dateInput(
                        inputId = ns('datepicker'),
                        label = NULL,
                        value = today(),
                        min = ymd('2026-01-01'),
                        max = ymd('2026-12-01')
                      ),

                      span(h6("Click a location"), style="color:#045a8d"),

                      plotlyOutput(ns("timeline_plot"), height="150px", width="100%"),
                  )

                ),

                nav_panel('About',
                         tagList(

                           br(),

                           span(h5("Risk Mapping Demo"), style="color:#045a8d"),

                           span(h6("About Soybean Aphid"), style="color:#045a8d"),

                           span(h6("Soybean Aphid (Aphis glycines) is a global pest originating from Asia. In the US, it costs Midwest farmers up to $4.9bn per year.")),

                           span(h6("About the model"), style="color:#045a8d"),

                           span(h6("This toy example is based on an machine learning (XGBoost) model which included topographic, climate, demographic, and biological predictors."),
                                h6("Annual predictions here are pre-generated. Client data has been removed, and predictions shouldn't be treated as accurate.")),

                           span(h6("About the creator"), style="color:#045a8d"),

                           span(h6("Alex Blake is a freelance data scientist & machine learning engineer with 6 years of experience working with public and industry clients to visualise, forecast, root-cause, and optimize their business problems."),
                                h6("Prior work in agriculture, environment, aerospace, consumer goods, marketing, and product development.")),

                           fluidRow(span(tags$a(bs_icon("linkedin"), href = "https://www.linkedin.com/in/alex-gangur-25400125/", target = "_blank")), align = 'right')

                         )

                )

                )
  )
}

#' @export
server <- function(id, params, dat, coords, grid) {

  moduleServer(id, function(input, output, session) {

    reactive_metadat = reactive({

      # req(coords)

      md = ifelse(is.na(coords$lat | is.na(coords$long)),
             'No coords selected',
             glue('Lat: {coords$lat}, Long: {coords$long}'))
      md
    })

    reactive_grid = reactive({ grid %>% filter(state == params$state) })

    reactive_key = reactive({

      key_i = NA

      if(!is.na(coords$lat) & !is.na(coords$long)) {

        pt_i = data.frame(x = coords$long, y = coords$lat, selected = T) %>%
          st_as_sf(coords = c('x', 'y'), crs = 4326)

        key_temp = reactive_grid() %>%
          st_join(pt_i) %>%
          filter(selected)

        if (nrow(key_temp) == 1) {key_i = key_temp %>% pull(key)}
      }

      key_i #returns NA if outside of focal state

    })

    output$timeline_plot = renderPlotly({

      req(reactive_key())

      dat_i = dat() %>% filter(key == reactive_key())

      p = ggplot(dat_i, aes(x = date, y = round(pred, 3))) + geom_point(size = 0.1, alpha = 1, color='black') + geom_step() +
        scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month", date_labels = "%b") +
        scale_y_continuous(labels = percent) +
        ylab("Risk") +  xlab("Date") + theme_bw() +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10),
              plot.margin = margin(0, 12, 5, 5))
      ggplotly(p) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) %>% #, hovermode='x unified'
        config(displayModeBar = F) %>%
        style(hoverinfo = "none")


    })

    output$reactive_coords = renderText({ reactive_metadat() })

    output$reactive_key = renderText({ reactive_key() })

    observe({ params$date = input$datepicker })

    observe({

      params$state = input$statepicker

      updateSelectInput(session, "statepicker",
                        choices = params$state_list,
                        selected = params$state
      )

      })

    return(params)

  })

}
