library(shiny)
library(shidashi)
library(ggplot2)
library(ggExtra)
library(plyr)
library(data.table)
library(plotly)
library(dplyr)
library(leaflet)
library(readr)
library(geojsonio)

ui_acceso <- function(){
  
  fluidRow(
    
    column(
      width = 9L,
      card(
        title = "Información sobre la universidad",
        class_body = "height-250",
        textOutput(ns("txtOutput")),
        tools = list(
          as_badge(sprintf("%s|bg-primary", Sys.Date())),
          as_badge("5 Años|badge-warning"),
          as_badge("5 Universidades|badge-warning"))
      )
    ),
    
    column(
      width = 3L,
      div(
        uiOutput(ns("image"))
      )
    ),
    
    column(
      width = 9L,
      card2(
        title = "Series temporales del número de estudiantes por municipio",
        class_body = "height-150",
        
        tools = list(
          card_tool(widget = "maximize", title = "Maximizar")
        ),
        
        plotlyOutput(ns("time_series"), height = "100%"),

        
        body_side = div(
          class = "padding-top-0",
          h1(),
          selectInput(ns("time_series_options"),
                      label = NULL,
                      choices = NULL,
                      selected = NULL, 
                      multiple = TRUE)
        )
        
      )
    ),
    
    tags$head(tags$style(HTML('
      .form-group, .selectize-control {
           margin-top: -10px;
      }'
      ))),
    
    column(
      width = 3L,
      card(
        title = "Selecciona",
        class_body = "height-150",
        class = "padding-top-0",
        selectInput(ns("university"),
                    label = '',
                    choices = c('Universid Autónoma de Madrid' = 'uam',
                                'Universidad Carlos III de Madrid' = 'uc3m',
                                'Universidad Complutense de Madrid' = 'ucm',
                                'Universidad Rey Juan Carlos' = 'urjc',
                                'Universidad de Valladolid' = 'uva'),
                    selected = 'uva'),
        selectInput(ns("year"), label = '', choices = c('Curso académico 2017' = '2017',
                                                        'Curso académico 2018' = '2018',
                                                        'Curso académico 2019' = '2019',
                                                        'Curso académico 2020' = '2020',
                                                        'Curso académico 2021' = '2021'),
                    selected = '2021')
      )
    ),

    column(
      width = 3L,
      card(
         
        title = "Tipo de acceso",
        class_body = "height-600",
        
        column(
          width = 12L,
          progressOutput(outputId = ns("progress_1"),
                         description = "",
                         "Bachillerato LOE",
                         class = "bg-yellow"),
          
          progressOutput(outputId = ns("progress_2"),
                         description = "",
                         "Bachillerato LOGSE",
                         class = "bg-yellow"),
          
          progressOutput(outputId = ns("progress_3"),
                         description = "",
                         "Bachillerato LOMCE",
                         class = "bg-yellow"),
          
          progressOutput(outputId = ns("progress_4"),
                         description = "",
                         "COU",
                         class = "bg-yellow"),
          
          progressOutput(outputId = ns("progress_5"),
                         description = "",
                         "Técnico superior o equivalente",
                         class = "bg-yellow")
        )
      )
    ),
    
    column(
      width = 9L,
      card_tabset(
        title = "Mapa coroplético de estudiantes",
        class_body = "no-padding",
        tools = list(
          card_tool(widget = "maximize", title = "Maximizar")
        ),
        "Provincias" = div(
          class = "fill-width height-600 min-height-600 resize-vertical",
          leafletOutput(ns("province_map"), height = "100%")
        ),
        "Comunidades autónomas" = div(
          class = "fill-width height-600 min-height-600 resize-vertical",
          leafletOutput(ns("community_map"), height = "100%")
        )
      )
      
    
    
    )
  )
}

server_demo <- function(input, output, session, ...){

  shared_data <- shidashi::register_session_id(session)
  event_data <- register_session_events(session)
  local_data <- reactiveValues()
  
  path_rds <- file.path(getwd(), 'rds')
  
  inf.lst <- readRDS(file.path(path_rds, 'inf.lst.RDS'))
  nEstAc.lst <- readRDS(file.path(path_rds, 'nEstAc.lst.RDS'))
  nProvCenSec.lst <- readRDS(file.path(path_rds, 'nProvCenSec.lst.RDS'))
  nComCenSec.lst <- readRDS(file.path(path_rds, 'nComCenSec.lst.RDS'))
  nMunCenSec.lst <- readRDS(file.path(path_rds, 'nMunCenSec.lst.RDS'))
  allMun.lst <- readRDS(file.path(path_rds, 'allMun.lst.RDS'))
  year.vec <- readRDS(file.path(path_rds, 'year.vec.RDS'))
  idPro.dt <- readRDS(file.path(path_rds, 'idPro.dt.RDS'))
  idCom.dt <- readRDS(file.path(path_rds, 'idCom.dt.RDS'))
  spain_provinces.sp <- readRDS(file.path(path_rds, 'spain_provinces.sp.RDS'))
  spain_communities.sp <- readRDS(file.path(path_rds, 'spain_communities.sp.RDS'))
  
  observe({
    updateSelectInput(session, "time_series_options",
                      label = 'Municipios',
                      choices = allMun.lst[[input$university]],
                      selected = allMun.lst[[input$university]][sample(length(allMun.lst[[input$university]]), 3)])
  })
  
  output$txtOutput = renderText({
    inf.lst[[input$university]]
  })
  
  output$image <- renderUI({
    tags$img(src = paste0("shidashi/img/logo_", input$university, ".png"),
             width = 200, height = 200,
             style="display: block; margin-left: auto; margin-right: auto; margin-top: 50px;")
  })
  
  output$progress_1 <- renderProgress({
    temp.dt <- nEstAc.lst[[input$university]][year %in% as.vector(input$year)]
    
    if(nrow(temp.dt) > 0){
      num <- temp.dt[des_estudio_acceso %in% 'Bachillerato LOE', nStu]
      if(length(num) == 0){num <- 0}
      den <- temp.dt[, sum(nStu)]
      lab <- den
    }
    else{
      num <- 0
      den <- Inf
      lab <- 0
    }

    return(list(
      description =  paste(num, '/', lab),
      value = num / den * 100)
    )
  })

  output$progress_2 <- renderProgress({
    temp.dt <- nEstAc.lst[[input$university]][year %in% as.vector(input$year)]
    
    if(nrow(temp.dt) > 0){
      num <- temp.dt[des_estudio_acceso %in% 'Bachillerato LOGSE', nStu]
      if(length(num) == 0){num <- 0}
      den <- temp.dt[, sum(nStu)]
      lab <- den
    }
    else{
      num <- 0
      den <- Inf
      lab <- 0
    }
    
    return(list(
      description =  paste(num, '/', lab),
      value = num / den * 100)
    )
  })
  
  output$progress_3 <- renderProgress({
    temp.dt <- nEstAc.lst[[input$university]][year %in% as.vector(input$year)]
    
    if(nrow(temp.dt) > 0){
      num <- temp.dt[des_estudio_acceso %in% 'Bachillerato LOMCE', nStu]
      if(length(num) == 0){num <- 0}
      den <- temp.dt[, sum(nStu)]
      lab <- den
    }
    else{
      num <- 0
      den <- Inf
      lab <- 0
    }
    
    return(list(
      description =  paste(num, '/', lab),
      value = num / den * 100)
    )
  })
  
  output$progress_4 <- renderProgress({
    temp.dt <- nEstAc.lst[[input$university]][year %in% as.vector(input$year)]
    
    if(nrow(temp.dt) > 0){
      num <- temp.dt[des_estudio_acceso %in% 'COU', nStu]
      if(length(num) == 0){num <- 0}
      den <- temp.dt[, sum(nStu)]
      lab <- den
    }
    else{
      num <- 0
      den <- Inf
      lab <- 0
    }
    
    return(list(
      description =  paste(num, '/', lab),
      value = num / den * 100)
    )
  })
  
  output$progress_5 <- renderProgress({
    temp.dt <- nEstAc.lst[[input$university]][year %in% as.vector(input$year)]
    
    if(nrow(temp.dt) > 0){
      num1 <- temp.dt[des_estudio_acceso %in% 'Técnico/Técnica Superior de Artes plásticas y diseño o título equivalente', nStu]
      if(length(num1) == 0){num1 <- 0}
      
      num2 <- temp.dt[des_estudio_acceso %in% 'Técnico Deportivo/Técnica Deportiva  Superior o título equivalente', nStu]
      if(length(num2) == 0){num2 <- 0}
      
      num3 <- temp.dt[des_estudio_acceso %in% 'Técnico /Técnica Superior de Formación Profesional o título equivalente', nStu]
      if(length(num3) == 0){num3 <- 0}
      
      num <- num1 + num2 + num3
      
      den <- temp.dt[, sum(nStu)]
      lab <- den
    }
    else{
      num <- 0
      den <- Inf
      lab <- 0
    }
    
    return(list(
      description =  paste(num, '/', lab),
      value = num / den * 100)
    )
  })
  
  output$province_map <- renderLeaflet({
    nProvCenSec.dt <- nProvCenSec.lst[[input$university]][year %in% as.vector(input$year)]

    idPro.dt[, idLeaflet := c(14, 12, 16, 10, 13, 15, 1, 18, 19, 20,
                              21, 22, 23, 25, 3, 26, 4, 27, 28, 29,
                              30, 31, 24, 32, 33, 34, 38, 39, 9, 11,
                              36, 17, 2, 40, 5, 6, 41, 7, 8, 42,
                              43, 35, 45, 46, 44, 47, 48, 49, 50, 51,
                              37, 52)]
    
    idPro.dt <- merge(idPro.dt, nProvCenSec.dt, by = "province", all = TRUE)[, year := NULL] %>%
      replace(is.na(.), 0)
    
    idPro.dt <- arrange(idPro.dt, idLeaflet)
   
    bins <- c(0, 1, 25, 50, 75, 100, 250, 500, 1000, Inf)
    pal <- colorBin(rev(hcl.colors(9, "RedOr")), bins = bins)
    
    name <- idPro.dt$province 
    density <- idPro.dt$nStu # Ver los idx
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g estudiantes",
      name, density
    ) %>% lapply(htmltools::HTML)
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = spain_provinces.sp,
                  fillColor = ~pal(density),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal,
                values = density,
                opacity = 1,
                title = NULL,
                position = "topright") %>%
      setView(lng = -3.996481424489623, lat =  39.996922927972754, zoom = 6) %>% 
      addProviderTiles("TomTom.Basic")
    
  })
  
  output$community_map <- renderLeaflet({
    
    nComCenSec.dt <- nComCenSec.lst[[input$university]][year %in% as.vector(input$year)]

    idCom.dt[, idLeaflet := c(9, 15, 10, 6, 7, 8, 1, 18, 2, 11, 17, 14, 16, 4, 13, 19, 5, 3, 12)]
    
    idCom.dt <- merge(idCom.dt, nComCenSec.dt, by = "community", all = TRUE)[, year := NULL] %>%
      replace(is.na(.), 0)
    
    idCom.dt <- arrange(idCom.dt, idLeaflet)
    
    bins <- c(0, 1, 25, 50, 75, 100, 250, 500, 1000, Inf)
    pal <- colorBin(rev(hcl.colors(9, "RedOr")), bins = bins)
    
    name <- idCom.dt$community
    density <- idCom.dt$nStu
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g estudiantes",
      name, density
    ) %>% lapply(htmltools::HTML)
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = spain_communities.sp,
                  fillColor = ~pal(density),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal,
                values = density,
                opacity = 1,
                title = NULL,
                position = "bottomright") %>%
      setView(lng = -3.996481424489623, lat =  39.996922927972754, zoom = 6) %>% 
      addProviderTiles("TomTom.Basic")
  })
  
  output$time_series <- renderPlotly({
    theme <- get_theme(event_data)
    ggtheme <- generate_ggtheme(theme)
    
    temp.dt <- nMunCenSec.lst[[input$university]][municipality %in% as.vector(input$time_series_options)]
    setnames(temp.dt, c('year', 'nStu', 'municipality'), c('Año', 'Estudiantes', 'Municipio'), skip_absent=TRUE)

    if(nrow(temp.dt) > 0){
      g <- ggplot(temp.dt,
                  aes(x = Año,
                      y = Estudiantes,
                      color = Municipio,
                      group = Municipio)) +
        geom_line() +
        ggtheme +
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank()) +
        scale_color_manual(values=hcl.colors(length(input$time_series_options), "RedOr"), "Teal") 

      g %>%
        ggplotly(tooltip = c("x","y","colour")) %>%
        layout(xaxis = list(title = year.vec)) %>%
        layout(showlegend = FALSE) %>%
        config(modeBarButtonsToAdd = list('drawline', 
                                          'drawopenpath'),
               modeBarButtonsToRemove = c('select', 'lasso2d'),
               displaylogo = FALSE)
    }
   
    
  })
  
  output$time_series_table <- renderTable({
    
    temp.dt <- nMunCenSec.lst[[input$university]][municipality %in% as.vector(input$time_series_options)]
    temp.dt[
      , year := as.integer(year)][
      , nStu := as.integer(year)]
    setnames(temp.dt, c('year', 'nStu', 'municipality'), c('Año', 'Estudiantes', 'Municipio'), skip_absent=TRUE)
    
  })
  
  generate_ggtheme <- function(
    theme,
    panel.background = element_rect(
      fill = theme$background, color = theme$background),
    plot.background = element_rect(
      fill = theme$background, color = theme$background),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.y.left = element_blank(),
    legend.key = element_rect(fill = theme$background, colour = theme$background),
    rect = element_rect(fill = theme$background, colour = theme$foreground),
    title = element_text(color = theme$foreground),
    text = element_text(color = theme$foreground),
    line = element_line(color = theme$foreground),
    ...){
    ggplot2::theme(
      panel.background = panel.background,
      plot.background = plot.background,
      panel.grid.major = panel.grid.major,
      panel.grid.minor = panel.grid.minor,
      axis.line.x.bottom = axis.line.x.bottom,
      axis.line.y.left = axis.line.y.left,
      legend.key = legend.key,
      rect = rect,
      title = title,
      text = text,
      line = line,
      ...
    )
  }

  run_analysis <- function(){
    show_notification(
      title = "Generating analysis...",
      subtitle = "This might take a while",
      class = "bg-primary",
      close = FALSE,
      autohide = FALSE,
      progressOutput(ns("data_gen_pro"), description = "Loading data...",
                     size = 'xs', class = "bg-yellow")
    )
    on.exit({ clear_notifications() })

    progress <- shiny_progress("", max = 10, outputId = "data_gen_pro")
    for(i in 1:10){
      progress$inc(sprintf("step %s", i), message = ifelse(
        i > 5, "Analyze data", "Loading data"
      ))
      Sys.sleep(0.2)
    }
    local_data$data <- data.frame(
      name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100) ,
              sample(LETTERS, 20000, replace = TRUE)),
      value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1), rnorm(20000, 15, 30) )
    )
  }
  
  observeEvent(input$refresh, {
    run_analysis()
  })
  observeEvent(shared_data$reactives[[ns("refresh")]], {
    if(shared_data$reactives[[ns("refresh")]] > 0){
      run_analysis()
    }
  })


}
