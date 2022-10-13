library(shiny)
library(shidashi)
library(ggplot2)
library(ggExtra)
library(plyr)
library(data.table)
library(plotly)
library(dplyr)
library(leaflet)
library(grDevices)

ui_titulaciones <- function(){
  
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
          as_badge("5 Universidades|badge-warning")
        ),
      )
    ),
    
    column(
      width = 3L,
      div(
        uiOutput(ns("image"))
      )
    ),
    
    column(
      width = 3L,
      card(
        title = "Nuevos estudiantes totales",
        class_body = "height-150",
        tools = list(
          card_tool(widget = 'maximize', title = "Maximizar")
        ),
        plotlyOutput(ns("new_students"), height = "100%")
      )
    ),
    
    column(
      width = 3L,
      card(
        title = "Carrera con más estudiantes",
        class_body = "height-150",
        tools = list(
          card_tool(widget = 'maximize', title = "Maximizar")
        ),
        plotlyOutput(ns("titulation_top_students"), height = "100%")
      )
    ),
    
    column(
      width = 3L,
      card(
        title = "Centro con más estudiantes",
        class_body = "height-150",
        tools = list(
          card_tool(widget = 'maximize', title = "Maximizar")
        ),
        plotlyOutput(ns("faculty_top_students"), height = "100%")
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
        
        selectInput(ns("year"),
                    label = '',
                    choices = c('Curso académico 2017' = '2017',
                                'Curso académico 2018' = '2018',
                                'Curso académico 2019' = '2019',
                                'Curso académico 2020' = '2020',
                                'Curso académico 2021' = '2021'),
                    selected = '2021')
        
      )
    ),
    
    column(
      width = 6L,
      card2(
        title = "Series temporales de carreras y centros",
        class_body = "no-padding",
        
        tools = list(
          card_tool(widget = "flip", title = "Ver datos"),
          card_tool(widget = 'maximize', title = "Maximizar")
        ),
        
        body_main = flip_box(
          front = div(
            class = "fill-width height-600 min-height-600",
            plotlyOutput(ns("time_series"), height = "100%")
          ),
          back = column(12,
                        align="center", 
                        tableOutput(ns("time_series_table")))
        ),
        
        body_side = div(
          class = "padding-top-0",
          h1(),
          selectInput(ns("time_series_type"),
                      label = 'Tipo de series',
                      choices = c('Titulaciones' = 'Titulaciones',
                                  'Facultades' = 'Facultades'),
                      selected = 'Titulaciones',
                      multiple = FALSE),
          
          selectInput(ns("time_series_options"),
                      label = NULL,
                      choices = NULL,
                      selected = NULL, 
                      multiple = TRUE),
          
          h6("Mostrar nombres"),
          checkboxInput(ns("time_series_labels"),
                        label = NULL,
                        value = TRUE)
        )
        
      )
    ),
    
    column(
      width = 6L,
      card2(
        title = "Mapa de burbujas de facultades",
        class_body = "no-padding",
        
        tools = list(
          card_tool(widget = "flip", title = "Ver datos"),
          card_tool(widget = 'maximize', title = "Maximizar")
        ),
        
        body_main = flip_box(
          front = div(
            class = "fill-width height-600 min-height-600",
            leafletOutput(ns("circle_map"), height = "100%")
          ),
          back = column(12,
                        align="center", 
                        tableOutput(ns("circle_map_table")))
        ),
        
        body_side = div(
          class = "padding-top-0",
          h1(),
          sliderInput(ns("range_slider"), "Número de estudiantes",
                      min = 0, max = 2000, value = c(0, 2000)
          )
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
  
  uni.vec <- readRDS(file.path(path_rds, 'uni.vec.RDS'))
  year.vec <- readRDS(file.path(path_rds, 'year.vec.RDS'))
  inf.lst <- readRDS(file.path(path_rds, 'inf.lst.RDS'))
  allTit.lst <- readRDS(file.path(path_rds, 'allTit.lst.RDS'))
  allFac.lst <- readRDS(file.path(path_rds, 'allFac.lst.RDS'))
  seriesTit.lst <- readRDS(file.path(path_rds, 'seriesTit.lst.RDS'))
  seriesFac.lst <- readRDS(file.path(path_rds, 'seriesFac.lst.RDS'))
  nStuYear.lst <- readRDS(file.path(path_rds, 'nStuYear.lst.RDS'))
  nStuTitYear.lst <- readRDS(file.path(path_rds, 'nStuTitYear.lst.RDS'))
  nStuFacYear.lst <- readRDS(file.path(path_rds, 'nStuFacYear.lst.RDS'))
  coordFac.lst <- readRDS(file.path(path_rds, 'coordFac.lst.RDS'))
  
  observe({
    if(input$time_series_type == 'Titulaciones'){
      updateSelectInput(session, "time_series_options",
                        label = 'Titulaciones',
                        choices = allTit.lst[[input$university]],
                        selected = seriesTit.lst[[input$university]])
    }
    else if(input$time_series_type == 'Facultades'){
      updateSelectInput(session, "time_series_options",
                        label = 'Facultades',
                        choices = allFac.lst[[input$university]],
                        selected = seriesFac.lst[[input$university]])
    }
  })
  
  output$txtOutput = renderText({
    inf.lst[[input$university]]
  })
  
  output$image <- renderUI({
    tags$img(src = paste0("shidashi/img/logo_", input$university, ".png"), width = 200, height = 200, style="display: block; margin-left: auto; margin-right: auto; margin-top: 50px;")
  })
  
  output$new_students <- renderPlotly({
    theme <- get_theme(event_data)
    ggtheme <- generate_ggtheme(theme)
    
    temp.dt <- nStuYear.lst[[input$university]]
    setnames(temp.dt, c('year', 'nStu'), c('Año', 'Estudiantes'), skip_absent=TRUE)
    
    g <- ggplot(temp.dt, aes(x=Año, y=Estudiantes)) +
      geom_line( color=hcl.colors(1, "TealGrn")) +
      ggtheme +
      theme(axis.line=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.grid.major.x = element_blank())
    g %>%
      ggplotly() %>%
      config(displayModeBar = FALSE) %>% 
      layout(xaxis = list(title = year.vec))
    
  })
  
  output$titulation_top_students <- renderPlotly({
    theme <- get_theme(event_data)
    ggtheme <- generate_ggtheme(theme)
    
    temp.dt <- nStuTitYear.lst[[input$university]]
    temp.dt <- temp.dt[des_titulacion %in% temp.dt[year %in% as.vector(input$year) & nStu %in% temp.dt[year %in% as.vector(input$year)][, max(nStu)]][, des_titulacion]]
    setnames(temp.dt, c('year', 'nStu', 'des_titulacion'), c('Año', 'Estudiantes', 'Titulacion'), skip_absent=TRUE)
    
    g <- ggplot(temp.dt, aes(x=Año, y=Estudiantes, Titulacion = Titulacion)) +
      geom_line( color=hcl.colors(1, "TealGrn")) +
      ggtheme +
      labs(title=temp.dt$Titulacion[1]) +
      theme(axis.line=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=6, hjust = 0.5))
    g %>%
      ggplotly() %>%
      config(displayModeBar = FALSE) %>% 
      layout(xaxis = list(title = year.vec))
  })
  
  output$faculty_top_students <- renderPlotly({
    theme <- get_theme(event_data)
    ggtheme <- generate_ggtheme(theme)
    
    temp.dt <- nStuFacYear.lst[[input$university]]
    temp.dt <- temp.dt[des_centro %in% temp.dt[year %in% as.vector(input$year) & nStu %in% temp.dt[year %in% as.vector(input$year)][, max(nStu)]][, des_centro]]
    setnames(temp.dt, c('year', 'nStu', 'des_centro'), c('Año', 'Estudiantes', 'Facultad'), skip_absent=TRUE)

    g <- ggplot(temp.dt, aes(x=Año, y=Estudiantes, Facultad = Facultad)) +
      geom_line( color=hcl.colors(1, "TealGrn")) +
      ggtheme +
      labs(title=temp.dt$Facultad[1]) +
      theme(axis.line=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size=6, hjust = 0.5))
    g %>%
      ggplotly() %>%
      config(displayModeBar = FALSE) %>% 
      layout(xaxis = list(title = year.vec))
  })

  output$time_series <- renderPlotly({
    theme <- get_theme(event_data)
    ggtheme <- generate_ggtheme(theme)
    
    
    if(input$time_series_type == 'Titulaciones'){
      temp.dt <- nStuTitYear.lst[[input$university]][des_titulacion %in% as.vector(input$time_series_options)]
      temp.dt[, year := as.factor(year)]
      setnames(temp.dt, c('year', 'nStu', 'des_titulacion'), c('Año', 'Estudiantes', 'Titulacion'), skip_absent=TRUE)
      
      if(nrow(temp.dt) > 0){
        g <- ggplot(temp.dt,
              aes(x = Año,
                   y = Estudiantes,
                   colour = Titulacion,
                   group = Titulacion)) +
              geom_line() +
              ggtheme +
              theme(axis.title.x=element_blank(),
                    axis.title.y=element_blank())  +
          scale_color_manual(values=hcl.colors(length(input$time_series_options), "Teal")) + 
          scale_x_discrete(name ="", limits=c("2017","2018","2019", "2020", "2021"))
        
        if(input$time_series_labels){
          g <- g +  
            geom_text(data = . %>%
                        group_by(Titulacion) %>%
                        filter(Año == '2017'),
                      aes(label = Titulacion),
                      nudge_x = 0,
                      nudge_y = 9,
                      color = "gray",
                      size = 2.4)
        }
        
        g %>% 
          ggplotly(tooltip = c("x","y","colour")) %>% 
          layout(xaxis = list(title = year.vec)) %>%
          layout(showlegend = FALSE) %>%
          config(modeBarButtonsToAdd = list('drawline', 
                                            'drawopenpath'),
                 modeBarButtonsToRemove = c('select', 'lasso2d'),
                 displaylogo = FALSE)
      }
    }
    else if(input$time_series_type == 'Facultades'){
      temp.dt <- nStuFacYear.lst[[input$university]][des_centro %in% as.vector(input$time_series_options)]
      temp.dt[, year := as.factor(year)]
      setnames(temp.dt, c('year', 'nStu', 'des_centro'), c('Año', 'Estudiantes', 'Facultad'), skip_absent=TRUE)
      
      if(nrow(temp.dt) > 0){
        g <- ggplot(temp.dt,
                    aes(x = Año,
                        y = Estudiantes,
                        color = Facultad,
                        group = Facultad)) +
          geom_line() +
          ggtheme +
          theme(axis.title.x=element_blank(),
                axis.title.y=element_blank()) +
          scale_color_manual(values=hcl.colors(length(input$time_series_options), "Teal")) + 
          scale_x_discrete(name ="", limits=c("2017","2018","2019", "2020", "2021"))
        
        
        if(input$time_series_labels){
          g <- g + geom_text(data = . %>%
                        group_by(Facultad) %>%
                        filter(Año == '2017'),
                      aes(label = Facultad),
                      nudge_x = 0,
                      nudge_y = 9,
                      color = "gray",
                      size = 2.4)
        }
        
        g %>% 
          ggplotly(tooltip = c("x","y","colour")) %>% 
          layout(xaxis = list(title = year.vec)) %>%
          layout(showlegend = FALSE) %>%
          config(modeBarButtonsToAdd = list('drawline', 
                                            'drawopenpath'),
                 modeBarButtonsToRemove = c('select', 'lasso2d'),
                 displaylogo = FALSE)
      }
      
    }
    
  })
  
  output$time_series_table <- renderTable({
    theme <- get_theme(event_data)
    ggtheme <- generate_ggtheme(theme)

    
    if(input$time_series_type == 'Titulaciones'){
      temp.dt <- nStuTitYear.lst[[input$university]][des_titulacion %in% as.vector(input$time_series_options)]
      temp.dt[
        , year := as.integer(year)][
          , nStu := as.integer(nStu)]
      setnames(temp.dt, c('year', 'nStu', 'des_titulacion'), c('Año', 'Estudiantes', 'Titulacion'), skip_absent=TRUE)
     
    }
    else if(input$time_series_type == 'Facultades'){
      temp.dt <- nStuFacYear.lst[[input$university]][des_centro %in% as.vector(input$time_series_options)]
      temp.dt[
        , year := as.integer(year)][
        , nStu := as.integer(nStu)]
      setnames(temp.dt, c('year', 'nStu', 'des_centro'), c('Año', 'Estudiantes', 'Facultad'), skip_absent=TRUE)
      
    }
    
  })
  
  output$circle_map <- renderLeaflet({
    
    data.dt <- nStuFacYear.lst[[input$university]][year %in% as.vector(input$year)][coordFac.lst[[input$university]], on = 'des_centro'][nStu >= input$range_slider[1] & nStu <= input$range_slider[2]]

    if(nrow(data.dt) > 0){
      data.dt <- data.dt  %>% 
        dplyr::mutate(label = paste("CNTY_ID", "NAME", sep =  "<br>"))
      
      labels <- sprintf(
        "<strong>Centro: </strong> %s <br/> <strong>Estudiantes: </strong> %g",
        data.dt$des_centro, data.dt$nStu
      ) %>% lapply(htmltools::HTML)
      

      pal <- colorBin(rev(hcl.colors(9, "RedOr")), data.dt$nStu, bins = c(0, 1, 25, 50, 75, 100, 250, 500, 1000, Inf))
  
      leaflet(data = data.dt) %>% addTiles(group = "OpenStreetMap") %>%
        
        addProviderTiles(providers$HERE.normalDayMobile) %>%
        
        addCircleMarkers(~lon, ~lat,
                         label = labels,
                         color = ~pal(nStu),
                         clusterOptions = markerClusterOptions(),
                         radius = 20,
                         stroke = FALSE, fillOpacity = 1) %>%
        
        addLayersControl(baseGroups = c("OpenStreetMap"),
                         overlayGroups = c("Leyenda")) %>%
        
        addLegend(title = "Estudiantes",
                  pal = pal,
                  values = ~nStu,
                  group = "Leyenda",
                  position = "topright",
                  labels = "1",
                  opacity = 1)
    }
    
  })
  
  output$circle_map_table <- renderTable({
    
    data.dt <- nStuFacYear.lst[[input$university]][year %in% as.vector(input$year)][coordFac.lst[[input$university]], on = 'des_centro'][nStu >= input$range_slider[1] & nStu <= input$range_slider[2]]
    data.dt[
      , year := as.integer(year)][
      , nStu := as.integer(nStu)]
    setnames(data.dt, c('year', 'nStu', 'des_centro', 'lat', 'lon'), c('Año', 'Estudiantes', 'Facultad', 'Latitud', 'Longitud'))

    
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
