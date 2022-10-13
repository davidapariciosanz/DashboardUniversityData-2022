library(shiny)
library(shidashi)
library(ggplot2)
library(ggExtra)
library(plyr)
library(data.table)
library(plotly)
library(dplyr)
library(leaflet)
library(cowplot)
library(ggiraph)
library(grDevices)

ui_notas <- function(){
  
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
        )
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
        title = "Notas de corte",
        class_body = "height-150",
        tools = list(
          card_tool(widget = 'maximize', title = "Maximizar")
        ),
        plotlyOutput(ns("cut_notes"), height = "100%")
      )
    ),
    
    
    column(
      width = 3L,
      card(
        title = "Notas de acceso",
        class_body = "height-150",
        tools = list(
          card_tool(widget = 'maximize', title = "Maximizar")
        ),
        plotlyOutput(ns("access"), height = "100%")
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
        title = "Selecciona titulación",
        class_body = "height-150",
        class = "padding-top-0",
        selectInput(ns("titulation"),
                    label = NULL, 
                    choices = NULL,
                    selected = NULL)
      )
    ),
    
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
      width = 9L,
      card(
        title = "Diagrama de cajas múltiples de notas de acceso",
        class_body = "no-padding",
        tools = list(
          card_tool(widget = "flip", title = "Ver datos"),
          card_tool(widget = 'maximize', title = "Maximizar")
        ),
        body_main = flip_box(
          front = div(
            class = "fill-width height-600 min-height-600 resize-vertical",
            plotlyOutput(ns("time_seriess"), height = "100%")

          ),
          back = column(12,
                               align="center", 
                        tableOutput(ns("multiple_boxplot")))
        ),
        body_side = div(
          class = "padding-top-0"
        )
      )
    ),
    
    column(
      width = 3L,
      card(
        title = "Nota de acceso global",
        class_body = "no-padding",
        tools = list(
          card_tool(widget = 'maximize', title = "Maximizar")
        ),

        div(
          class = "fill-width height-600 min-height-600 resize-vertical",
          plotlyOutput(ns("circle_map"), height = "100%")
        ),

        body_side = div(
          class = "padding-top-0"
        
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
  allTit.lst <- readRDS(file.path(path_rds, 'allTit.lst.RDS'))
  notas.lst <- readRDS(file.path(path_rds, 'notas.lst.RDS'))
  year.vec <- readRDS(file.path(path_rds, 'year.vec.RDS'))
  
  output$txtOutput = renderText({
    inf.lst[[input$university]]
  })
  
  output$image <- renderUI({
    tags$img(src = paste0("shidashi/img/logo_", input$university, ".png"), width = 200, height = 200, style="display: block; margin-left: auto; margin-right: auto; margin-top: 50px;")
  })
  
  observe({
    updateSelectInput(session, "titulation",
                      label = NULL,
                      choices = allTit.lst[[input$university]],
                      selected = allTit.lst[[input$university]][1]
                      )
  })
  
  output$circle_map <- renderPlotly({
    theme <- get_theme(event_data)
    ggtheme <- generate_ggtheme(theme)
    
    temp.dt <- notas.lst[[input$university]][des_titulacion %in% as.vector(input$titulation)]
    
    if(nrow(temp.dt) > 0){
      g <- ggplot(temp.dt,
                  aes(y = nota_admision)) +
        geom_boxplot(fill = hcl.colors(1, "TealGrn")) +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "#646464", size = 12, face = "bold", hjust = 0.5),
        ) +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position="none",
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.grid.major.x = element_blank()) +
        scale_y_continuous(name="Stopping distance", breaks=c(5:14)) +
        ggtheme
      
      g %>%
        ggplotly() %>%
        config(displayModeBar = FALSE)
    }
  })
  
  output$time_seriess <- renderPlotly({
    theme <- get_theme(event_data)
    ggtheme <- generate_ggtheme(theme)
    
    temp.dt <- notas.lst[[input$university]][des_titulacion %in% as.vector(input$titulation)][, year := as.factor(year)]
    
    if(nrow(temp.dt) > 0){
      g <- ggplot(temp.dt, aes(x = year, y = nota_admision)) +
        geom_boxplot(fill = hcl.colors(length(year.vec), "Teal")) +
        labs(
          title = "",
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "#FF8000", size = 16, face = "bold", hjust = 0.5),
        ) +
        theme(axis.line=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position="none",
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.grid.major.x = element_blank()) +
        scale_y_continuous(name="Stopping distance", breaks=c(5:14)) +
        ggtheme
  
      g %>%
        ggplotly() %>%
        config(displayModeBar = FALSE)
    }
  })
  
  output$multiple_boxplot <- renderTable({
    temp.dt <- notas.lst[[input$university]][des_titulacion %in% as.vector(input$titulation)]
    temp.dt[
      , year := as.integer(year)][
      , nota_admision_cat := NULL]
    setnames(temp.dt, c('year', 'nota_admision', 'des_titulacion'), c('Año', 'Nota', 'Titulación'))
  })
  
  output$access <- renderPlotly({
    theme <- get_theme(event_data)
    ggtheme <- generate_ggtheme(theme)
    
    temp.dt <- na.omit(notas.lst[[input$university]][des_titulacion %in% as.vector(input$titulation) & year %in% as.vector(input$year), .N, by = nota_admision_cat])
    setnames(temp.dt, c('N', 'nota_admision_cat'), c('Estudiantes', 'Nota'))
    
    if(nrow(temp.dt) > 0){
      g <- ggplot(data=temp.dt,
             aes(y=Estudiantes, x=Nota)) +
        geom_bar(stat="identity", position = 'dodge', fill=hcl.colors(1, "TealGrn")) +
        theme_minimal() +
        theme(axis.line=element_blank(),
              axis.text.y=element_blank(),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position="none",
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.grid.major.x = element_blank()) +
        ggtheme
       
      g %>%
        ggplotly() %>%
        config(displayModeBar = FALSE)
    }
  })
  
  output$cut_notes <- renderPlotly({
    theme <- get_theme(event_data)
    ggtheme <- generate_ggtheme(theme)
    
    temp.dt <- notas.lst[[input$university]][des_titulacion %in% as.vector(input$titulation), .(NotaDeCorte = min(nota_admision)), by = year]
    setnames(temp.dt, 'year', 'Año')
    
    if(nrow(temp.dt) > 0){
      g <- ggplot(temp.dt, aes(x=Año, y=NotaDeCorte, group = 1)) +
        geom_line(color=hcl.colors(1, "TealGrn")) +
        theme(axis.line=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position="none",
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.grid.major.x = element_blank(),
              plot.title = element_text(size=6, hjust = 0.5)) +
        ggtheme
      
      g %>%
        ggplotly() %>%
        config(displayModeBar = FALSE)
    }
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
