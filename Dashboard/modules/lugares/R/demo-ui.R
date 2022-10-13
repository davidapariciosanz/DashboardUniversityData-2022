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

ui_lugares <- function(){
  
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
      width = 6L,
      card(
        title = "País de origen",
        class_body = "height-150",
        tools = list(
          card_tool(widget = 'maximize', title = "Maximizar")
        ),
        plotlyOutput(ns("origin_country"), height = "100%")
      )
    ),
    
    column(
      width = 3L,
      card(
        title = "Región de origen",
        class_body = "height-150",
        tools = list(
          card_tool(widget = 'maximize', title = "Maximizar")
        ),
        plotlyOutput(ns("origin_continent"), height = "100%")
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
        title = "Gráficos de anillos de continentes, regiones y países",
        class_body = "no-padding",
        
        tools = list(
          card_tool(widget = "flip", title = "Ver datos")
        ),
        
        body_main = flip_box(
          front = div(
            class = "fill-width height-600 min-height-600 resize-vertical",
            girafeOutput(ns("donut_chart"), height = "100%")

          ),
          back = column(12,
                        align="center", 
            h6("Primer gráfico"),
            tableOutput(ns("donut_chart_one_table")),
            h6("Segundo gráfico"),
            tableOutput(ns("donut_chart_two_table"))
            )
        ),
        body_side = div(
          class = "padding-top-0",
          radioButtons(
            ns("continent_option"),
            label= 'Ámbito territorial',
            choices = c('Continentes' = 'Continentes',
                        'Regiones' = 'ContinentesDesagregados',
                        'Paises' = 'Paises')
          )
        )
      )
    ),
    
    column(
      width = 6L,
      card2(
        title = "Mapa de burbujas de países",
        class_body = "no-padding",
        tools = list(
          card_tool(widget = "flip", title = "Ver datos"),
          card_tool(widget = 'maximize', title = "Maximizar")
        ),
        
        body_main = flip_box(
          front = div(
            class = "fill-width height-600 min-height-600 resize-vertical",
            leafletOutput(ns("circle_map"), height = "100%")
          ),
          back = column(12,
                        align="center", 
                        tableOutput(ns("circle_map_table")),
          )
        ),
        
        body_side = div(
          class = "padding-top-0",
          h1(),
          sliderInput(ns("range_slider"), "Filtrar por países que tengan más
                      que este número de estudiantes",
                      min = 0, max = 100, value = 0
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
  
  inf.lst <- readRDS(file.path(path_rds, 'inf.lst.RDS'))
  nCont.lst <- readRDS(file.path(path_rds, 'nCont.lst.RDS'))
  nContPais.lst <- readRDS(file.path(path_rds, 'nContPais.lst.RDS'))
  nContAgr.lst <- readRDS(file.path(path_rds, 'nContAgr.lst.RDS'))
  coordPais.dt <- readRDS(file.path(path_rds, 'coordPais.dt.RDS'))
  
  output$txtOutput = renderText({
    inf.lst[[input$university]]
  })
  
  output$image <- renderUI({
    tags$img(src = paste0("shidashi/img/logo_", input$university, ".png"), width = 200, height = 200, style="display: block; margin-left: auto; margin-right: auto; margin-top: 50px;")
  })
  
  output$donut_chart <- renderGirafe({
    theme <- get_theme(event_data)
    ggtheme <- generate_ggtheme(theme)
    
    
    switch(input$continent_option,
           Continentes = {
             
             data <- na.omit(nCont.lst[[input$university]][year %in% input$year & !des_continente_nacionalidad %in% 'Europa', sum(nStu)])
             
             temp.dt <- rbind(
               data.table(des_continente_nacionalidad  = 'Otros',
                          year = input$year,
                          nStu = nCont.lst[[input$university]][year %in% input$year & !des_continente_nacionalidad %in% 'Europa', sum(nStu)]),
               nCont.lst[[input$university]][year %in% input$year & des_continente_nacionalidad %in% 'Europa'])
             
             data <- temp.dt
             data$fraction <- data$nStu / sum(data$nStu)
             data$ymax <- cumsum(data$fraction)
             data$ymin <- c(0, head(data$ymax, n=-1))
             data$labelPosition <- (data$ymax + data$ymin) / 2
             data$label <- paste0(data$des_continente_nacionalidad, "\n", data$nStu)
             data <- na.omit(data)
             g1 <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=des_continente_nacionalidad)) +
               geom_rect() +
               geom_text( x=2, aes(y=labelPosition, label=label, color=des_continente_nacionalidad), size=3) + # x here controls label position (inner / outer)
               scale_fill_brewer(palette=3) +
               scale_color_brewer(palette=3) +
               coord_polar(theta="y") +
               xlim(c(-1, 4)) +
               theme_void() +
               theme(legend.position = "none") +
               theme(plot.background = element_rect(fill = "#343a40", colour='#343a40')) +
               scale_color_manual(values=hcl.colors(2, "Teal")) +
               scale_fill_manual(values =hcl.colors(2, "Teal"))
             
             data <- nCont.lst[[input$university]][year %in% input$year & !des_continente_nacionalidad %in% 'Europa']
             data$fraction <- data$nStu / sum(data$nStu)
             data$ymax <- cumsum(data$fraction)
             data$ymin <- c(0, head(data$ymax, n=-1))
             data$labelPosition <- (data$ymax + data$ymin) / 2
             data$label <- paste0(data$des_continente_nacionalidad, "\n", data$nStu)
             data <- na.omit(data)
             
           
             g2 <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=des_continente_nacionalidad)) +
               geom_rect() +
               geom_text( x=2, aes(y=labelPosition, label=label, color=des_continente_nacionalidad), size=3) + # x here controls label position (inner / outer)
               scale_fill_brewer(palette=3) +
               scale_color_brewer(palette=3) +
               coord_polar(theta="y") +
               xlim(c(-1, 4)) +
               theme_void() +
               theme(legend.position = "none") +
               theme(plot.background = element_rect(fill = "#343a40", colour='#343a40')) +
               scale_color_manual(values=hcl.colors(nrow(data)+2, "Teal")[-c(1,2)]) +
               scale_fill_manual(values =hcl.colors(nrow(data)+2, "Teal")[-c(1,2)])
             
             girafe(ggobj = plot_grid(plotlist=list(g1, g2), ncol=2, nrow=1, align = "v"),
                    width_svg = 6, height_svg = 3,
                    options = list(
                      opts_toolbar(saveaspng = FALSE)))
           },
           ContinentesDesagregados = {
             temp.dt <- rbind(
               data.table(des_agregacion_paises_nacionalidad  = 'Otros',
                          year = input$year,
                          nStu = nContAgr.lst[[input$university]][year %in% input$year & !des_agregacion_paises_nacionalidad %in% 'Europa meridional', sum(nStu)]),
               nContAgr.lst[[input$university]][year %in% input$year & des_agregacion_paises_nacionalidad %in% 'Europa meridional'])
             data <- temp.dt
             data$fraction <- data$nStu / sum(data$nStu)
             data$ymax <- cumsum(data$fraction)
             data$ymin <- c(0, head(data$ymax, n=-1))
             data$labelPosition <- (data$ymax + data$ymin) / 2
             data$label <- paste0(data$des_agregacion_paises_nacionalidad, "\n", data$nStu)
             data <- na.omit(data)
             g1 <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=des_agregacion_paises_nacionalidad)) +
               geom_rect() +
               geom_text( x=2, aes(y=labelPosition, label=label, color=des_agregacion_paises_nacionalidad), size=3) + # x here controls label position (inner / outer)
               scale_fill_brewer(palette=3) +
               scale_color_brewer(palette=3) +
               coord_polar(theta="y") +
               xlim(c(-1, 4)) +
               theme_void() +
               theme(legend.position = "none") +
               theme(plot.background = element_rect(fill = "#343a40", colour='#343a40')) +
               scale_color_manual(values=hcl.colors(2, "Teal")) +
               scale_fill_manual(values =hcl.colors(2, "Teal"))
             
             data <- nContAgr.lst[[input$university]][year %in% input$year & !des_agregacion_paises_nacionalidad %in% 'Europa meridional']
             
             temp.dt <- rbind(
               data.table(des_agregacion_paises_nacionalidad  = 'Otros',
                          year = input$year,
                          nStu = data[nStu < mean(nStu), sum(nStu)]),
               data[nStu > mean(nStu)]
             )
             data <- temp.dt
             
             
             data$fraction <- data$nStu / sum(data$nStu)
             data$ymax <- cumsum(data$fraction)
             data$ymin <- c(0, head(data$ymax, n=-1))
             data$labelPosition <- (data$ymax + data$ymin) / 2
             data$label <- paste0(data$des_agregacion_paises_nacionalidad, "\n", data$nStu)
             data <- na.omit(data)
             g2 <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=des_agregacion_paises_nacionalidad)) +
               geom_rect() +
               geom_text( x=2, aes(y=labelPosition, label=label, color=des_agregacion_paises_nacionalidad), size=3) + # x here controls label position (inner / outer)
               scale_fill_brewer(palette=3) +
               scale_color_brewer(palette=3) +
               coord_polar(theta="y") +
               xlim(c(-1, 4)) +
               theme_void() +
               theme(legend.position = "none") +
               theme(plot.background = element_rect(fill = "#343a40", colour='#343a40'))  +
               scale_color_manual(values=hcl.colors(nrow(data)+2, "Teal")[-c(1,2)]) +
               scale_fill_manual(values =hcl.colors(nrow(data)+2, "Teal")[-c(1,2)])
             
             girafe(ggobj = plot_grid(plotlist=list(g1, g2), ncol=2, nrow=1, align = "v"),
                    width_svg = 6, height_svg = 3,
                    options = list(
                      opts_toolbar(saveaspng = FALSE)))
           },
           Paises = {
             temp.dt <- rbind(
               data.table(des_pais_nacionalidad  = 'Otros',
                          year = input$year,
                          nStu = nContPais.lst[[input$university]][year %in% input$year & !des_pais_nacionalidad %in% 'España', sum(nStu)]),
               nContPais.lst[[input$university]][year %in% input$year & des_pais_nacionalidad %in% 'España'])
             
             data <- temp.dt
             data$fraction <- data$nStu / sum(data$nStu)
             data$ymax <- cumsum(data$fraction)
             data$ymin <- c(0, head(data$ymax, n=-1))
             data$labelPosition <- (data$ymax + data$ymin) / 2
             data$label <- paste0(data$des_pais_nacionalidad, "\n", data$nStu)
             data <- na.omit(data)
             g1 <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=des_pais_nacionalidad)) +
               geom_rect() +
               geom_text( x=2, aes(y=labelPosition, label=label, color=des_pais_nacionalidad), size=3) + # x here controls label position (inner / outer)
               scale_fill_brewer(palette=3) +
               scale_color_brewer(palette=3) +
               coord_polar(theta="y") +
               xlim(c(-1, 4)) +
               theme_void() +
               theme(legend.position = "none") +
               theme(plot.background = element_rect(fill = "#343a40", colour='#343a40')) +
               scale_color_manual(values=hcl.colors(2, "Teal")) +
               scale_fill_manual(values =hcl.colors(2, "Teal"))
             
             data <- nContPais.lst[[input$university]][year %in% input$year & !des_pais_nacionalidad %in% 'España']
             
             temp.dt <- rbind(
               data.table(des_pais_nacionalidad  = 'Otros',
                          year = input$year,
                          nStu = data[nStu < mean(nStu) + 5, sum(nStu)]),
               data[nStu > mean(nStu) + 5]
             )
             data <- temp.dt
             
             data$fraction <- data$nStu / sum(data$nStu)
             data$ymax <- cumsum(data$fraction)
             data$ymin <- c(0, head(data$ymax, n=-1))
             data$labelPosition <- (data$ymax + data$ymin) / 2
             data$label <- paste0(data$des_pais_nacionalidad, "\n", data$nStu)
             data <- na.omit(data)
             g2 <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=des_pais_nacionalidad)) +
               geom_rect() +
               geom_text( x=2, aes(y=labelPosition, label=label, color=des_pais_nacionalidad), size=3) + # x here controls label position (inner / outer)
               scale_fill_brewer(palette=3) +
               scale_color_brewer(palette=3) +
               coord_polar(theta="y") +
               xlim(c(-1, 4)) +
               theme_void() +
               theme(legend.position = "none") +
               theme(plot.background = element_rect(fill = "#343a40", colour='#343a40'))+
               scale_color_manual(values=hcl.colors(nrow(data)+2, "Teal")[-c(1,2)]) +
               scale_fill_manual(values =hcl.colors(nrow(data)+2, "Teal")[-c(1,2)])
             
             girafe(ggobj = plot_grid(plotlist=list(g1, g2), ncol=2, nrow=1, align = "v"),
                    width_svg = 6, height_svg = 3,
                    options = list(
                      opts_toolbar(saveaspng = FALSE)))
           })

    
  })
  
  output$donut_chart_one_table <- renderTable({
    switch(input$continent_option,
           Continentes = {
             data <- na.omit(nCont.lst[[input$university]][year %in% input$year & !des_continente_nacionalidad %in% 'Europa', sum(nStu)])
             
             temp.dt <- rbind(
               data.table(des_continente_nacionalidad  = 'Otros',
                          year = input$year,
                          nStu = nCont.lst[[input$university]][year %in% input$year & !des_continente_nacionalidad %in% 'Europa', sum(nStu)]),
               nCont.lst[[input$university]][year %in% input$year & des_continente_nacionalidad %in% 'Europa'])
             
             data <- temp.dt
             data$fraction <- data$nStu / sum(data$nStu)
             data$ymax <- cumsum(data$fraction)
             data$ymin <- c(0, head(data$ymax, n=-1))
             data$labelPosition <- (data$ymax + data$ymin) / 2
             data$label <- paste0(data$des_continente_nacionalidad, "\n", data$nStu)
             data <- na.omit(data)
             data[
               , ymax := NULL][
               , ymin := NULL][
               , label := NULL][
               , labelPosition := NULL][
               , year := as.integer(year)][
               , nStu := as.integer(nStu)]
             setnames(data,
                      c('des_continente_nacionalidad', 'year', 'nStu', 'fraction'),
                      c('Continente', 'Año', 'Estudiantes', 'Proporción'))
           },
           ContinentesDesagregados = {
             temp.dt <- rbind(
               data.table(des_agregacion_paises_nacionalidad  = 'Otros',
                          year = input$year,
                          nStu = nContAgr.lst[[input$university]][year %in% input$year & !des_agregacion_paises_nacionalidad %in% 'Europa meridional', sum(nStu)]),
               nContAgr.lst[[input$university]][year %in% input$year & des_agregacion_paises_nacionalidad %in% 'Europa meridional'])
             data <- temp.dt
             data$fraction <- data$nStu / sum(data$nStu)
             data$ymax <- cumsum(data$fraction)
             data$ymin <- c(0, head(data$ymax, n=-1))
             data$labelPosition <- (data$ymax + data$ymin) / 2
             data$label <- paste0(data$des_agregacion_paises_nacionalidad, "\n", data$nStu)
             data <- na.omit(data)
             data[
               , ymax := NULL][
                 , ymin := NULL][
                   , label := NULL][
                     , labelPosition := NULL][
                     , year := as.integer(year)][
                       , nStu := as.integer(nStu)]
             setnames(data,
                      c('des_agregacion_paises_nacionalidad', 'year', 'nStu', 'fraction'),
                      c('Región', 'Año', 'Estudiantes', 'Proporción'))
             
           },
           Paises = {
             temp.dt <- rbind(
               data.table(des_pais_nacionalidad  = 'Otros',
                          year = input$year,
                          nStu = nContPais.lst[[input$university]][year %in% input$year & !des_pais_nacionalidad %in% 'España', sum(nStu)]),
               nContPais.lst[[input$university]][year %in% input$year & des_pais_nacionalidad %in% 'España'])
             
             data <- temp.dt
             data$fraction <- data$nStu / sum(data$nStu)
             data$ymax <- cumsum(data$fraction)
             data$ymin <- c(0, head(data$ymax, n=-1))
             data$labelPosition <- (data$ymax + data$ymin) / 2
             data$label <- paste0(data$des_pais_nacionalidad, "\n", data$nStu)
             data <- na.omit(data)
             data[
               , ymax := NULL][
                 , ymin := NULL][
                   , label := NULL][
                     , labelPosition := NULL][
                     , year := as.integer(year)][
                       , nStu := as.integer(nStu)]
             setnames(data,
                      c('des_pais_nacionalidad', 'year', 'nStu', 'fraction'),
                      c('País', 'Año', 'Estudiantes', 'Proporción'))
             
           })
  })
  
  output$donut_chart_two_table <- renderTable({
    switch(input$continent_option,
           Continentes = {
             
             
             data <- nCont.lst[[input$university]][year %in% input$year & !des_continente_nacionalidad %in% 'Europa']
             data$fraction <- data$nStu / sum(data$nStu)
             data$ymax <- cumsum(data$fraction)
             data$ymin <- c(0, head(data$ymax, n=-1))
             data$labelPosition <- (data$ymax + data$ymin) / 2
             data$label <- paste0(data$des_continente_nacionalidad, "\n", data$nStu)
             data <- na.omit(data)
             data[
               , ymax := NULL][
                 , ymin := NULL][
                   , label := NULL][
                     , labelPosition := NULL][
                       , year := as.integer(year)][
                         , nStu := as.integer(nStu)]
             setnames(data,
                      c('des_continente_nacionalidad', 'year', 'nStu', 'fraction'),
                      c('Continente', 'Año', 'Estudiantes', 'Proporción'))
           },
           ContinentesDesagregados = {
             
             
             data <- nContAgr.lst[[input$university]][year %in% input$year & !des_agregacion_paises_nacionalidad %in% 'Europa meridional']
             
             temp.dt <- rbind(
               data.table(des_agregacion_paises_nacionalidad  = 'Otros',
                          year = input$year,
                          nStu = data[nStu < mean(nStu), sum(nStu)]),
               data[nStu > mean(nStu)]
             )
             data <- temp.dt
             
             
             data$fraction <- data$nStu / sum(data$nStu)
             data$ymax <- cumsum(data$fraction)
             data$ymin <- c(0, head(data$ymax, n=-1))
             data$labelPosition <- (data$ymax + data$ymin) / 2
             data$label <- paste0(data$des_agregacion_paises_nacionalidad, "\n", data$nStu)
             data <- na.omit(data)
             data[
               , ymax := NULL][
                 , ymin := NULL][
                   , label := NULL][
                     , labelPosition := NULL][
                       , year := as.integer(year)][
                         , nStu := as.integer(nStu)]
             setnames(data,
                      c('des_agregacion_paises_nacionalidad', 'year', 'nStu', 'fraction'),
                      c('Región', 'Año', 'Estudiantes', 'Proporción'))
           },
           Paises = {
             
             
             data <- nContPais.lst[[input$university]][year %in% input$year & !des_pais_nacionalidad %in% 'España']
             
             temp.dt <- rbind(
               data.table(des_pais_nacionalidad  = 'Otros',
                          year = input$year,
                          nStu = data[nStu < mean(nStu) + 5, sum(nStu)]),
               data[nStu > mean(nStu) + 5]
             )
             data <- temp.dt
             
             data$fraction <- data$nStu / sum(data$nStu)
             data$ymax <- cumsum(data$fraction)
             data$ymin <- c(0, head(data$ymax, n=-1))
             data$labelPosition <- (data$ymax + data$ymin) / 2
             data$label <- paste0(data$des_pais_nacionalidad, "\n", data$nStu)
             data <- na.omit(data)
             data[
               , ymax := NULL][
                 , ymin := NULL][
                   , label := NULL][
                     , labelPosition := NULL][
                       , year := as.integer(year)][
                         , nStu := as.integer(nStu)]
             setnames(data,
                      c('des_pais_nacionalidad', 'year', 'nStu', 'fraction'),
                      c('País', 'Año', 'Estudiantes', 'Proporción'))
           })
  })
  
  output$circle_map <- renderLeaflet({


    temp.dt <- coordPais.dt
    
    data.dt <- nContPais.lst[[input$university]][year %in% as.vector(input$year)][temp.dt, on = 'des_pais_nacionalidad'] %>%
      replace(is.na(.), 0)
    data.dt <- data.dt[nStu >= input$range_slider[1]]
    data.dt[, year := input$year]
    

    if(nrow(data.dt) > 0){
    
      
      data.dt <- data.dt  %>% 
        dplyr::mutate(label = paste("CNTY_ID", "NAME", sep =  "<br>"))
      
      
      labels <- sprintf(
        "<strong>País: </strong> %s <br/> <strong>Estudiantes: </strong> %g",
        data.dt$des_pais_nacionalidad, data.dt$nStu
      ) %>% lapply(htmltools::HTML)
      
      pal <- colorBin(rev(hcl.colors(9, "RedOr")), data.dt$nStu, bins = c(0, 1, 2, 3, 4, 5, 10, 25, 100, Inf))
  
   
      leaflet(data = data.dt) %>% addTiles(group = "OpenStreetMap") %>% 
        addProviderTiles(providers$HERE.normalDayMobile) %>%
        addCircleMarkers(~lon, ~lat,
                         label = labels, color =  ~pal(nStu),
                         radius = ~20, clusterOptions = markerClusterOptions(),
                         stroke = FALSE, fillOpacity = 1) %>%
        
        addLayersControl(baseGroups = c("OpenStreetMap"),
                         overlayGroups = c("Leyenda")) %>%
        addLegend(title = "Estudiantes", pal = pal, values = ~nStu, group = "Leyenda", position = "topright", opacity = 1)
    }
    
  })
  
  output$circle_map_table <- renderTable({
    temp.dt <- coordPais.dt
    
    data.dt <- nContPais.lst[[input$university]][year %in% as.vector(input$year)][temp.dt, on = 'des_pais_nacionalidad'] %>%
      replace(is.na(.), 0)

    
    pal <- c( "#FF8000", "#FF0000", "#CCFFE5", "#0000CC", "#FF07FF")
    
    ran <- c(-Inf, 100, 200, 300, 400, Inf)
    
    data.dt[, nStu_cat := cut(data.dt[, nStu], breaks = ran, labels = pal)]
    data.dt[, nStu_c := cut(data.dt[, nStu], breaks = ran, labels = 1:5)]
    data.dt[, nStu_c := as.numeric(nStu_c)]
    
    
    data.dt <- data.dt  %>% 
      dplyr::mutate(label = paste("CNTY_ID", "NAME", sep =  "<br>"))
    
    
    labels <- sprintf(
      "<strong>País: </strong> %s <br/> <strong>Estudiantes: </strong> %g",
      data.dt$des_pais_nacionalidad, data.dt$nStu
    ) %>% lapply(htmltools::HTML)
    
    data.dt[
      , year := as.integer(year)][
      , nStu := as.integer(nStu)][
      , nStu_cat := NULL][
      , nStu_c := NULL][
      , label := NULL]
    
    data.dt <- data.dt[nStu > 0]
    setnames(data.dt,
             c('des_pais_nacionalidad', 'year', 'nStu', 'lat', 'lon'),
             c('País', 'Año', 'Estudiantes', 'Latitud', 'Longitud'))
  })

  output$origin_continent <- renderPlotly({
    theme <- get_theme(event_data)
    ggtheme <- generate_ggtheme(theme)
    
    data.dt <- nContAgr.lst[[input$university]][year %in% input$year & !des_agregacion_paises_nacionalidad %in% 'Europa meridional']
    data.dt[, des_agregacion_paises_nacionalidad := factor(des_agregacion_paises_nacionalidad, levels = data.dt[data.dt[, order(desc(nStu))]]$des_agregacion_paises_nacionalidad)]
    
    abrev.dt <- data.table(des_agregacion_paises_nacionalidad =
                 c("América Latina y el Caribe", "Europa meridional",         
                   "África septentrional",       "Europa oriental",           
                   "África Subsahariana",        "Europa occidental",         
                   "Asia oriental",              "Europa septentrional",      
                   "Asia central",               "América septentrional",     
                   "Asia occidental",            "Asia sudoriental",          
                   "Asia meridional"),
               Abreviatura = c('AmeLat', 'EurMer',
                               'AfrSep', 'EurOri',
                               'AfrSub', 'EurOcc',
                               'AsiOri', 'EurSep',
                               'AsiCen', 'AmeSep',
                               'AsiOcc', 'AsiSud',
                               'AsiMer'))
    data.dt <- data.dt[abrev.dt, on = 'des_agregacion_paises_nacionalidad']
    
    data.dt[, Abreviatura := factor(Abreviatura, levels = data.dt[data.dt[, order(desc(nStu))]]$Abreviatura)]
    
    setnames(data.dt, c('des_agregacion_paises_nacionalidad', 'nStu'), c('Region','Estudiantes'))

    data.dt <- na.omit(data.dt)
    
    g <- ggplot(data=data.dt,
                aes(x=Abreviatura, Region = Region, y=Estudiantes)) +
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
  })
  
  output$origin_country <- renderPlotly({
    theme <- get_theme(event_data)
    ggtheme <- generate_ggtheme(theme)
    
    
    data.dt <- nContPais.lst[[input$university]][year %in% input$year & !des_pais_nacionalidad %in% 'España']
    data.dt[, des_pais_nacionalidad := factor(des_pais_nacionalidad, levels = data.dt[data.dt[, order(desc(nStu))]]$des_pais_nacionalidad)]
    
    abrev.dt <- data.table(des_pais_nacionalidad =
                             c("Venezuela (República Bolivariana de)",
                               "España",
                               "Colombia",
                               "Túnez",
                               "Marruecos",
                               "Bulgaria",
                               "Federación de Rusia",
                               "Gabón" ,                                         
                               "Rumania"                                        ,
                               "Italia"                                         ,
                               "Portugal"                                       ,
                               "Honduras"                                       ,
                               "Alemania"                                       ,
                               "Chile"                                          ,
                               "China"                                          ,
                               "Reino Unido de Gran Bretaña e Irlanda del Norte",
                               "Nicaragua"                                      ,
                               "República de Corea"                             ,
                               "Lituania"                                       ,
                               "Brasil"                                         ,
                               "Perú"                                           ,
                               "México"                                         ,
                               "Ecuador"                                        ,
                               "Ucrania"                                        ,
                               "Panamá"                                         ,
                               "Kazajstán"                                      ,
                               "Estados Unidos de América"                      ,
                               "Argentina"                                      ,
                               "República Dominicana"                           ,
                               "Guinea Ecuatorial"                              ,
                               "Francia"                                        ,
                               "Polonia"                                        ,
                               "Paraguay"                                       ,
                               "Jordania"                                       ,
                               "Uzbekistán"                                     ,
                               "Bolivia (Estado Plurinacional de)"              ,
                               "Dominica"                                       ,
                               "Estonia"                                        ,
                               "Armenia"                                        ,
                               "República de Moldova"                           ,
                               "Argelia"                                        ,
                               "Andorra"                                        ,
                               "Bélgica"                                        ,
                               "Viet Nam"                                       ,
                               "Kirguistán"                                     ,
                               "India"                                          ,
                               "Bangladesh"                                     ,
                               "República Árabe Siria"                          ,
                               "El Salvador"                                    ,
                               "Taiwan (Provincia de China)"                    ,
                               "Georgia"                                        ,
                               "Cuba"                                           ,
                               "Kenya"                                          ,
                               "Uruguay"                                        ,
                               "Suecia"                                         ,
                               "República Democrática del Congo"                ,
                               "Guatemala"                                      ,
                               "Nigeria"                                        ,
                               "Japón"                                          ,
                               "Angola"                                         ,
                               "Egipto"                                         ,
                               "Dinamarca"                                      ,
                                "Indonesia"                                     , 
                               "Países Bajos"                                   
                               ),
                           Abreviatura = c("VEN",
                                           "ESP",
                                           "COL",
                                           "TUN",
                                           "MAR",
                                           "BGR",
                                           "RUS",
                                           "GAB",
                                           "ROM",
                                           "ITA",
                                           "PRT",
                                           "HND",
                                           "DEU",
                                           "CHL",
                                           "CHN",
                                           "GBR",
                                           "NIC",
                                           "KOR",
                                           "LTU",
                                           "BRA",
                                           "PER",
                                           "MEX",
                                           "ECU",
                                           "UKR",
                                           "PAN",
                                           "KAZ",
                                           "USA",
                                           "ARG",
                                           "DOM",
                                           "GNQ",
                                           "FRA",
                                           "POL",
                                           "PRY",
                                           "JOR",
                                           "UZB",
                                           "BOL",
                                           "DMA",
                                           "EST",
                                           "ARM",
                                           "MDA",
                                           "DZA",
                                           "AND",
                                           "BEL",
                                           "VNM",
                                           "KGZ",
                                           "IND",
                                           "BGD",
                                           "SYR",
                                           "SLV",
                                           "TWN",
                                           "GEO",
                                           "CUB",
                                           "KEN",
                                           "URY",
                                           "SWE",
                                           "ZAR",
                                           "GTM",
                                           "NGA",
                                           "JPN",
                                           "AGO",
                                           "EGY",
                                           "DNK",
                                           "IDN",
                                           "NLD"
                                           ))
    data.dt <- data.dt[abrev.dt, on = 'des_pais_nacionalidad']
    data.dt[, Abreviatura := factor(Abreviatura, levels = data.dt[data.dt[, order(desc(nStu))]]$Abreviatura)]
    setnames(data.dt, c('des_pais_nacionalidad', 'nStu'), c('Pais','Estudiantes'))
    data.dt <- na.omit(data.dt)
    
    g <- ggplot(data=data.dt,
                aes(Pais = Pais, y=Estudiantes, x = Abreviatura)) +
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
