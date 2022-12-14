library(shiny)

# Debug
if(FALSE){
  template_settings$set(
    'root_path' = "inst/template/"
  )
}

server <- function(input, output, session){

  # Sync input ID
  shared_data <- shidashi::register_session_id(session)
  shared_data$enable_broadcast()
  shared_data$enable_sync()

  # Fixed usage, call modules
  shiny::observeEvent(session$clientData$url_search, {
    req <- list(QUERY_STRING = session$clientData$url_search)
    resource <- shidashi::load_module(request = req)
    if(resource$has_module){

      module_table <- shidashi::module_info()
      module_table <- module_table[module_table$id %in% resource$module$id, ]
      if(nrow(module_table)){
        group_name <- as.character(module_table$group[[1]])
        if(is.na(group_name)){
          group_name <- "<no group>"
        }
        if(system.file(package = "logger") != ''){
          logger::log_info("Loading - { module_table$label[1] } ({group_name}/{ module_table$id })")
        }
        shiny::moduleServer(resource$module$id, resource$module$server, session = session)
      }
    }
  })
}
