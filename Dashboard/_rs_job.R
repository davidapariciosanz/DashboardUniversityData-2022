shidashi::template_settings$set('root_path' = getwd())
options("crayon.enabled" = TRUE)
options("crayon.colors" = 256)
shiny::runApp(host = "127.0.0.1", port = 8310L, launch.browser = TRUE, 
    test.mode = FALSE, appDir = getwd())
