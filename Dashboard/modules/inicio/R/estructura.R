ui_estructura <- function(){
  column(
    width = 6L,
    h2("Estructura de la aplicación", class = "shidashi-anchor"),
    fluidRow(
      column(
        width = 12L,
        div(
          class = "border padding-5 fill-width",
          a(img(src = "shidashi/img/app_structure.png", width = "100%"), href="https://www.google.com/")
        )
      )
    )
  )
}
