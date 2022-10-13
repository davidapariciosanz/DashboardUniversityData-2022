ui_presentacion <- function(){
  column(
    width = 6L,
    
    h2("Presentación", class = "shidashi-anchor"),
    p(
      "Soy David Aparicio Sanz, estudiante del Doble Grado en
      Ingeniería Informática y Estadística en la Universidad de Valladolid. Me
      encanta la ciencia de datos, así que he decidido participar en este
      concurso para plantear preguntas acerca de los datos y responderlas con
      métodos innovadores, ampliando mis conocimientos durante todo el proceso."
    ),
    fluidRow(
      column(
        width = 1L,
        div(
          class = "border padding-5 fill-width",
          a(img(src = "shidashi/img/github.png", width = "100%"), href="https://github.com/davidapariciosanz")
        )
      ),
      column(
        width = 1L,
        div(
          class = "border padding-5 fill-width",
          a(img(src = "shidashi/img/gmail.png", width = "100%"), href="mailto:davidapariciosanz01@gmail.com")
        )
      ),
      column(
        width = 1L,
        div(
          class = "border padding-5 fill-width",
          a(img(src = "shidashi/img/linkedln.png", width = "100%"), href="https://es.linkedin.com/in/david-aparicio-sanz-63a853146")
        )
      ),
      column(
        width = 1L,
        div(
          class = "border padding-5 fill-width",
          a(img(src = "shidashi/img/stackoverflow.png", width = "100%"), href="https://stackoverflow.com/users/20133069/david-aparicio-sanz")
        )
      ),
      column(
        width = 1L,
        div(
          class = "border padding-5 fill-width",
          a(img(src = "shidashi/img/twitter.png", width = "100%"), href="https://twitter.com/davidapasanz")
        )
      )
    )
  )
}
