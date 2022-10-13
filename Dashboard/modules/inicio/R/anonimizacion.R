ui_anonimizacion <- function(){
  column(
    width = 6L,
    h2("Proceso de anonimización", class = "shidashi-anchor"),
    p(
      "Como los conjuntos de datos están anonimizados hay que interpretar los
      datos de una forma distinta ya que las variables se dividen en grupos.
      Un grupo tiene variables pivote y el resto de grupos son bloques, por lo
      que hay que considerar los siguientes puntos:"
    ),
    div(
      tags$ul(
        tags$li(
          span(
            class = "inline-all",
            "Se mantiene la relación entre las variables pivote y todas las demás variables del dataset")
        ),
        tags$li(
          span(
            class = "inline-all",
            "Se mantiene entre las variables que comparten un mismo bloque de coherencia")
        ),
        tags$li(
          span(
            class = "inline-all",
            "No se mantiene la relación entre variables no pivote que NO comparten un mismo bloque de coherencia")
        )
      )
    ),
    p("Además, hay que tener en cuenta que el proceso de anonimización conlleva
      a la pérdida de datos, por tanto puede haber inconsistencias en algunas
      situaciones."
      
    )
  )
}
