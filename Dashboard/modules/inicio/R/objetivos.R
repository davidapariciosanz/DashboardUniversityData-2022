ui_objetivos <- function(){
  column(
    width = 6L,
    h2("Objetivos", class = "shidashi-anchor"),
    p("Una vez planetada la estructura de la aplicación, es importante fijar los
      objetivos que tiene que cumplir, las personas a las que está dirigido y
      las preguntas que permitirá responder, entre muchas otras."),
    p("He dado mucha importancia a la facilidad de uso, disponiendo los gráficos
       y los menús de selección de una forma sencilla y visual para que todo el
       mundo, sin necesidad de conocimientos previos pueda explorar estos datos.
       Puede ser útil para personas que vayan a tener la selectividad y les
       interese ver las notas de corte o el número de estudiantes de las
       titulaciones. También puede resultar de interés para personas extranjeras
       que quieran estudiar en España ya que así pueden ver cuántas personas de
       su país han estudiado aquí otros años. En conclusión, al disponer de
       tantos gráficos, esta aplicación está orientaa a todo el mundo ya que no
       se puede pensar en un tipo de persona genérico al que vaya dirigido."),
    p("Otro objetivo es que la aplicación permita responder a las siguientes
      preguntas para cada universidad y cada año."),
    div(
      tags$ul(
        tags$li(
          span(
            class = "inline-all",
            "¿Cuántos estudiantes han accedido a la universidad?")
        ),
        tags$li(
          span(
            class = "inline-all",
            "¿Cuál es la titulación o la facultad con más estudiantes?")
        ),
        tags$li(
          span(
            class = "inline-all",
            "¿Cómo puedo comparar el número de estudiantes de distintas titulaciones o facultades a lo largo del tiempo?")
        ),
        tags$li(
          span(
            class = "inline-all",
            "¿Cómo puedo comparar geográficamente la densidad de estudiantes de las facultades?")
        ),
        tags$li(
          span(
            class = "inline-all",
            "¿Cuál es el país, región o continente de origen de los nuevos estudiantes?")
        ),
        tags$li(
          span(
            class = "inline-all",
            "¿Cómo puedo comparar geográficamente la densidad de estudiantes según el país?")
        ),
        tags$li(
          span(
            class = "inline-all",
            "¿Con qué tipo de acceso entraron los nuevos estudiantes?")
        ),
        tags$li(
          span(
            class = "inline-all",
            "¿Cómo puedo comparar geográficamente de qué municipios, provincias o comunidades autónomas provienen los nuevos estudiantes?")
        ),
        tags$li(
          span(
            class = "inline-all",
            "¿Cuál ha sido la nota de corte de cada titulación?")
        ),
        tags$li(
          span(
            class = "inline-all",
            "¿Cuál es la distribución que siguen las notas de acceso de los estudiantes para cada titulación?")
        )
      )
    )
  )
}
