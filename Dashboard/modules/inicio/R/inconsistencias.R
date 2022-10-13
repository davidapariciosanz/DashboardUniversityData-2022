ui_inconsistencias <- function(){
  column(
    width = 6L,
    h2("Inconsistencias", class = "shidashi-anchor"),
    p("En este apartado se expondrán las inconsistencias encontradas en los
      datos gracias a la visualización del dashboard. Pueden deberse al proceso
      de anonimización, a una mala recogida o al proceso de almacenamiento de
      los mismos."),
    div(
      tags$ul(
        tags$li(
          span(
            class = "inline-all",
            "Ventana Titulaciones"),
          tags$ul(
            tags$li(
              span(
                class = "inline-all",
                "Universidad Complutense de Madrid"),
              p(""),
              p("En 2020 la siguiente facultad tuvo cero estudiantes:"),
              p("Centro Universitario Villanueva"),
              
              p("En 2021 la siguiente facultad tuvo cero estudiantes:"),
              p("Centro Universitario Villanueva")
            ),
            tags$li(
              span(
                class = "inline-all",
                "Universidad Rey Juan Carlos"),
              p(""),
              p("En 2018 las siguientes facultades tuvieron cero estudiantes:"),
              p("Escuela Tecnica Superior De Ingenieria Informatica"),
              p("Escuela Universitaria De Turismo Eserp"),
              
              p("En 2018 las siguientes facultades tuvieron cero estudiantes:"),
              p("Escuela Tecnica Superior De Ingenieria Informatica"),
              p("Escuela Universitaria De Turismo Eserp"),
              
              p("En 2018 las siguientes facultades tuvieron cero estudiantes:"),
              p("Escuela Superior De Ingenieria Informatica"),
              p("Escuela Tecnica Superior De Ingenieria De Telecomunicaciones"),
              
              p("En 2018 las siguientes facultades tuvieron cero estudiantes:"),
              p("Escuela Superior De Ingenieria Informatica"),
              p("Escuela Tecnica Superior De Ingenieria De Telecomunicaciones"),
              
              p("En 2018 las siguientes facultades tuvieron cero estudiantes:"),
              p("Escuela Superior De Gestion Empresarial Y Marketing (Esic)"),
              p("Escuela Superior De Ingenieria Informatica"),
              p("Escuela Tecnica Superior De Ingenieria De Telecomunicaciones")
            ),
            tags$li(
              span(
                class = "inline-all",
                "Universidad de Valladolid"),
              p(""),
              p("En 2017 la siguiente facultad tuvo cero estudiantes:"),
              p("Facultad De Ciencias De La Salud De Soria"),
              
              p("En 2018 las siguientes facultades tuvieron cero estudiantes:"),
              p("	Escuela Universitaria De Ingenieria Tecnica Agricola Inea (Adscrita A La Uva)"),
              p("Facultad De Ciencias De La Salud De Soria"),
              
              p("En 2019 las siguientes facultades tuvieron cero estudiantes:"),
              p("	Escuela Universitaria De Ingenieria Tecnica Agricola Inea (Adscrita A La Uva)"),
              p("Facultad De Enfermeria De Soria"),
              p("Facultad De Fisioterapia De Soria"),
              
              p("En 2020 las siguientes facultades tuvieron cero estudiantes:"),
              p("	Escuela Universitaria De Ingenieria Tecnica Agricola Inea (Adscrita A La Uva)"),
              p("Facultad De Enfermeria De Soria"),
              p("Facultad De Fisioterapia De Soria"),
              
              p("En 2021 las siguientes facultades tuvieron cero estudiantes:"),
              p("	Escuela Universitaria De Ingenieria Tecnica Agricola Inea (Adscrita A La Uva)"),
              p("Facultad De Enfermeria De Soria"),
              p("Facultad De Fisioterapia De Soria")
            )
          ),
          tags$li(
            span(
              class = "inline-all",
              "Ventana Acceso"),
            tags$ul(
              tags$li(
                span(
                  class = "inline-all",
                  "Universidad Carlos III de Madrid"),
                p(""),
                p("En 2018 sólo contiene 4 datos sobre la provincia o comunidad
                   en que los estudiantes realizaron los estudios previos a la universidad."),
                p("En 2019 contiene cero datos sobre la provincia o comunidad
                   en que los estudiantes realizaron los estudios previos a la universidad."),
                p("En 2020 sólo contiene 5 datos sobre la provincia o comunidad
                   en que los estudiantes realizaron los estudios previos a la universidad."),
                p("En 2017, 2018, 2019, 2020 y 2021 contiene cero datos sobre el
                   tipo de acceso con el que los estudiantes entraron a la
                   universidad."),
              ),
              tags$li(
                span(
                  class = "inline-all",
                  "Universidad de Valladolid"),
                p(""),
                p("En 2017 sólo contiene 78 datos sobre la provincia o comunidad
                   en que los estudiantes realizaron los estudios previos a la universidad."),
                p("En 2019 contiene cero datos sobre el tipo de acceso con el
                  que los estudiantes entraron a la universidad."),
              )
            )
          ),
          tags$li(
            span(
              class = "inline-all",
              "Ventana Notas"),
            tags$ul(
              tags$li(
                span(
                  class = "inline-all",
                  "Todas las universidades"),
                p(""),
                p("Algunas notas de corte no coinciden con las reales ya que las
                  he buscado en las páginas de las universidades. La nota de
                  corte ha sido calculada como la mínima nota, de cada
                  titulación para cada año.")
              )
            )
          )
        )
      )
    )
  )
}
