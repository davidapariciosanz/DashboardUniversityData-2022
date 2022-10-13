ui_funcionalidades <- function(){
  column(
    width = 6L,
    h2("Funcionalidades", class = "shidashi-anchor"),
    h5("1. Menú principal"),
    p(style="text-align: justify;", "El menú principal es común a todas las
      páginas por lo que es necesario conocer su funcionamiento para
      aprovecharlo al máximo. Está compuesto por la barra lateral de navegación
      que permite buscar la página deseada u acceder a ella haciendo click.
      También dispone de una barra superior de opciones con tres funcionalidades
      que explicaré de izquierda a derecha."),
    p(style="text-align: center;", "La primera opción permite abrir y cerrar la
      barra lateral de navegación."),
    fluidRow(
      column(
        width = 5,
        div(
          class = "border padding-5 fill-width",
          img(src = "shidashi/img/1_01.png", width = "100%")
        )
      ),
      column(
        width = 2,
        flex_container(
          direction = "column",
          class = "fill",
          flex_item(flex = 100, ""),
          flex_item(style = "text-align: center;",
                    span(class="btn btn-default", "<>")),
          flex_item(flex = 100, "")
        )
      ),
      column(
        width = 5,
        div(
          class = "border padding-5 fill-width",
          img(src = "shidashi/img/1_02.png", width = "100%")
        )
      ),
    ),
    br(),
    p(style="text-align: center;", "La segunda opción permite cambiar el tema de
      la aplicación de oscuro a claro y viceversa"),
    fluidRow(
      column(
        width = 5,
        div(
          class = "border padding-5 fill-width",
          img(src = "shidashi/img/1_01.png", width = "100%")
        )
      ),
      column(
        width = 2,
        flex_container(
          direction = "column",
          class = "fill",
          flex_item(flex = 100, ""),
          flex_item(style = "text-align: center;",
                    span(class="btn btn-default", "<>")),
          flex_item(flex = 100, "")
        )
      ),
      column(
        width = 5,
        div(
          class = "border padding-5 fill-width",
          img(src = "shidashi/img/1_03.png", width = "100%")
        )
      ),
    ),
    br(),
    p(style="text-align: center;", "La tercera opción permite visualizar la
      aplicación en modo pantalla completa, es decir, sin la barra horizontal ni
      la vertical. En esta ventana disponemos de una barra de navegación
      horizontal para cambiar entre las páginas que tenemos activas y una cruz
      en el lateral izquierdo que permite cerrar todas las ventanas o cerrar
      todas las ventanas menos la actual."),
    fluidRow(
      column(
        width = 5,
        div(
          class = "border padding-5 fill-width",
          img(src = "shidashi/img/1_01.png", width = "100%")
        )
      ),
      column(
        width = 2,
        flex_container(
          direction = "column",
          class = "fill",
          flex_item(flex = 100, ""),
          flex_item(style = "text-align: center;",
                    span(class="btn btn-default", "<>")),
          flex_item(flex = 100, "")
        )
      ),
      column(
        width = 5,
        div(
          class = "border padding-5 fill-width",
          img(src = "shidashi/img/1_04.png", width = "100%")
        )
      ),
    ),
    br(),
    h5("2. Gráficos"),
    p("En todas las páginas podemos seleccionar la universidad y el curso
    académico para la generación de los gráficos. La mayoría de gráficos son
      interactivos, lo que significa que muestran información relevante al pasar
      el cursor sobre ellos."),
    
    p(style="text-align: center;", "Algunas ventanas disponen de uno o varios de
      estos botones. El primer botón permite de ver la tabla de datos utilizada
      para representar el gráfico, el siguiente permite maximizar el gráfico
      para visualizarle en pantalla completa y el último muestra opciones que
      modifican los gráficos."),
    fluidRow(
      column(
        width = 3
      ),
      column(
        width = 6,
        div(
          class = "border padding-5 fill-width",
          img(src = "shidashi/img/2_01.png", width = "100%")
        )
      ),
      column(
        width = 3
      ),
    ),
    br(),
    p(style="text-align: center;", "Hay gráficos que muestran una barra de
      herramientas al pasar el cursor sobre ellos. La cámara descarga el gráfico
      en formato png, la lupa realiza zoom sobre el área seleccionada, las
      cuatro flechas permiten mover el punto de referencia de visualización del
      gráfico. Los pinceles activan la opción de dibujar rectas y lienzo libre
      respectivamente, el más y el menos permiten añadir o quitar un nivel de
      zoom respectivamente. Las cuatro flechas dentro de un cuadrado activa la
      escala automática y la casa resetea los ejes. Los dos últimas opciones
      muestran una ventana emergente con información detallada de la
      observación que se está seleccionando con el cursor o de todas las
      observaciones con ese mismo valor en el eje de abscisas."),
    fluidRow(
      column(
        width = 12,
        div(
          class = "border padding-5 fill-width",
          img(src = "shidashi/img/2_02.jpg", width = "100%")
        )
      )
    )
  )
}
