ui_paginas <- function(){
  column(
    width = 6L,
    h2("Páginas", class = "shidashi-anchor"),
    h5("1. Titulaciones"),
    p("Ofrece gráficos sobre las titulaciones y las facultades en
      las que se imparten. En ella podemos ver series temporales del número de
      nuevos estudiantes para cada año, de la titulación y la facultad con
      más estudiantes en el año seleccionado. También hay series temporales
      múltiples para las titulaciones y las facultades. Otro gráfico es un mapa
      de burbujas del número de nuevos estudiantes en cada facultad."),
    h5("2. Lugares"),
    p("Dispone de dos diagramas de barras que contienen el número
      de nuevos estudiantes en función del país del que provinenen 
      (sin incluir a España) y del continente agregado
      (sin incluir a Europa meridional). También hay dos gráficos de anillos que
      pueden representar los continentes, los continentes agregados o regiones y
      los países. El primer gráfico representa el número de estudiantes
      procedentes de España, Europa meridional o Europa contra el resto de
      estudiantes agrupados en una categoría, mientras que el segundo gráfico
      representa esa agrupación desglosada."),
    h5("3. Acceso"),
    p("Contiene series temporales múltiples que representan el número de
      estudiantes según el municipio en el que realizaron los estudios previos a
      la universidad. También contiene dos mapas coropléticos que representan la
      anterior variable según las provincias y las comunidades autónomas.
      Existen unas barras de progreso que indican el método de acceso de los
      estudiantes."),
    h5("4. Notas"),
    p("Esta página es muy útil ya que para cada carrera podemos ver la nota de
       corte mediante series temporales y la distribución de las notas de acceso
      global y según el año mediante histogramas y diagramas de cajas y bigotes.
      ")
  )
}
