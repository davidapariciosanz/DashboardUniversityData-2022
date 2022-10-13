ui_proyecto <- function(){
  column(
    width = 6L,
    h2("Proyecto", class = "shidashi-anchor"),
    p(
      "Tras revisar los numerosos conjuntos de datos disponibles en UniversiDATA
      decidí centrar mi propuesta en los estudiantes que se matriculan por
      primera vez en cada titulación."
    ),
    p(
      "Disponemos de los datos de acceso a la universidad de 5 años para 5
      universidades. De la universidad de huelva sólo tenemos datos de 1 año por
      lo que he decidido no incluirla en el análisis. Además, cada conjunto de
      datos tiene 78 columnas y tantas filas como estudiantes se hayan
      matriculado por primera vez en cierta universidad en
      cierto año. Como he utilizado datos de 5 universidades y para cada una
      tenemos información de 5 años, resultan ser 25 conjuntos de datos así que
      he considerado que la mejor forma de analizar estos datos es con un
      dashboard interactivo."
    ),
    p("La estructura de este proyecto será modular para permitir tanto su 
      mantenimiento como su evolución y en la medida de lo posible se perseguirá
      la automatización de cada paso de producción.")
  )
}
