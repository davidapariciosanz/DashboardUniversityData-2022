# Prevent scientific notation
options(scipen=999) 

# Load packages                                                                 ----
library(data.table)
library(readxl)
library(dplyr)
library(geojsonio)
library(stringr)
library(readr)

# Load paths                                                                    ----
path_root <- getwd()
path_data <- file.path(path_root, 'data')

path_rds <- file.path(path_root, 'rds')

path_uva <- file.path(path_data, 'uva')
path_urjc <- file.path(path_data, 'urjc')
path_uam <- file.path(path_data, 'uam')
path_ucm <- file.path(path_data, 'ucm')
path_uc3m <- file.path(path_data, 'uc3m')

# Load parameters                                                               ----
uni.vec <- c('uam', 'uc3m', 'ucm', 'urjc', 'uva')
year.vec <- 2017:2021

saveRDS(uni.vec, file.path(path_rds, 'uni.vec.RDS'))
saveRDS(year.vec, file.path(path_rds, 'year.vec.RDS'))

uva_files <- list.files(path_uva)
uva_years <- substr(uva_files, 12, 15)

urjc_files <- list.files(path_urjc)
urjc_years <- substr(urjc_files, 13, 16)

uam_files <- list.files(path_uam)
uam_years <- substr(uam_files, 12, 15)

ucm_files <- list.files(path_ucm)
ucm_years <- substr(ucm_files, 12, 15)

uc3m_files <- list.files(path_uc3m)
uc3m_years <- substr(uc3m_files, 13, 16)

pivot <- c('cod_centro', 'des_centro', 'cod_titulacion', 'des_titulacion',
           'cod_genero', 'des_genero')
block_1 <- c('cod_universidad', 'des_universidad', 'curso_academico')
block_2 <- c('cod_campus', 'des_campus', 'cod_centro', 'des_centro',
             'cod_naturaleza_centro', 'des_naturaleza_centro', 'cod_tipo_estudio',
             'des_tipo_estudio', 'cod_titulacion', 'des_titulacion')
block_3 <- c('cod_comunidad_centro_sec', 'des_comunidad_centro_sec',
             'cod_provincia_centro_sec', 'des_provincia_centro_sec',
             'cod_municipio_centro_sec', 'des_municipio_centro_sec',
             'lat_municipio_centro_sec', 'lon_municipio_centro_sec',
             'cod_naturaleza_centro_sec', 'des_naturaleza_centro_sec',
             'nota_admision')
block_4 <- c('cod_pais_fin_estudio_acceso', 'des_pais_fin_estudio_acceso',
             'lat_pais_fin_estudio_acceso', 'lon_pais_fin_estudio_acceso',
             'cod_continente_fin_estudio_acceso', 'des_continente_fin_estudio_acceso',
             'cod_agregacion_paises_fin_estudio_acceso', 'des_agregacion_paises_fin_estudio_acceso')
block_5 <- c('cod_estudio_acceso', 'des_estudio_acceso', 'cod_especialidad_acceso',
             'des_especialidad_acceso', 'des_tipo_especialidad_acceso',
             'cod_ocupacion_estudiante', 'des_ocupacion_estudiante')
block_6 <- 'anio_fin_estudio_acceso'
block_7 <- c('cod_forma_admision', 'des_forma_admision', 'ind_accede_al_SUE_por_1a_vez',
             'anio_acceso_SUE', 'ind_se_matricula_en_primero', 'ind_curso_adaptacion')
block_8 <- 'anio_nacimiento'
block_9 <- c('cod_pais_nacionalidad', 'des_pais_nacionalidad', 'lat_pais_nacionalidad',
             'lon_pais_nacionalidad', 'cod_continente_nacionalidad', 'des_continente_nacionalidad',
             'cod_agregacion_paises_nacionalidad', 'des_agregacion_paises_nacionalidad')
block_10 <- c('cod_pais_residencia', 'des_pais_residencia', 'lat_pais_residencia',
              'lon_pais_residencia', 'cod_continente_residencia', 'des_continente_residencia',
              'cod_agregacion_paises_residencia', 'des_agregacion_paises_residencia')
block_11 <- c('cod_dedicacion', 'des_dedicacion')
block_12 <- c('cod_familia_numerosa', 'des_familia_numerosa')
block_13 <- c('cod_nivel_estudios_madre', 'des_nivel_estudios_madre',
              'cod_ocupacion_madre', 'des_ocupacion_madre')
block_14 <- c('cod_nivel_estudios_padre', 'des_nivel_estudios_padre',
              'cod_ocupacion_padre', 'des_ocupacion_padre')

# Load data                                                                     ----
uva_data.lst <- vector(mode = 'list', length = length(uva_files))
names(uva_data.lst) <- uva_years
for(file in uva_files){
  uva_data.lst[[substr(file, 12, 15)]] <- data.table(read_excel(file.path(path_uva, file)))
}

urjc_data.lst <- vector(mode = 'list', length = length(urjc_files))
names(urjc_data.lst) <- urjc_years
for(file in urjc_files){
  urjc_data.lst[[substr(file, 13, 16)]] <- data.table(read_excel(file.path(path_urjc, file)))
}

uam_data.lst <- vector(mode = 'list', length = length(uam_files))
names(uam_data.lst) <- uam_years
for(file in uam_files){
  uam_data.lst[[substr(file, 12, 15)]] <- data.table(read_excel(file.path(path_uam, file)))
}

ucm_data.lst <- vector(mode = 'list', length = length(ucm_files))
names(ucm_data.lst) <- ucm_years
for(file in ucm_files){
  ucm_data.lst[[substr(file, 12, 15)]] <- data.table(read_excel(file.path(path_ucm, file)))
}

uc3m_data.lst <- vector(mode = 'list', length = length(uc3m_files))
names(uc3m_data.lst) <- uc3m_years
for(file in uc3m_files){
  uc3m_data.lst[[substr(file, 13, 16)]] <- data.table(read_excel(file.path(path_uc3m, file)))
}

# Block analysis                                                                ----
uva_data.dt <- rbindlist(uva_data.lst)
uva_data.dt[, ..pivot]
uva_data.dt[, ..block_1]
uva_data.dt[, ..block_2]
uva_data.dt[, ..block_3]
uva_data.dt[, ..block_4]
uva_data.dt[, ..block_5]
uva_data.dt[, ..block_6]
uva_data.dt[, ..block_7]
uva_data.dt[, ..block_8]
uva_data.dt[, ..block_9]
uva_data.dt[, ..block_10]
uva_data.dt[, ..block_11]
uva_data.dt[, ..block_12]
uva_data.dt[, ..block_13]
uva_data.dt[, ..block_14]

# Information                                                                   ----
inf.lst <- vector(mode = 'list', length = length(uni.vec))
names(inf.lst) <- uni.vec
inf.lst[['uva']] <- "La UVa es una universidad pública española, fue fundada en el año 1241, lo que la convierte en la segunda universidad más antigua de España. Su actividad se desarrolla en cuatro campus, situados en Castilla y León: Palencia, Segovia, Soria y Valladolid. Ofrece una amplia variedad de titulaciones de grado, máster y doctorado, con programas en todos los ámbitos del conocimiento. En total, se cursan 68 grados, 65 másteres y 29 programas de doctorado en sus 26 Facultades y Escuelas, 59 Departamentos, 12 Institutos Universitarios y 4 Centros de Investigación y Tecnológicos. Cuenta con cerca de 2.500 profesores y 20.000 estudiantes. Su apuesta por la formación permanente, su liderazgo en transparencia y movilidad y su extensa red de relaciones internacionales y sus prestigiosos centros de investigación conforman un entorno académico excepcional, comparable al de las más prestigiosas universidades europeas."
inf.lst[['urjc']] <- "La Universidad Rey Juan Carlos (por sus siglas, URJC) es una universidad pública española con sede en la Comunidad de Madrid. Cuenta con campus en Alcorcón, Aranjuez, Fuenlabrada, Madrid y Móstoles. Fundada en 1996, es la segunda universidad de Madrid por número de alumnos matriculados."
inf.lst[['uam']] <- "La Universidad Autónoma de Madrid es una universidad pública joven, fundada en 1968, que se encuentra en el norte de Madrid, a solo 20 minutos del centro. Ofrece una amplia oferta de estudios en 7 Facultades y una Escuela Politécnica Superior, y cada año más de 30.000 estudiantes elijen la UAM (de ellos más de 3.000 extranjeros), dada la reputación de su calidad tanto en educación como en investigación. Actualmente tiene más de 300 grupos de investigación, que junto con el proyecto Campus de Excelencia Internacional con el CSIC y la alianza internacional CIVIS, permite concentrar investigaciones de primer nivel internacional. La UAM cuenta con 12 centros de investigación con más de 2.300 profesores e investigadores con dedicación exclusiva. La UAM ocupa las primeras posiciones entre las universidades españolas en los principales ranking universitarios internacionales."
inf.lst[['uc3m']] <- "La UC3M es una universidad pública española que destaca en docencia, investigación e innovación. Ocupa el puesto 35 a nivel mundial en el ranking QS de las 50 mejores universidades del mundo con menos de 50 años y está incluida en el Times Higher Education (THE) 150 Under 50. Mantiene más de 870 acuerdos con universidades de 56 países, entre las que se encuentran algunas de las mejores del mundo según el ranking de la universidad de Shangai. El 20% de sus estudiantes son internacionales. La Universidad es una de las promotoras de la alianza Young Universities for the Future of Europe (YUFE), una de las alianzas seleccionadas por la UE en su European Universities Initiative. La UC3M cuenta con numerosas acreditaciones y distinciones de calidad, como el sello EUR-ACE en el ámbito de las ingenierías o la acreditación AACSB en los programas de empresa y finanzas, entre otros."
inf.lst[['ucm']] <- "La Universidad Complutense de Madrid (UCM), es la universidad pública más antigua de Madrid, considerada una de las universidades más importantes y prestigiosas de España y del mundo hispanohablante.​ Además, es actualmente la 3ª universidad de enseñanza presencial más grande de Europa."

saveRDS(inf.lst, file.path(path_rds, 'inf.lst.RDS'))

# Universities                                                                  ----

# uva
uva_data.dt <- rbindlist(uva_data.lst)
uva_data.dt[, year := as.numeric(substr(curso_academico, 0, 4))]
uva_data.dt[, des_titulacion := str_replace_all(
  str_to_title(des_titulacion),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))]
uva_data.dt[, des_centro := str_replace_all(
  str_to_title(des_centro),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))]
uva_data.dt[, des_municipio_centro_sec := str_to_title(des_municipio_centro_sec)]
uva_data.dt[des_provincia_centro_sec %in% 'Alicante/Alacant']$des_provincia_centro_sec <- 'Alicante'
uva_data.dt[des_provincia_centro_sec %in% 'Castellón/Castelló']$des_provincia_centro_sec <- 'Castellón'
uva_data.dt[des_provincia_centro_sec %in% 'Illes Balears']$des_provincia_centro_sec <- 'Islas Baleares'
uva_data.dt[des_provincia_centro_sec %in% 'Valencia/València']$des_provincia_centro_sec <- 'Valencia'
uva_data.dt[des_comunidad_centro_sec %in% 'Castilla León']$des_comunidad_centro_sec <- 'Castilla y León'
uva_data.dt[des_comunidad_centro_sec %in% 'Catalunya']$des_comunidad_centro_sec <- 'Cataluña'

# urjc
urjc_data.dt <- rbindlist(urjc_data.lst)
urjc_data.dt[, year := as.numeric(substr(curso_academico, 0, 4))]
urjc_data.dt[, des_titulacion := str_replace_all(
  str_to_title(des_titulacion),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))]
urjc_data.dt[, des_centro := str_replace_all(
  str_to_title(des_centro),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))]
urjc_data.dt[, des_municipio_centro_sec := str_to_title(des_municipio_centro_sec)]
urjc_data.dt[des_provincia_centro_sec %in% 'Alicante/Alacant']$des_provincia_centro_sec <- 'Alicante'
urjc_data.dt[des_provincia_centro_sec %in% 'Castellón/Castelló']$des_provincia_centro_sec <- 'Castellón'
urjc_data.dt[des_provincia_centro_sec %in% 'Illes Balears']$des_provincia_centro_sec <- 'Islas Baleares'
urjc_data.dt[des_provincia_centro_sec %in% 'Valencia/València']$des_provincia_centro_sec <- 'Valencia'
urjc_data.dt[des_comunidad_centro_sec %in% 'Castilla León']$des_comunidad_centro_sec <- 'Castilla y León'
urjc_data.dt[des_comunidad_centro_sec %in% 'Catalunya']$des_comunidad_centro_sec <- 'Cataluña'
urjc_data.dt[des_centro %in% "Escuela Superior De Ciencias Experimentales Y Tecnologia. Campus De Madrid"]$des_centro <- "Escuela Superior De Ciencias Experimentales Y Tecnologia"
urjc_data.dt[des_centro %in% "Facultad De Ciencias De La Comunicacion. Campus De Madrid"]$des_centro <- "Facultad De Ciencias De La Comunicacion" 
urjc_data.dt[des_centro %in% "Facultad De Ciencias De La Salud. Campus De Aranjuez"]$des_centro <- "Facultad De Ciencias De La Salud"
urjc_data.dt[des_centro %in% "Escuela Superior Eserp"]$des_centro <- "Escuela Universitaria De Turismo Eserp"  
urjc_data.dt[des_centro %in% "Escuela Tecnica Superior De Ingenieria Informatica. Campus De Madrid"]$des_centro <- "Escuela Tecnica Superior De Ingenieria Informatica"  
urjc_data.dt[des_centro %in% "Escuela Tecnica Superior De Ingenieria De Telecomunicacion. Campus Alcorcon"]$des_centro <- "Escuela Tecnica Superior De Ingenieria De Telecomunicaciones" 
urjc_data.dt[des_centro %in% "Escuela Tecnica Superior De Ingenieria De Telecomunicacion"]$des_centro <- "Escuela Tecnica Superior De Ingenieria De Telecomunicaciones" 
urjc_data.dt[des_centro %in% "Escuela Tecnica Superior De Ingenieria De Telecomunicaciones. Campus De Alcorcon"]$des_centro <- "Escuela Tecnica Superior De Ingenieria De Telecomunicaciones"                   
urjc_data.dt[des_centro %in% "Facultad De Ciencias Juridicas Y Sociales. Campus Alcorcon"]$des_centro <- "Facultad De Ciencias Juridicas Y Sociales"                  
urjc_data.dt[des_centro %in% "Facultad De Ciencias Juridicas Y Sociales. Campus De Aranjuez"]$des_centro <- "Facultad De Ciencias Juridicas Y Sociales"
urjc_data.dt[des_centro %in% "Facultad De Ciencias Juridicas Y Sociales. Campus Fuenlabrada"]$des_centro <- "Facultad De Ciencias Juridicas Y Sociales"
urjc_data.dt[des_centro %in% "Facultad De Ciencias Juridicas Y Sociales. Campus Mostoles"]$des_centro <- "Facultad De Ciencias Juridicas Y Sociales"
urjc_data.dt[des_centro %in% "Facultado De Ciencias Juridicas Y Sociales. Campus De Aranjuez"]$des_centro <- "Facultad De Ciencias Juridicas Y Sociales"

# uam
uam_data.dt <- rbindlist(uam_data.lst)
uam_data.dt[, year := as.numeric(substr(curso_academico, 0, 4))]
uam_data.dt[, des_titulacion := str_replace_all(
  str_to_title(des_titulacion),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))]
uam_data.dt[, des_centro := str_replace_all(
  str_to_title(des_centro),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))]
uam_data.dt[, des_municipio_centro_sec := str_to_title(des_municipio_centro_sec)]
uam_data.dt[des_provincia_centro_sec %in% 'Alicante/Alacant']$des_provincia_centro_sec <- 'Alicante'
uam_data.dt[des_provincia_centro_sec %in% 'Castellón/Castelló']$des_provincia_centro_sec <- 'Castellón'
uam_data.dt[des_provincia_centro_sec %in% 'Illes Balears']$des_provincia_centro_sec <- 'Islas Baleares'
uam_data.dt[des_provincia_centro_sec %in% 'Valencia/València']$des_provincia_centro_sec <- 'Valencia'
uam_data.dt[des_comunidad_centro_sec %in% 'Castilla León']$des_comunidad_centro_sec <- 'Castilla y León'
uam_data.dt[des_comunidad_centro_sec %in% 'Catalunya']$des_comunidad_centro_sec <- 'Cataluña'

# ucm
ucm_data.dt <- rbindlist(ucm_data.lst)
ucm_data.dt[, year := as.numeric(substr(curso_academico, 0, 4))]
ucm_data.dt[, des_titulacion := str_replace_all(
  str_to_title(des_titulacion),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))]
ucm_data.dt[, des_centro := str_replace_all(
  str_to_title(des_centro),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))]
ucm_data.dt[, des_municipio_centro_sec := str_to_title(des_municipio_centro_sec)]
ucm_data.dt[des_provincia_centro_sec %in% 'Alicante/Alacant']$des_provincia_centro_sec <- 'Alicante'
ucm_data.dt[des_provincia_centro_sec %in% 'Castellón/Castelló']$des_provincia_centro_sec <- 'Castellón'
ucm_data.dt[des_provincia_centro_sec %in% 'Illes Balears']$des_provincia_centro_sec <- 'Islas Baleares'
ucm_data.dt[des_provincia_centro_sec %in% 'Valencia/València']$des_provincia_centro_sec <- 'Valencia'
ucm_data.dt[des_comunidad_centro_sec %in% 'Castilla León']$des_comunidad_centro_sec <- 'Castilla y León'
ucm_data.dt[des_comunidad_centro_sec %in% 'Catalunya']$des_comunidad_centro_sec <- 'Cataluña'
ucm_data.dt[des_centro %in% "Escuela Universitaria De Magisterio \"Fomento De Centros De Enseñanza\""]$des_centro <- "Escuela Universitaria De Magisterio - Escuni"


# uc3m
uc3m_data.dt <- rbindlist(uc3m_data.lst)
uc3m_data.dt[, year := as.numeric(substr(curso_academico, 0, 4))]
uc3m_data.dt[, des_titulacion := str_replace_all(
  str_to_title(des_titulacion),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))]
uc3m_data.dt[, des_centro := str_replace_all(
  str_to_title(des_centro),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))]
uc3m_data.dt[, des_municipio_centro_sec := str_to_title(des_municipio_centro_sec)]
uc3m_data.dt[des_provincia_centro_sec %in% 'Alicante/Alacant']$des_provincia_centro_sec <- 'Alicante'
uc3m_data.dt[des_provincia_centro_sec %in% 'Castellón/Castelló']$des_provincia_centro_sec <- 'Castellón'
uc3m_data.dt[des_provincia_centro_sec %in% 'Illes Balears']$des_provincia_centro_sec <- 'Islas Baleares'
uc3m_data.dt[des_provincia_centro_sec %in% 'Valencia/València']$des_provincia_centro_sec <- 'Valencia'
uc3m_data.dt[des_comunidad_centro_sec %in% 'Castilla León']$des_comunidad_centro_sec <- 'Castilla y León'
uc3m_data.dt[des_comunidad_centro_sec %in% 'Catalunya']$des_comunidad_centro_sec <- 'Cataluña'
uc3m_data.dt[des_centro %in% "Facultad De Ciencias Sociales Y Juridicas. (Getafe)"]$des_centro <- "Facultad De Ciencias Sociales Y Juridicas (Colmenarejo)"

# Page 1                                                                        ----
## Degree arrays                                                                ----
allTit.lst <- vector(mode = 'list', length = length(uni.vec))
names(allTit.lst) <- uni.vec

# uva
care_uva.vec <- uva_data.dt[, sort(unique(des_titulacion))]
names(care_uva.vec) <- care_uva.vec
allTit.lst[['uva']] <- care_uva.vec
  
# urjc
care_urjc.vec <- urjc_data.dt[, sort(unique(des_titulacion))]
names(care_urjc.vec) <- care_urjc.vec
allTit.lst[['urjc']] <- care_urjc.vec

# uam
care_uam.vec <- uam_data.dt[, sort(unique(des_titulacion))]
names(care_uam.vec) <- care_uam.vec
allTit.lst[['uam']] <- care_uam.vec

# ucm
care_ucm.vec <- ucm_data.dt[, sort(unique(des_titulacion))]
names(care_ucm.vec) <- care_ucm.vec
allTit.lst[['ucm']] <- care_ucm.vec

# uc3m
care_uc3m.vec <- uc3m_data.dt[, sort(unique(des_titulacion))]
names(care_uc3m.vec) <- care_uc3m.vec
allTit.lst[['uc3m']] <- care_uc3m.vec

saveRDS(allTit.lst, file.path(path_rds, 'allTit.lst.RDS'))

## Faculties arrays                                                             ----
allFac.lst <- vector(mode = 'list', length = length(uni.vec))
names(allFac.lst) <- uni.vec

# uva
fac_uva.vec <- uva_data.dt[, sort(unique(des_centro))]
names(fac_uva.vec) <- fac_uva.vec
allFac.lst[['uva']] <- fac_uva.vec

# urjc
fac_urjc.vec <- urjc_data.dt[, sort(unique(des_centro))]
names(fac_urjc.vec) <- fac_urjc.vec
allFac.lst[['urjc']] <- fac_urjc.vec

# uam
fac_uam.vec <- uam_data.dt[, sort(unique(des_centro))]
names(fac_uam.vec) <- fac_uam.vec
allFac.lst[['uam']] <- fac_uam.vec

# ucm
fac_ucm.vec <- ucm_data.dt[, sort(unique(des_centro))]
names(fac_ucm.vec) <- fac_ucm.vec
allFac.lst[['ucm']] <- fac_ucm.vec

# uc3m
fac_uc3m.vec <- uc3m_data.dt[, sort(unique(des_centro))]
names(fac_uc3m.vec) <- fac_uc3m.vec
allFac.lst[['uc3m']] <- fac_uc3m.vec

saveRDS(allFac.lst, file.path(path_rds, 'allFac.lst.RDS'))

## Faculties-Coordinates                                                        ----
coordFac.lst <- vector(mode = 'list', length = length((uni.vec)))
names(coordFac.lst) <- uni.vec
uva_data.dt[, sort(unique(des_centro))]
# uva
coordFac_uva.dt <- data.table(des_centro = allFac.lst[['uva']],
                              lat = c(41.65698463840998,
                                      40.942920180070125, 
                                      41.663150801980464,
                                      41.65649449637725,
                                      41.65114265038973,
                                      41.9892341679705,
                                      41.66240611580216,
                                      41.99798248592585,
                                      41.60383942996183,
                                      41.66349554785002,
                                      41.75385875737127, 
                                      41.986392537712355,
                                      41.65878773999642,
                                      41.75262405943497,
                                      40.943180392834506, 
                                      41.65876246130263,
                                      41.65204483671496,
                                      41.98669142727475,
                                      40.942679241413366, 
                                      41.752888046820196,
                                      41.66222323191935,
                                      41.75336463472991, 
                                      41.65479175956627,
                                      41.65774812974966,
                                      41.75257403059164, 
                                      41.65503478686476,
                                      40.96189741700475),
                              lon = c(-4.714040290755967,
                                      -4.113794112190194,
                                      -4.704518274413881,
                                      -4.714552598402661,
                                      -4.740797987084718,
                                      -4.514076416091416,
                                      -4.705968605418073,
                                      -4.518483758493887,
                                      -4.792686460364791,
                                      -4.705413433904246,
                                      -2.4705418293575163,
                                      -4.517303515339668,
                                      -4.710344510743703,
                                      -2.469649002792383,
                                      -4.113494280196894,
                                      -4.7141385378544785,
                                      -4.721523011711138,
                                      -4.517015167135792,
                                      -4.114317223326721,
                                      -2.470072189297979,
                                      -4.707300360466019,
                                      -2.4705237652902508,
                                      -4.717664099666101,
                                      -4.713691411675138,
                                      -2.4689642341496616,
                                      -4.717030622022257,
                                      -5.666510993274852))
coordFac.lst[['uva']] <- coordFac_uva.dt

# urjc
coordFac_urjc.dt <- data.table(des_centro = allFac.lst[['urjc']],
                              lat = c(40.43459900783673,
                                      40.418772133438424,
                                      40.33642549515373,
                                      #40.336760797652246,
                                      40.43701886617984,
                                      40.33708357031226, 
                                      #40.458825436742636,
                                      #40.38956339206432,
                                      #40.38962895291142,
                                      40.389239443407035,
                                      #40.38906975539608,
                                      40.3899519646737,
                                      #40.39040957489332,
                                      40.4213766576651,
                                      40.45887203266203,
                                      40.334523146370046,
                                      #40.33402235328607,
                                      40.029735955353466,
                                      #40.02970309460623,
                                      40.40687467621611),
                                      #40.40683577098023, 
                                      #40.40745825205377, 
                                      #40.40738692638948, 
                                      #40.407257243169816, 
                                      #40.40673202357492),
                              lon = c(-3.6732477316743495,
                                      -3.690782874004485,
                                      -3.8766327028425644,
                                      #-3.876997483239319,
                                      -3.8014781874982866,
                                      -3.877144513886466,
                                      #-3.675096517022819,
                                      #-3.6290105816532003,
                                      #-3.6281093078373274,
                                      -3.6280333577816313,
                                      #-3.629020708310255,
                                      -3.6279962921742164,
                                      #-3.628141131449398,
                                      -3.689052274004394,
                                      -3.675695815585905,
                                      -3.878940008678527,
                                      #-3.879919260191073,
                                      -3.602149975864407,
                                      #-3.6013560420597073,
                                      -3.6099853078262183))
                                      #-3.610717639333857,
                                      #-3.610692092887084,
                                      #-3.609891637554867,
                                      #-3.6086824390742818,
                                      #-3.6084440055710685))
coordFac.lst[['urjc']] <- coordFac_urjc.dt

# uam
coordFac_uam.dt <- data.table(des_centro = allFac.lst[['uam']],
                              lat = c(40.467369577297426,
                                      40.54714280475394,
                                      40.447375884802305,
                                      40.43873467086105,
                                      40.49377477421758,
                                      40.545597292247955,
                                      40.54477807410744,
                                      40.54175879015607,
                                      40.54438160811528,
                                      40.54459931356121,
                                      40.48152904510565,
                                      40.54399042031376),
                              lon = c(-3.7815866605083786,
                                      -3.6916159028352062,
                                      -3.7096692451682984,
                                      -3.7190785672566,
                                      -3.70768941325622,
                                      -3.6950659144832687,
                                      -3.6971490844930557,
                                      -3.6910908163177676,
                                      -3.6983611316704916,
                                      -3.6977325163296997,
                                      -3.6907861451670776,
                                      -3.6924446893409013))
coordFac.lst[['uam']] <- coordFac_uam.dt                               

# ucm
coordFac_ucm.dt <- data.table(des_centro = allFac.lst[['ucm']],
                              lat = c(40.45074605067194, 
                                      40.49748895619215,
                                      40.43065624639605,
                                      40.45176093153232, 
                                      40.37823946713009,
                                      #40.37823946713009,
                                      40.44010062501105,
                                      40.44903294646385,
                                      40.43838940008543,
                                      40.44583099282849,
                                      40.433529474712735,
                                      40.45099477137511,
                                      40.44884524680592,
                                      40.44963839623426,
                                      40.43002431359666,
                                      40.45078891473123,
                                      40.439783269711405,
                                      40.45164715625414,
                                      40.45177851651153, 
                                      40.444161120414726,
                                      40.44699317383555,
                                      40.445938027833314,
                                      40.44883130834868,
                                      40.44890905620706,
                                      40.449289652652304,
                                      40.45288793943826,
                                      40.4440463206379,
                                      40.44246764042798,
                                      40.42588315033516,
                                      40.43220084408484,
                                      40.430935546039926,
                                      40.44954668786275,
                                      40.4188101790749,
                                      40.450810957653715,
                                      40.588626039206936),
                              lon = c(-3.717826100805606,
                                      -3.7041251682938805,
                                      -3.6761121200264513,
                                      -3.7201621099392463,
                                      -3.749967348396425,
                                      #-3.749967348396425,
                                      -3.7338924028389204,
                                      -3.7268357181794523,
                                      -3.701442574003817,
                                      -3.7284884758499333,
                                      -3.7894623758503774,
                                      -3.7260517874977697,
                                      -3.7255775451682354,
                                      -3.7258997181794142,
                                      -3.7895436470152797,
                                      -3.7240583028385683,
                                      -3.7117192874981604,
                                      -3.730736387497791,
                                      -3.717616010995022,
                                      -3.7241350893443865,
                                      -3.7368233335203174,
                                      -3.724935256816335,
                                      -3.731221207808135,
                                      -3.729673568842895,
                                      -3.734599272157031,
                                      -3.733479116332905,
                                      -3.724588889344407,
                                      -3.725812274003649,
                                      -3.6136328893450336,
                                      -3.793095660509606,
                                      -3.789484518180044,
                                      -3.738717662355411,
                                      -3.690741497976085,
                                      -3.681525587497768,
                                      -4.1505770451633435))
coordFac.lst[['ucm']] <- coordFac_ucm.dt  

# uc3m
coordFac_uc3m.dt <- data.table(des_centro = allFac.lst[['uc3m']],
                               lat = c(40.05277466566666,
                                       40.547319552867606,
                                       40.332578079825915,
                                       40.54420394906492,
                                       40.314221949300574,
                                       40.31691736487609),
                               lon = c(-3.5998059316876345,
                                       -4.011213502334747,
                                       -3.765080535655515,
                                       -4.012511616329703,
                                       -3.7248345893489048,
                                       -3.7263569740080507))
coordFac.lst[['uc3m']] <- coordFac_uc3m.dt

saveRDS(coordFac.lst, file.path(path_rds, 'coordFac.lst.RDS'))

## Selected degrees arrays                                                      ----
seriesTit.lst <- vector(mode = 'list', length = length(uni.vec))
names(seriesTit.lst) <- uni.vec

# uva
seriesTit_uva.vec <- c('Grado En Comercio',           
                    'Grado En Educacion Primaria',
                    'Grado En Economia',
                    'Grado En Educacion Infantil', 
                    'Grado En Enologia',           
                    'Grado En Ingenieria Mecanica')
seriesTit.lst[['uva']] <- str_replace_all(
  str_to_title(seriesTit_uva.vec),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))

# urjc
seriesTit_urjc.vec <- c('Grado En Filosofia',           
                     'Grado En Turismo',
                     'Grado En Criminologia',
                     'Grado En Derecho', 
                     'Grado En Fundamentos De La Arquitectura',           
                     'Grado En Marketing')
seriesTit.lst[['urjc']] <- str_replace_all(
  str_to_title(seriesTit_urjc.vec),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))

# uam
seriesTit_uam.vec <- c('Grado En  Derecho / Grado En Administracion Y Direccion De Empresas',
                    'Grado En  Ingenieria Informatica / Grado En Matematicas',
                    'Graduado O Graduada En Biologia Por La Universidad Autonoma De Madrid',
                    'Graduado O Graduada En Ciencias Y Lenguas De La Antigüedad Por La Universidad Autonoma De Madrid',
                    'Graduado O Graduada En Matematicas Por La Universidad Autonoma De Madrid',
                    'Graduado O Graduada En Economia Por La Universidad Autonoma De Madrid')
seriesTit.lst[['uam']] <- str_replace_all(
  str_to_title(seriesTit_uam.vec),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))

# ucm
seriesTit_ucm.vec <- c('Grado En Sociologia',           
                    'Grado En Arqueologia',
                    'Grado En Geologia',
                    'Grado En Matematicas', 
                    'Grado En Medicina',           
                    'Grado En Turismo')
seriesTit.lst[['ucm']] <- str_replace_all(
  str_to_title(seriesTit_ucm.vec),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))

# uc3m
seriesTit_uc3m.vec <- c('Doble Grado Ciencia E Ingenieria De Datos - Ingenieria En Tecnologias De Telecomunicacion',           
                    'Doble Grado En Ciencias Politicas Y Sociologia',
                    'Doble Grado En Derecho Y Administracion De Empresas',
                    'Grado En Ingenieria Electrica', 
                    'Grado En Periodismo',           
                    'Grado En Sociologia')
seriesTit.lst[['uc3m']] <- str_replace_all(
  str_to_title(seriesTit_uc3m.vec),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))

saveRDS(seriesTit.lst, file.path(path_rds, 'seriesTit.lst.RDS'))

## Selected faculties array                                                     ----
seriesFac.lst <- vector(mode = 'list', length = length(uni.vec))
names(seriesFac.lst) <- uni.vec

# uva
seriesFac_uva.vec <- c('Escuela de Ingeniería de la Industria Forestal, Agronómica y de la Bioenergía',           
                       'Escuela de Ingeniería Informática de Segovia',
                       'Escuela de Ingeniería Informática de Valladolid',
                       'Facultad de Ciencias', 
                       'Escuela Técnica Superior de Arquitectura',           
                       'Escuela de Ingenierías Industriales')
seriesFac.lst[['uva']] <- str_replace_all(
  str_to_title(seriesFac_uva.vec),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))

# urjc
seriesFac_urjc.vec <- c('Centro de Educación Superior CEDEU',           
                        'Facultad de Ciencias Jurídicas y Sociales',
                        'Facultad de Ciencias de la Salud',
                        'Escuela Superior de Ciencias Experimentales y Tecnología', 
                        'Facultad de Ciencias Jurídicas y Sociales. Campus Fuenlabrada',           
                        'Facultad de Ciencias Jurídicas y Sociales. Campus Móstoles')
seriesFac.lst[['urjc']] <- str_replace_all(
  str_to_title(seriesFac_urjc.vec),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))

# uam
seriesFac_uam.vec <- c('Centro Superior de Estudios Universitarios La Salle',           
                       'Escuela Politécnica Superior',
                       'Escuela Universitaria de Enfermería Cruz Roja',
                       'Facultad de Psicología', 
                       'Facultad de Derecho',           
                       'Facultad de Ciencias')
seriesFac.lst[['uam']] <- str_replace_all(
  str_to_title(seriesFac_uam.vec),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))

# ucm
seriesFac_ucm.vec <- c('Centro de Enseñanza Superior en Humanidades y Ciencias de la Educación \"Don Bosco\"',           
                       'Colegio Universitario Cardenal Cisneros',
                       'Facultad de Ciencias de la Documentación',
                       'Facultad de Ciencias Geológicas', 
                       'Escuela Universitaria de Magisterio - Escuni',           
                       'Facultad de Ciencias Químicas')
seriesFac.lst[['ucm']] <- str_replace_all(
  str_to_title(seriesFac_ucm.vec),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))

# uc3m
seriesFac_uc3m.vec <- c('Centro Universitario Guardia Civil',           
                        'Escuela Politécnica Superior (Colmenarejo)',
                        'Escuela Politécnica Superior. (Leganés)',
                        'Facultad de Ciencias Sociales y Jurídicas. (Getafe)', 
                        'Facultad de Ciencias Sociales y Jurídicas (Colmenarejo)',           
                        'Facultad de Humanidades, Comunicación y Documentación. (Getafe)')
seriesFac.lst[['uc3m']] <- str_replace_all(
  str_to_title(seriesFac_uc3m.vec),
  c('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u', '  ' = ' '))

saveRDS(seriesFac.lst, file.path(path_rds, 'seriesFac.lst.RDS'))

## New students                                                                 ----
nStuYear.lst <- vector(mode = 'list', length = length(uni.vec))
names(nStuYear.lst) <- uni.vec

# uva
nStuYear_uva.dt <- uva_data.dt[, .(nStu = .N), by = year]
nStuYear.lst[['uva']] <- nStuYear_uva.dt

# urjc
nStuYear_urjc.dt <- urjc_data.dt[, .(nStu = .N), by = year]
nStuYear.lst[['urjc']] <- nStuYear_urjc.dt

# uam
nStuYear_uam.dt <- uam_data.dt[, .(nStu = .N), by = year]
nStuYear.lst[['uam']] <- nStuYear_uam.dt

# ucm
nStuYear_ucm.dt <- ucm_data.dt[, .(nStu = .N), by = year]
nStuYear.lst[['ucm']] <- nStuYear_ucm.dt

# uc3m
nStuYear_uc3m.dt <- uc3m_data.dt[, .(nStu = .N), by = year]
nStuYear.lst[['uc3m']] <- nStuYear_uc3m.dt

saveRDS(nStuYear.lst, file.path(path_rds, 'nStuYear.lst.RDS'))

## Degree with more students                                                    ----
nStuCareYear.lst <- vector(mode = 'list', length = length(uni.vec))
names(nStuCareYear.lst) <- uni.vec

# uva
nStuCareYear_uva.dt <- na.omit(uva_data.dt[, .(nStu = .N), by = c('year', 'des_titulacion')])
nStuCareYear_uva_levels.lst <- vapply(names(nStuCareYear_uva.dt), function(var){
  return(unique(nStuCareYear_uva.dt[, ..var]))
}, list(1))
names(nStuCareYear_uva_levels.lst) <- names(nStuCareYear_uva.dt)
nStuCareYear_uva_levels.lst[[3]] <- NULL

nStuCareYear_copy.dt <- copy(nStuCareYear_uva.dt)
for(tit in nStuCareYear_uva_levels.lst[['des_titulacion']]){
  missing_years <- setdiff(nStuCareYear_uva_levels.lst[['year']], nStuCareYear_uva.dt[des_titulacion == tit, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_titulacion = tit,
                          nStu = 0)
    nStuCareYear_copy.dt <- rbind(nStuCareYear_copy.dt, temp.dt)
  }
}
nStuCareYear_uva.dt <- copy(nStuCareYear_copy.dt)
nStuCareYear.lst[['uva']] <- nStuCareYear_uva.dt

# urjc
nStuCareYear_urjc.dt <- na.omit(urjc_data.dt[, .(nStu = .N), by = c('year', 'des_titulacion')])
nStuCareYear_urjc_levels.lst <- vapply(names(nStuCareYear_urjc.dt), function(var){
  return(unique(nStuCareYear_urjc.dt[, ..var]))
}, list(1))
names(nStuCareYear_urjc_levels.lst) <- names(nStuCareYear_urjc.dt)
nStuCareYear_urjc_levels.lst[[3]] <- NULL

nStuCareYear_copy.dt <- copy(nStuCareYear_urjc.dt)
for(tit in nStuCareYear_urjc_levels.lst[['des_titulacion']]){
  missing_years <- setdiff(nStuCareYear_urjc_levels.lst[['year']], nStuCareYear_urjc.dt[des_titulacion == tit, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_titulacion = tit,
                          nStu = 0)
    nStuCareYear_copy.dt <- rbind(nStuCareYear_copy.dt, temp.dt)
  }
}
nStuCareYear_urjc.dt <- copy(nStuCareYear_copy.dt)
nStuCareYear.lst[['urjc']] <- nStuCareYear_urjc.dt

# uam
nStuCareYear_uam.dt <- na.omit(uam_data.dt[, .(nStu = .N), by = c('year', 'des_titulacion')])
nStuCareYear_uam_levels.lst <- vapply(names(nStuCareYear_uam.dt), function(var){
  return(unique(nStuCareYear_uam.dt[, ..var]))
}, list(1))
names(nStuCareYear_uam_levels.lst) <- names(nStuCareYear_uam.dt)
nStuCareYear_uam_levels.lst[[3]] <- NULL

nStuCareYear_copy.dt <- copy(nStuCareYear_uam.dt)
for(tit in nStuCareYear_uam_levels.lst[['des_titulacion']]){
  missing_years <- setdiff(nStuCareYear_uam_levels.lst[['year']], nStuCareYear_uam.dt[des_titulacion == tit, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_titulacion = tit,
                          nStu = 0)
    nStuCareYear_copy.dt <- rbind(nStuCareYear_copy.dt, temp.dt)
  }
}
nStuCareYear_uam.dt <- copy(nStuCareYear_copy.dt)
nStuCareYear.lst[['uam']] <- nStuCareYear_uam.dt

# ucm
nStuCareYear_ucm.dt <- na.omit(ucm_data.dt[, .(nStu = .N), by = c('year', 'des_titulacion')])
nStuCareYear_ucm_levels.lst <- vapply(names(nStuCareYear_ucm.dt), function(var){
  return(unique(nStuCareYear_ucm.dt[, ..var]))
}, list(1))
names(nStuCareYear_ucm_levels.lst) <- names(nStuCareYear_ucm.dt)
nStuCareYear_ucm_levels.lst[[3]] <- NULL

nStuCareYear_copy.dt <- copy(nStuCareYear_ucm.dt)
for(tit in nStuCareYear_ucm_levels.lst[['des_titulacion']]){
  missing_years <- setdiff(nStuCareYear_ucm_levels.lst[['year']], nStuCareYear_ucm.dt[des_titulacion == tit, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_titulacion = tit,
                          nStu = 0)
    nStuCareYear_copy.dt <- rbind(nStuCareYear_copy.dt, temp.dt)
  }
}
nStuCareYear_ucm.dt <- copy(nStuCareYear_copy.dt)
nStuCareYear.lst[['ucm']] <- nStuCareYear_ucm.dt

# uc3m
nStuCareYear_uc3m.dt <- na.omit(uc3m_data.dt[, .(nStu = .N), by = c('year', 'des_titulacion')])
nStuCareYear_uc3m_levels.lst <- vapply(names(nStuCareYear_uc3m.dt), function(var){
  return(unique(nStuCareYear_uc3m.dt[, ..var]))
}, list(1))
names(nStuCareYear_uc3m_levels.lst) <- names(nStuCareYear_uc3m.dt)
nStuCareYear_uc3m_levels.lst[[3]] <- NULL

nStuCareYear_copy.dt <- copy(nStuCareYear_uc3m.dt)
for(tit in nStuCareYear_uc3m_levels.lst[['des_titulacion']]){
  missing_years <- setdiff(nStuCareYear_uc3m_levels.lst[['year']], nStuCareYear_uc3m.dt[des_titulacion == tit, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_titulacion = tit,
                          nStu = 0)
    nStuCareYear_copy.dt <- rbind(nStuCareYear_copy.dt, temp.dt)
  }
}
nStuCareYear_uc3m.dt <- copy(nStuCareYear_copy.dt)
nStuCareYear.lst[['uc3m']] <- nStuCareYear_uc3m.dt

saveRDS(nStuCareYear.lst, file.path(path_rds, 'nStuTitYear.lst.RDS'))

## Faculty with more students                                                   ----
nStuFacYear.lst <- vector(mode = 'list', length = length(uni.vec))
names(nStuFacYear.lst) <- uni.vec

# uva
nStuFacYear_uva.dt <- na.omit(uva_data.dt[, .(nStu = .N), by = c('year', 'des_centro')])
nStuFacYear_uva_levels.lst <- vapply(names(nStuFacYear_uva.dt), function(var){
  return(unique(nStuFacYear_uva.dt[, ..var]))
}, list(1))
names(nStuFacYear_uva_levels.lst) <- names(nStuFacYear_uva.dt)
nStuFacYear_uva_levels.lst[[3]] <- NULL

nStuFacYear_copy.dt <- copy(nStuFacYear_uva.dt)
for(fac in nStuFacYear_uva_levels.lst[['des_centro']]){
  missing_years <- setdiff(nStuFacYear_uva_levels.lst[['year']], nStuFacYear_uva.dt[des_centro == fac, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_centro = fac,
                          nStu = 0)
    nStuFacYear_copy.dt <- rbind(nStuFacYear_copy.dt, temp.dt)
  }
}
nStuFacYear_uva.dt <- copy(nStuFacYear_copy.dt)
nStuFacYear.lst[['uva']] <- nStuFacYear_uva.dt
  
# urjc
nStuFacYear_urjc.dt <- na.omit(urjc_data.dt[, .(nStu = .N), by = c('year', 'des_centro')])
nStuFacYear_urjc_levels.lst <- vapply(names(nStuFacYear_urjc.dt), function(var){
  return(unique(nStuFacYear_urjc.dt[, ..var]))
}, list(1))
names(nStuFacYear_urjc_levels.lst) <- names(nStuFacYear_urjc.dt)
nStuFacYear_urjc_levels.lst[[3]] <- NULL

nStuFacYear_copy.dt <- copy(nStuFacYear_urjc.dt)
for(fac in nStuFacYear_urjc_levels.lst[['des_centro']]){
  missing_years <- setdiff(nStuFacYear_urjc_levels.lst[['year']], nStuFacYear_urjc.dt[des_centro == fac, year])
  
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_centro = fac,
                          nStu = 0)
    nStuFacYear_copy.dt <- rbind(nStuFacYear_copy.dt, temp.dt)
  }
}
nStuFacYear_urjc.dt <- copy(nStuFacYear_copy.dt)
nStuFacYear.lst[['urjc']] <- nStuFacYear_urjc.dt

# uam
nStuFacYear_uam.dt <- na.omit(uam_data.dt[, .(nStu = .N), by = c('year', 'des_centro')])
nStuFacYear_uam_levels.lst <- vapply(names(nStuFacYear_uam.dt), function(var){
  return(unique(nStuFacYear_uam.dt[, ..var]))
}, list(1))
names(nStuFacYear_uam_levels.lst) <- names(nStuFacYear_uam.dt)
nStuFacYear_uam_levels.lst[[3]] <- NULL

nStuFacYear_copy.dt <- copy(nStuFacYear_uam.dt)
for(fac in nStuFacYear_uam_levels.lst[['des_centro']]){
  missing_years <- setdiff(nStuFacYear_uam_levels.lst[['year']], nStuFacYear_uam.dt[des_centro == fac, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_centro = fac,
                          nStu = 0)
    nStuFacYear_copy.dt <- rbind(nStuFacYear_copy.dt, temp.dt)
  }
}
nStuFacYear_uam.dt <- copy(nStuFacYear_copy.dt)
nStuFacYear.lst[['uam']] <- nStuFacYear_uam.dt

# ucm
nStuFacYear_ucm.dt <- na.omit(ucm_data.dt[, .(nStu = .N), by = c('year', 'des_centro')])
nStuFacYear_ucm_levels.lst <- vapply(names(nStuFacYear_ucm.dt), function(var){
  return(unique(nStuFacYear_ucm.dt[, ..var]))
}, list(1))
names(nStuFacYear_ucm_levels.lst) <- names(nStuFacYear_ucm.dt)
nStuFacYear_ucm_levels.lst[[3]] <- NULL

nStuFacYear_copy.dt <- copy(nStuFacYear_ucm.dt)
for(fac in nStuFacYear_ucm_levels.lst[['des_centro']]){
  missing_years <- setdiff(nStuFacYear_ucm_levels.lst[['year']], nStuFacYear_ucm.dt[des_centro == fac, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_centro = fac,
                          nStu = 0)
    nStuFacYear_copy.dt <- rbind(nStuFacYear_copy.dt, temp.dt)
  }
}
nStuFacYear_ucm.dt <- copy(nStuFacYear_copy.dt)
nStuFacYear.lst[['ucm']] <- nStuFacYear_ucm.dt

# uc3m
nStuFacYear_uc3m.dt <- na.omit(uc3m_data.dt[, .(nStu = .N), by = c('year', 'des_centro')])
nStuFacYear_uc3m_levels.lst <- vapply(names(nStuFacYear_uc3m.dt), function(var){
  return(unique(nStuFacYear_uc3m.dt[, ..var]))
}, list(1))
names(nStuFacYear_uc3m_levels.lst) <- names(nStuFacYear_uc3m.dt)
nStuFacYear_uc3m_levels.lst[[3]] <- NULL

nStuFacYear_copy.dt <- copy(nStuFacYear_uc3m.dt)
for(fac in nStuFacYear_uc3m_levels.lst[['des_centro']]){
  missing_years <- setdiff(nStuFacYear_uc3m_levels.lst[['year']], nStuFacYear_uc3m.dt[des_centro == fac, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_centro = fac,
                          nStu = 0)
    nStuFacYear_copy.dt <- rbind(nStuFacYear_copy.dt, temp.dt)
  }
}
nStuFacYear_uc3m.dt <- copy(nStuFacYear_copy.dt)
nStuFacYear.lst[['uc3m']] <- nStuFacYear_uc3m.dt

saveRDS(nStuFacYear.lst, file.path(path_rds, 'nStuFacYear.lst.RDS'))




# Page 2                                                                        ----

## Continents                                                                   ----
nCont.lst <- vector(mode = 'list', length = length(uni.vec))
names(nCont.lst) <- uni.vec

# uva
nCont_uva.dt <- uva_data.dt[, .(nStu = .N), by = c('des_continente_nacionalidad', 'year')]
nCont.lst[['uva']] <- nCont_uva.dt

# urjc
nCont_urjc.dt <- urjc_data.dt[, .(nStu = .N), by = c('des_continente_nacionalidad', 'year')]
nCont.lst[['urjc']] <- nCont_urjc.dt

# uam
nCont_uam.dt <- uam_data.dt[, .(nStu = .N), by = c('des_continente_nacionalidad', 'year')]
nCont.lst[['uam']] <- nCont_uam.dt

# ucm
nCont_ucm.dt <- ucm_data.dt[, .(nStu = .N), by = c('des_continente_nacionalidad', 'year')]
nCont.lst[['ucm']] <- nCont_ucm.dt

# uc3m
nCont_uc3m.dt <- uc3m_data.dt[, .(nStu = .N), by = c('des_continente_nacionalidad', 'year')]
nCont.lst[['uc3m']] <- nCont_uc3m.dt

saveRDS(nCont.lst, file = file.path(path_rds, 'nCont.lst.RDS'))

## Regions                                                                      ----
nContAgr.lst <- vector(mode = 'list', length = length(uni.vec))
names(nContAgr.lst) <- uni.vec

# uva
nContAgr_uva.dt <- uva_data.dt[, .(nStu = .N), by = c('des_agregacion_paises_nacionalidad', 'year')]
nContAgr.lst[['uva']] <- nContAgr_uva.dt

# urjc
nContAgr_urjc.dt <- urjc_data.dt[, .(nStu = .N), by = c('des_agregacion_paises_nacionalidad', 'year')]
nContAgr.lst[['urjc']] <- nContAgr_urjc.dt

# uam
nContAgr_uam.dt <- uam_data.dt[, .(nStu = .N), by = c('des_agregacion_paises_nacionalidad', 'year')]
nContAgr.lst[['uam']] <- nContAgr_uam.dt

# ucm
nContAgr_ucm.dt <- ucm_data.dt[, .(nStu = .N), by = c('des_agregacion_paises_nacionalidad', 'year')]
nContAgr.lst[['ucm']] <- nContAgr_ucm.dt

# uc3m
nContAgr_uc3m.dt <- uc3m_data.dt[, .(nStu = .N), by = c('des_agregacion_paises_nacionalidad', 'year')]
nContAgr.lst[['uc3m']] <- nContAgr_uc3m.dt

saveRDS(nContAgr.lst, file = file.path(path_rds, 'nContAgr.lst.RDS'))

## Countries                                                                    ----
nContPais.lst <- vector(mode = 'list', length = length(uni.vec))
names(nContPais.lst) <- uni.vec

# uva
nContPaisr_uva.dt <- uva_data.dt[, .(nStu = .N), by = c('des_pais_nacionalidad', 'year')]
nContPais.lst[['uva']] <- nContPaisr_uva.dt

# urjc
nContPais_urjc.dt <- urjc_data.dt[, .(nStu = .N), by = c('des_pais_nacionalidad', 'year')]
nContPais.lst[['urjc']] <- nContPais_urjc.dt

# uam
nContPais_uam.dt <- uam_data.dt[, .(nStu = .N), by = c('des_pais_nacionalidad', 'year')]
nContPais.lst[['uam']] <- nContPais_uam.dt

# ucm
nContPais_ucm.dt <- ucm_data.dt[, .(nStu = .N), by = c('des_pais_nacionalidad', 'year')]
nContPais.lst[['ucm']] <- nContPais_ucm.dt

# uc3m
nContPais_uc3m.dt <- uc3m_data.dt[, .(nStu = .N), by = c('des_pais_nacionalidad', 'year')]
nContPais.lst[['uc3m']] <- nContPais_uc3m.dt

saveRDS(nContPais.lst, file = file.path(path_rds, 'nContPais.lst.RDS'))

## Countries-Coordinates                                                        ----
coordPais.dt <- data.table(des_pais_nacionalidad = rbindlist(nContPais.lst)[, sort(unique(des_pais_nacionalidad))],
                           lat = c(33.96178262881533, 
                                   41.056661626982276,
                                   51.034610023316695,
                                   42.545233221403656, 
                                   -12.110385103957606,
                                   0,
                                   23.593679226292537,
                                   28.032211356155617,
                                   -34.62789976108856, 
                                   40.31279798686559,
                                   
                                   -23.8984649897633,
                                   47.5857617992739,
                                   40.33868314733914,
                                   24.125727229551277,
                                   53.161367632521376,
                                   50.87502957360749,
                                   16.76847774191263,
                                   -16.723875313267126,
                                   44.70432837479697,
                                   -22.280745037880425,
                                   
                                   -7.569057320625432,
                                   42.54179835425314,
                                   12.313291618161193,
                                   16.852657947015047,
                                   5.70996781840987,
                                   60.28376558898653,
                                   49.73242976477707,
                                   -26.72282175229627,
                                   23.86406854741636,
                                   35.02880154370415,
                                   
                                   3.3164361817001153, 
                                   -0.47081789255373757,
                                   10.36771553195094,
                                   7.160446913357453,
                                   45.08014388638591,
                                   21.516368051990465,
                                   55.59407374578977,
                                   15.435464194888281,
                                   -1.4430643706180812,
                                   26.677938794547018,
                                   
                                   13.77312394860064,
                                   48.6579593380223,
                                   46.060048899340224,
                                   40.59704216115838,
                                   31.788382467491076,
                                   39.1950600208933,
                                   58.70377014191063,
                                   62.16320126523596,
                                   12.673270192201407,
                                   62.020509432080964,
                                   
                                   46.5047137499958, 
                                   -0.459471546654049,
                                   13.50048080714687,
                                   42.16062100238217,
                                   7.923958344387426,
                                   39.54981625500051,
                                   15.732534460747463,
                                   10.487666249495652,
                                   12.085614066228446,
                                   1.560493863720317,
                                   
                                   18.704344941246745, 
                                   14.926744391302536, 
                                   46.92210014149002, 
                                   22.54665168983773, 
                                   -4.687532755408263, 
                                   32.0941699914144, 
                                   33.49985114109208, 
                                   53.16765088385265, 
                                   64.94598768455522, 
                                   31.067289234233012,
                                   
                                   43.09393582978359,
                                   36.37737044613728, 
                                   31.279164135783976,
                                   48.31147460204564, 
                                   0.4397586045688238, 
                                   41.48207329741381, 
                                   57.021894430803606,
                                   34.082582349858086,
                                   27.25500334688251, 
                                   55.565138142381315,
                                   
                                   49.62749995408661, 
                                   41.78180215166278, 
                                   3.826260276586441,
                                   17.829921886138376,
                                   31.83391780298535,
                                   20.36851229745517,
                                   23.73635489142635,
                                   42.90445804511217,
                                   -17.291624400867523,
                                   28.33065331229882,
                                   
                                   12.945025450566746,
                                   9.743592697056785, 
                                   61.98396670871205,
                                   -42.618341361038205, 
                                   52.13107239425709,
                                   30.011082885006406,
                                   8.556101488485597, 
                                   -23.382834787051646, 
                                   -10.34287725931054,
                                   52.887686873717534, 
                                   
                                   39.77516354589733,
                                   55.057259301022995,
                                   34.98356117889787, 
                                   36.42793253226101, 
                                   47.598816930795685,
                                   -2.5354249565973834,
                                   18.910993512055615, 
                                   40.23024463381887,
                                   -6.418809486897089,
                                   45.85389440219981, 
                                   
                                   24.7295075870535,
                                   14.553375983354211,
                                   44.109981289183956,
                                   7.6340131852284685,
                                   -30.96952383230039,
                                   64.45827512615455, 
                                   46.81756956427495, 
                                   15.60798370121396,
                                   23.974890747779494,
                                   8.755952582754219,
                                   
                                   33.9827486675652, 
                                   39.04833960831784, 
                                   49.046870108097835, 
                                   -32.79469802806999,
                                   41.85265199797337, 
                                   7.094916429140363,
                                   14.546293560698098,
                                   15.86127404221356
                                   ),
                           lon = c(65.208090747952,
                                   20.075881094755836,
                                   10.360540979872425,
                                   1.5688434285579593,
                                   17.514542972914935,
                                   0,
                                   45.046228751953386,
                                   2.700535340452862,
                                   -65.04795054752823,
                                   44.540177975639224,
                                   
                                   135.01488887749412,
                                   14.255479351963531,
                                   48.14737033383574,
                                   90.4166133678707,
                                   27.797170115711946,
                                   4.3117631279081,
                                   -88.70903112949698,
                                   -63.93934012734365,
                                   17.87936121897532,
                                   23.24359757118894,
                                   
                                   -54.96363871812551,
                                   25.134282784564004,
                                   -1.7671428266243139,
                                   -23.64348739645892,
                                   12.605145380065636,
                                   -112.12485604925158,
                                   15.033469770551338,
                                   -70.17131602371082,
                                   117.6296885286199,
                                   33.287126048887956,
                                   
                                   -72.89481501120912,
                                   15.264338143264764,
                                   -84.17547409361795,
                                   -5.873602981027668,
                                   14.942918662689893,
                                   -78.94193686458104,
                                   9.297710753121239,
                                   -61.33695986453028,
                                   -78.94912829385814,
                                   29.93928913986362,
                                   
                                   -89.28273281379711,
                                   19.36785848914023,
                                   14.658004416829398,
                                   -3.705352263053225,
                                   34.74467121893012,
                                   -102.41932912782356,
                                   25.51307184980274,
                                   93.33203202579818,
                                   122.92788301526888,
                                   24.99339373256647,
                                   
                                   2.514216639880329,
                                   12.268285669458265,
                                   -15.463198727069216,
                                   43.42105429964871,
                                   -1.048811690110416,
                                   22.14189389680027,
                                   -90.37050975924548,
                                   -10.760808204784498,
                                   -15.048441153433947,
                                   10.301199318950152,
                                   
                                   -72.28090774983214,
                                   -86.80644066439874,
                                   19.645565353824182,
                                   78.5538084010215,
                                   121.20455537447924,
                                   54.909109767841564,
                                   42.57573227966311,
                                   -8.222517002623002,
                                   -18.083541181014333,
                                   34.70507707936915,
                                   
                                   12.239140539215185,
                                   139.3814694137938,
                                   36.625031985645165,
                                   68.68723524851276,
                                   37.604052661889135,
                                   74.2903937680223,
                                   25.76202741722185,
                                   35.87960944204352,
                                   17.70171891762618,
                                   23.837729651054033,
                                   
                                   6.111203074305662,
                                   21.420407292950735,
                                   101.6139365687107,
                                   -2.0035995616736244,
                                   -6.570585114324263,
                                   -10.237373676270026,
                                   -102.91325241951363,
                                   19.391406224210694,
                                   35.15591841521269,
                                   84.09382723749779,
                                   
                                   -84.7360031555732,
                                   7.73630495005876,
                                   8.255170764180347,
                                   171.75421097517906,
                                   5.640108720616974,
                                   68.71726427693606,
                                   -80.27419015764788,
                                   -58.487381621255814,
                                   -75.41743587417368,
                                   18.40391741198693,
                                   
                                   -8.45562623236745,
                                   -3.022768685404803,
                                   38.59300398561261,
                                   128.01534747821728,
                                   28.31090722297048,
                                   24.053399937993692,
                                   -70.2463053712557,
                                   127.26019456064373,
                                   35.2572158840236,
                                   24.726589401232324,
                                   
                                   -13.495384599729292,
                                   -14.608682677551988,
                                   20.590433488369108,
                                   80.42755412791392,
                                   24.679935694021545,
                                   17.729609309317514,
                                   7.920749240382617,
                                   101.88544604562527,
                                   121.03572380125851,
                                   0.9382684617553629,
                                   
                                   9.855932509552044,
                                   34.39160457087874,
                                   31.01814604647812,
                                   -56.161082601041684,
                                   63.53120112572993,
                                   -66.41344532692963,
                                   108.549971336943,
                                   47.742486337705635
                                    ))

saveRDS(coordPais.dt, file = file.path(path_rds, 'coordPais.dt.RDS'))

# Page 3                                                                        ----
## Grades                                                                       ----
notas.lst <- vector(mode = 'list', length = length(uni.vec))
names(notas.lst) <- uni.vec

pal <- c('5-6', '6-7', '7-8', '8-9', '9-10', '10-11', '11-12', '12-13', '13-14')
ran <- c(4.99, 6, 7, 8, 9, 10, 11, 12, 13, 14.01)

# uva
notas_uva.dt <- rbind(uva_data.dt[is.na(nota_admision)], uva_data.dt[nota_admision >= 5])[!is.na(nota_admision)]

notas_copy.dt <- copy(notas_uva.dt[, c('nota_admision', 'year', 'des_titulacion')])
for(tit in nStuCareYear_uva_levels.lst[['des_titulacion']]){
  missing_years <- setdiff(nStuCareYear_uva_levels.lst[['year']], notas_uva.dt[des_titulacion == tit, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_titulacion = tit,
                          nota_admision = 5)
    notas_copy.dt <- rbind(notas_copy.dt, temp.dt)
  }
}

notas_uva.dt <- copy(notas_copy.dt)
notas_uva.dt[, nota_admision_cat := cut(notas_copy.dt[, nota_admision], breaks = ran, labels = pal)]

notas.lst[['uva']] <- notas_uva.dt

# urjc
notas_urjc.dt <- rbind(urjc_data.dt[is.na(nota_admision)], urjc_data.dt[nota_admision >= 5])[!is.na(nota_admision)]

notas_copy.dt <- copy(notas_urjc.dt[, c('nota_admision', 'year', 'des_titulacion')])
for(tit in nStuCareYear_urjc_levels.lst[['des_titulacion']]){
  missing_years <- setdiff(nStuCareYear_urjc_levels.lst[['year']], notas_urjc.dt[des_titulacion == tit, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_titulacion = tit,
                          nota_admision = 5)
    notas_copy.dt <- rbind(notas_copy.dt, temp.dt)
  }
}

notas_urjc.dt <- copy(notas_copy.dt)
notas_urjc.dt[, nota_admision_cat := cut(notas_copy.dt[, nota_admision], breaks = ran, labels = pal)]

notas.lst[['urjc']] <- notas_urjc.dt

# uam
notas_uam.dt <- rbind(uam_data.dt[is.na(nota_admision)], uam_data.dt[nota_admision >= 5])[!is.na(nota_admision)]

notas_copy.dt <- copy(notas_uam.dt[, c('nota_admision', 'year', 'des_titulacion')])
for(tit in nStuCareYear_uam_levels.lst[['des_titulacion']]){
  missing_years <- setdiff(nStuCareYear_uam_levels.lst[['year']], notas_uam.dt[des_titulacion == tit, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_titulacion = tit,
                          nota_admision = 5)
    notas_copy.dt <- rbind(notas_copy.dt, temp.dt)
  }
}

notas_uam.dt <- copy(notas_copy.dt)
notas_uam.dt[, nota_admision_cat := cut(notas_copy.dt[, nota_admision], breaks = ran, labels = pal)]

notas.lst[['uam']] <- notas_uam.dt

# ucm
notas_ucm.dt <- rbind(ucm_data.dt[is.na(nota_admision)], ucm_data.dt[nota_admision >= 5])[!is.na(nota_admision)]

notas_copy.dt <- copy(notas_ucm.dt[, c('nota_admision', 'year', 'des_titulacion')])
for(tit in nStuCareYear_ucm_levels.lst[['des_titulacion']]){
  missing_years <- setdiff(nStuCareYear_ucm_levels.lst[['year']], notas_ucm.dt[des_titulacion == tit, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_titulacion = tit,
                          nota_admision = 5)
    notas_copy.dt <- rbind(notas_copy.dt, temp.dt)
  }
}

notas_ucm.dt <- copy(notas_copy.dt)
notas_ucm.dt[, nota_admision_cat := cut(notas_copy.dt[, nota_admision], breaks = ran, labels = pal)]

notas.lst[['ucm']] <- notas_ucm.dt

# uc3m
notas_uc3m.dt <- rbind(uc3m_data.dt[is.na(nota_admision)], uc3m_data.dt[nota_admision >= 5])[!is.na(nota_admision)]

notas_copy.dt <- copy(notas_uc3m.dt[, c('nota_admision', 'year', 'des_titulacion')])
for(tit in nStuCareYear_uc3m_levels.lst[['des_titulacion']]){
  missing_years <- setdiff(nStuCareYear_uc3m_levels.lst[['year']], notas_uc3m.dt[des_titulacion == tit, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_titulacion = tit,
                          nota_admision = 5)
    notas_copy.dt <- rbind(notas_copy.dt, temp.dt)
  }
}

notas_uc3m.dt <- copy(notas_copy.dt)
notas_uc3m.dt[, nota_admision_cat := cut(notas_copy.dt[, nota_admision], breaks = ran, labels = pal)]

notas.lst[['uc3m']] <- notas_uc3m.dt

saveRDS(notas.lst, file = file.path(path_rds, 'notas.lst.RDS'))

# Page 4                                                                        ----

## Access study                                                                 ----
nEstAc.lst <- vector(mode = 'list', length = length(uni.vec))
names(nEstAc.lst) <- uni.vec

# uva
nEstAc_uva.dt <- na.omit(uva_data.dt[, .(nStu = .N), by = c('des_estudio_acceso', 'year')])
nEstAc.lst[['uva']] <- nEstAc_uva.dt

# urjc
nEstAc_urjc.dt <- na.omit(urjc_data.dt[, .(nStu = .N), by = c('des_estudio_acceso', 'year')])
nEstAc.lst[['urjc']] <- nEstAc_urjc.dt

# uam
nEstAc_uam.dt <- na.omit(uam_data.dt[, .(nStu = .N), by = c('des_estudio_acceso', 'year')])
nEstAc.lst[['uam']] <- nEstAc_uam.dt

# ucm
nEstAc_ucm.dt <- na.omit(ucm_data.dt[, .(nStu = .N), by = c('des_estudio_acceso', 'year')])
nEstAc.lst[['ucm']] <- nEstAc_ucm.dt

# uc3m
nEstAc_uc3m.dt <- na.omit(uc3m_data.dt[, .(nStu = .N), by = c('des_estudio_acceso', 'year')])
nEstAc.lst[['uc3m']] <- nEstAc_uc3m.dt

saveRDS(nEstAc.lst, file = file.path(path_rds, 'nEstAc.lst.RDS'))

## Municipalities                                                               ----
nMunCenSec.lst <- vector(mode = 'list', length = length(uni.vec))
names(nMunCenSec.lst) <- uni.vec

# uva
nMunCenSec_uva.dt <- na.omit(uva_data.dt[, .(nStu = .N), c('des_municipio_centro_sec', 'year')])
nMunCenSec_uva_levels.lst <- vapply(names(nMunCenSec_uva.dt), function(var){
  return(unique(nMunCenSec_uva.dt[, ..var]))
}, list(1))
names(nMunCenSec_uva_levels.lst) <- names(nMunCenSec_uva.dt)
nMunCenSec_uva_levels.lst[[3]] <- NULL

nMunCenSec_copy.dt <- copy(nMunCenSec_uva.dt)
for(mun in nMunCenSec_uva_levels.lst[['des_municipio_centro_sec']]){
  missing_years <- setdiff(nMunCenSec_uva_levels.lst[['year']], nMunCenSec_uva.dt[des_municipio_centro_sec == mun, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_municipio_centro_sec = mun,
                          nStu = 0)
    nMunCenSec_copy.dt <- rbind(nMunCenSec_copy.dt, temp.dt)
  }
}
nMunCenSec_uva.dt <- copy(nMunCenSec_copy.dt)

setnames(nMunCenSec_uva.dt, 'des_municipio_centro_sec', 'municipality')
nMunCenSec.lst[['uva']] <- nMunCenSec_uva.dt

# urjc
nMunCenSec_urjc.dt <- na.omit(urjc_data.dt[, .(nStu = .N), c('des_municipio_centro_sec', 'year')])
nMunCenSec_urjc_levels.lst <- vapply(names(nMunCenSec_urjc.dt), function(var){
  return(unique(nMunCenSec_urjc.dt[, ..var]))
}, list(1))
names(nMunCenSec_urjc_levels.lst) <- names(nMunCenSec_urjc.dt)
nMunCenSec_urjc_levels.lst[[3]] <- NULL

nMunCenSec_copy.dt <- copy(nMunCenSec_urjc.dt)
for(mun in nMunCenSec_urjc_levels.lst[['des_municipio_centro_sec']]){
  missing_years <- setdiff(nMunCenSec_urjc_levels.lst[['year']], nMunCenSec_urjc.dt[des_municipio_centro_sec == mun, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_municipio_centro_sec = mun,
                          nStu = 0)
    nMunCenSec_copy.dt <- rbind(nMunCenSec_copy.dt, temp.dt)
  }
}
nMunCenSec_urjc.dt <- copy(nMunCenSec_copy.dt)

setnames(nMunCenSec_urjc.dt, 'des_municipio_centro_sec', 'municipality')
nMunCenSec.lst[['urjc']] <- nMunCenSec_urjc.dt

# uam
nMunCenSec_uam.dt <- na.omit(uam_data.dt[, .(nStu = .N), c('des_municipio_centro_sec', 'year')])
nMunCenSec_uam_levels.lst <- vapply(names(nMunCenSec_uam.dt), function(var){
  return(unique(nMunCenSec_uam.dt[, ..var]))
}, list(1))
names(nMunCenSec_uam_levels.lst) <- names(nMunCenSec_uam.dt)
nMunCenSec_uam_levels.lst[[3]] <- NULL

nMunCenSec_copy.dt <- copy(nMunCenSec_uam.dt)
for(mun in nMunCenSec_uam_levels.lst[['des_municipio_centro_sec']]){
  missing_years <- setdiff(nMunCenSec_uam_levels.lst[['year']], nMunCenSec_uam.dt[des_municipio_centro_sec == mun, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_municipio_centro_sec = mun,
                          nStu = 0)
    nMunCenSec_copy.dt <- rbind(nMunCenSec_copy.dt, temp.dt)
  }
}
nMunCenSec_uam.dt <- copy(nMunCenSec_copy.dt)

setnames(nMunCenSec_uam.dt, 'des_municipio_centro_sec', 'municipality')
nMunCenSec.lst[['uam']] <- nMunCenSec_uam.dt

# ucm
nMunCenSec_ucm.dt <- na.omit(ucm_data.dt[, .(nStu = .N), c('des_municipio_centro_sec', 'year')])
nMunCenSec_ucm_levels.lst <- vapply(names(nMunCenSec_ucm.dt), function(var){
  return(unique(nMunCenSec_ucm.dt[, ..var]))
}, list(1))
names(nMunCenSec_ucm_levels.lst) <- names(nMunCenSec_ucm.dt)
nMunCenSec_ucm_levels.lst[[3]] <- NULL

nMunCenSec_copy.dt <- copy(nMunCenSec_ucm.dt)
for(mun in nMunCenSec_ucm_levels.lst[['des_municipio_centro_sec']]){
  missing_years <- setdiff(nMunCenSec_ucm_levels.lst[['year']], nMunCenSec_ucm.dt[des_municipio_centro_sec == mun, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_municipio_centro_sec = mun,
                          nStu = 0)
    nMunCenSec_copy.dt <- rbind(nMunCenSec_copy.dt, temp.dt)
  }
}
nMunCenSec_ucm.dt <- copy(nMunCenSec_copy.dt)

setnames(nMunCenSec_ucm.dt, 'des_municipio_centro_sec', 'municipality')
nMunCenSec.lst[['ucm']] <- nMunCenSec_ucm.dt

# uc3m
nMunCenSec_uc3m.dt <- na.omit(uc3m_data.dt[, .(nStu = .N), c('des_municipio_centro_sec', 'year')])
nMunCenSec_uc3m.dt <- rbind(nMunCenSec_uc3m.dt, data.table(des_municipio_centro_sec  = 'Valdemoro',
                                                           year = 2019,
                                                           nStu = 0))
nMunCenSec_uc3m_levels.lst <- vapply(names(nMunCenSec_uc3m.dt), function(var){
  return(unique(nMunCenSec_uc3m.dt[, ..var]))
}, list(1))
names(nMunCenSec_uc3m_levels.lst) <- names(nMunCenSec_uc3m.dt)
nMunCenSec_uc3m_levels.lst[[3]] <- NULL

nMunCenSec_copy.dt <- copy(nMunCenSec_uc3m.dt)
for(mun in nMunCenSec_uc3m_levels.lst[['des_municipio_centro_sec']]){
  missing_years <- setdiff(nMunCenSec_uc3m_levels.lst[['year']], nMunCenSec_uc3m.dt[des_municipio_centro_sec == mun, year])
  if(length(missing_years) != 0){
    temp.dt <- data.table(expand.grid(year = missing_years),
                          des_municipio_centro_sec = mun,
                          nStu = 0)
    nMunCenSec_copy.dt <- rbind(nMunCenSec_copy.dt, temp.dt)
  }
}
nMunCenSec_uc3m.dt <- copy(nMunCenSec_copy.dt)

setnames(nMunCenSec_uc3m.dt, 'des_municipio_centro_sec', 'municipality')
nMunCenSec.lst[['uc3m']] <- nMunCenSec_uc3m.dt

saveRDS(nMunCenSec.lst, file = file.path(path_rds, 'nMunCenSec.lst.RDS'))

## Provinces                                                                    ----
nProvCenSec.lst <- vector(mode = 'list', length = length(uni.vec))
names(nProvCenSec.lst) <- uni.vec

# uva
nProvCenSec_uva.dt <- na.omit(uva_data.dt[, .(nStu = .N), c('des_provincia_centro_sec', 'year')])
setnames(nProvCenSec_uva.dt, 'des_provincia_centro_sec', 'province')
nProvCenSec.lst[['uva']] <- nProvCenSec_uva.dt

# urjc
nProvCenSec_urjc.dt <- na.omit(urjc_data.dt[, .(nStu = .N), c('des_provincia_centro_sec', 'year')])
setnames(nProvCenSec_urjc.dt, 'des_provincia_centro_sec', 'province')
nProvCenSec.lst[['urjc']] <- nProvCenSec_urjc.dt

# uam
nProvCenSec_uam.dt <- na.omit(uam_data.dt[, .(nStu = .N), c('des_provincia_centro_sec', 'year')])
setnames(nProvCenSec_uam.dt, 'des_provincia_centro_sec', 'province')
nProvCenSec.lst[['uam']] <- nProvCenSec_uam.dt

# ucm
nProvCenSec_ucm.dt <- na.omit(ucm_data.dt[, .(nStu = .N), c('des_provincia_centro_sec', 'year')])
setnames(nProvCenSec_ucm.dt, 'des_provincia_centro_sec', 'province')
nProvCenSec.lst[['ucm']] <- nProvCenSec_ucm.dt

# uc3m
nProvCenSec_uc3m.dt <- na.omit(uc3m_data.dt[, .(nStu = .N), c('des_provincia_centro_sec', 'year')])
setnames(nProvCenSec_uc3m.dt, 'des_provincia_centro_sec', 'province')
nProvCenSec.lst[['uc3m']] <- nProvCenSec_uc3m.dt

saveRDS(nProvCenSec.lst, file = file.path(path_rds, 'nProvCenSec.lst.RDS'))

## Autonomous communities                                                       ----
nComCenSec.lst <- vector(mode = 'list', length = length(uni.vec))
names(nComCenSec.lst) <- uni.vec

# uva
nComCenSec_uva.dt <- na.omit(uva_data.dt[, .(nStu = .N), c('des_comunidad_centro_sec', 'year')])
setnames(nComCenSec_uva.dt, 'des_comunidad_centro_sec', 'community')
nComCenSec.lst[['uva']] <- nComCenSec_uva.dt

# urjc
nComCenSec_urjc.dt <- na.omit(urjc_data.dt[, .(nStu = .N), c('des_comunidad_centro_sec', 'year')])
setnames(nComCenSec_urjc.dt, 'des_comunidad_centro_sec', 'community')
nComCenSec.lst[['urjc']] <- nComCenSec_urjc.dt

# uam
nComCenSec_uam.dt <- na.omit(uam_data.dt[, .(nStu = .N), c('des_comunidad_centro_sec', 'year')])
setnames(nComCenSec_uam.dt, 'des_comunidad_centro_sec', 'community')
nComCenSec.lst[['uam']] <- nComCenSec_uam.dt

# ucm
nComCenSec_ucm.dt <- na.omit(ucm_data.dt[, .(nStu = .N), c('des_comunidad_centro_sec', 'year')])
setnames(nComCenSec_ucm.dt, 'des_comunidad_centro_sec', 'community')
nComCenSec.lst[['ucm']] <- nComCenSec_ucm.dt

# uc3m
nComCenSec_uc3m.dt <- na.omit(uc3m_data.dt[, .(nStu = .N), c('des_comunidad_centro_sec', 'year')])
setnames(nComCenSec_uc3m.dt, 'des_comunidad_centro_sec', 'community')
nComCenSec.lst[['uc3m']] <- nComCenSec_uc3m.dt

saveRDS(nComCenSec.lst, file = file.path(path_rds, 'nComCenSec.lst.RDS'))

## Municipalities array                                                         ----
allMun.lst <- vector(mode = 'list', length = length(uni.vec))
names(allMun.lst) <- uni.vec

# uva
allMun.lst[['uva']] <- nMunCenSec_uva.dt[, sort(unique(municipality))]

# urjc
allMun.lst[['urjc']] <- nMunCenSec_urjc.dt[, sort(unique(municipality))]

# uam
allMun.lst[['uam']] <- nMunCenSec_uam.dt[, sort(unique(municipality))]

# ucm
allMun.lst[['ucm']] <- nMunCenSec_ucm.dt[, sort(unique(municipality))]

# uc3m
allMun.lst[['uc3m']] <- nMunCenSec_uc3m.dt[, sort(unique(municipality))]

saveRDS(allMun.lst, file = file.path(path_rds, 'allMun.lst.RDS'))

## Geographic files                                                             ----
idPro.dt <- data.table(read_csv(file.path(path_data, 'enrichment', 'id_province.csv')))
idCom.dt <- data.table(read_csv(file.path(path_data, 'enrichment', 'id_community.csv')))
spain_provinces.sp <- geojson_read(file.path(path_data, 'enrichment', "spain-provinces.geojson"), what = "sp")
spain_communities.sp <- geojson_read(file.path(path_data, 'enrichment', "spain-communities.geojson"), what = "sp")

saveRDS(idPro.dt, file.path(path_rds, 'idPro.dt.RDS'))
saveRDS(idCom.dt, file.path(path_rds, 'idCom.dt.RDS'))
saveRDS(spain_provinces.sp, file.path(path_rds, 'spain_provinces.sp.RDS'))
saveRDS(spain_communities.sp, file.path(path_rds, 'spain_communities.sp.RDS'))

