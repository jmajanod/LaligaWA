# Este script es llamado desde ui.R, y los paquetes son cargados ya en esa aplicacion.
#########
#if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
# if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
#library(readxl)
#########

path <- "data/"
list.files(path)
ficheros <- list.files(path) 
ficheros <- ficheros[-1]

# cargamos resultados de nuevo si no exite
if (!"resultados" %in% ls()) { 
      resultados <- data.frame()
      for (i in ficheros) {
        temporada <- gsub("(\\D+)", "", i)
        print(temporada)
        full_path <- file.path(path, i)
        resultados_temp <- read_csv(full_path) %>% select(HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% 
          mutate(Temporada=temporada, FTHG = as.integer(FTHG), FTAG=as.integer(FTAG))
        resultados <- bind_rows(resultados, resultados_temp)
      }
  }

head(resultados)

#########    AHORA LEEMOS EL RANKING HISTORICO Y ESTABLECEMOS PESOS PARA CADA EQUIPO
# Nombres:
#   - historico:   Clasificacion historica Liga
#   - nombres:     Nombres de todos los equipos de la liga historicamente
#   - temporadas:  temporadas con datos
#   - ultima_jornada:  Ultima jornada en la actual temporada. Sirve para nivelar el peso de la clasificacion de la temp. actual
#   - peso_temporad: El peso que se va a dar a la clasificacion de cada temporada
#   - peso_clasificacion:  Peso de la clasificacion el la puntuacion final de cada equipo
#   - Pesos : El peso de cada equipo. A mas peso, mejor equipo
#             utilizamos la funcion dar_pesos para calcular el peso de cada equipo
#
#   Variables importantes:
#     - nombres:    nombres de cada equipo
#     - Pesos:      Fuerza de cada equipo

if (!"historico" %in% ls()) { 
          historico <- read_xlsx("data/ClasificacionHistorica.xlsx")
          nombres <- (unique(historico$Equipo)) %>% str_sort() %>% as.factor()
          historico$Equipo <- as.factor(historico$Equipo)
          levels(historico$Equipo) = levels(nombres)
          temporadas <- unique(historico$Temporada)
          ultima_jornada <- historico$Jugados[1] 
          peso_temporada <- seq(length(temporadas),1,by=-1)  # las temporadas recientes tienen mayor peso que las pasadas.
          peso_temporada[1] <- peso_temporada[1]*ultima_jornada*2/38   # el peso de la actual temporada es mayor a mayor numero de partidos
          peso_temporada <- data.frame(temporada = temporadas, peso = peso_temporada)
          peso_clasificacion <- unique(historico$Posicion) %>% sort(decreasing = TRUE)
          
          dar_pesos <- function() {
            Pesos <- data.frame(equipos=nombres, peso=rep(0,length(nombres)))
            for (t in temporadas) {
              datos <- historico %>% filter(Temporada==t) 
              for (n in nombres) {
                if (n %in% datos$Equipo) {posicion <- datos %>% filter(Equipo==n) %>% pull(Posicion)
                puntos <- peso_temporada$peso[peso_temporada$temporada == t] * peso_clasificacion[posicion]
                }
                else {puntos <- 0}
                Pesos$peso[Pesos$equipos== n] <- Pesos$peso[Pesos$equipos== n] + puntos  
              }
            }
            Pesos
          }
          options(digits = 0)
          Pesos <- dar_pesos()
          Pesos <- Pesos %>% arrange(desc(peso))
}          

