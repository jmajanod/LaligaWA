# - ht: equipo que juega en casa
# - at: equipo visitante
# 
# 
# 
# 
# 
#  Introducimos los equipos que compiten, local y visitante (ht, at)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
ht <- "Ath Madrid"
at <- "Barcelona"

#  mostramos la relacion de fuerzas historica
cat("===================", "\n")
cat("Relacion de fuerzas", "\n")
cat("===================", "\n")
cat("Equipo       Ranking", "\n")
cat(ht, " ", Pesos$peso[Pesos$equipos == ht], "\n")
cat ( at, " ", Pesos$peso[Pesos$equipos == at], "\n")
options(digits = 4)
fuerza <- Pesos$peso[Pesos$equipos == ht]/Pesos$peso[Pesos$equipos == at]
cat("Relacion de fuerzas: ", fuerza, "\n")
options(digits = 0)


cat("================", "\n")
cat("ultimos partidos", "\n")
cat("================", "\n")
ultimos_resultados <- resultados %>% filter(HomeTeam == ht & AwayTeam == at) %>% arrange(desc(as.integer(Temporada)))
print(ultimos_resultados)

#  mostramos los ultimos resultados de este partido:
cat("==================", "\n")
cat("ultimos resultados", "\n")
cat("==================", "\n")
casa <- sum (ultimos_resultados$FTR == "H")
empate <- sum (ultimos_resultados$FTR == "D")
fuera <- sum (ultimos_resultados$FTR == "A")
cat("Partidos ganados por equipo local", casa, "\n")
cat("Partidos empatados:              ", empate, "\n")
cat("Partidos ganados por equipo fuera", fuera, "\n")


#  mostramos los ultimos resultados como local y visitantes de ambos equipos
cat("===========================", "\n")
cat("Resultados en casa por temporada", ht, "\n")
cat("===========================", "\n")
res_casa <- resultados %>% filter(HomeTeam == ht) %>% select(HomeTeam, FTR, Temporada) %>% mutate(Temporada = as.integer(Temporada)) %>%  
                       arrange(desc(Temporada)) %>%                     
                       group_by(HomeTeam, Temporada) %>%
                       summarize(g = sum(FTR=="H"),
                                 e = sum(FTR == "D"),
                                 p = sum(FTR == "A")) 
print(res_casa)  
  
cat("===========================", "\n")
cat("Resultados fuera por temporada", at, "\n")
cat("===========================", "\n")
res_fuera <- resultados %>% filter(AwayTeam == at) %>% select(AwayTeam, FTR, Temporada) %>% mutate(Temporada = as.integer(Temporada)) %>%
  group_by(AwayTeam, Temporada) %>% 
  arrange(desc(Temporada)) %>%
  summarize(g = sum(FTR=="A"),
            e = sum(FTR == "D"),
            p = sum(FTR == "H")) 
print(res_fuera)  


# Ahora que probabilides hay
options(digits = 4)
cat("===========================", "\n")
cat("Resumen de resultados comunes", "\n")
cat("===========================", "\n")
cat(ht, " tiene una puntuacion ", fuerza, " veces mayor que ", at, "\n" )
cat("En los partidos en casa, ", ht, " gana el", casa/(casa+empate+fuera), " de las veces a", at, "\n" )
cat("                                 ", " empata el", empate/(casa+empate+fuera), " de las veces a", at, "\n" )
cat("                                 ", " pierde el", fuera/(casa+empate+fuera), " de las veces a", at, "\n" )

cat("===========================", "\n")
cat("Resumen de resultados en casa de ",  ht, "\n")
cat("===========================", "\n")
g_casa <- sum(res_casa$g)/(sum(res_casa$g)+sum(res_casa$e)+sum(res_casa$p))
e_casa <- sum(res_casa$e)/(sum(res_casa$g)+sum(res_casa$e)+sum(res_casa$p))
p_casa <- sum(res_casa$p)/(sum(res_casa$g)+sum(res_casa$e)+sum(res_casa$p))
cat("En los partidos en casa, el ", ht, " viene ganando el", g_casa, " de las veces.",  "\n" )
cat("                            ", " viene empatando el", e_casa, " de las veces.",  "\n" )
cat("                            ", " viene perdiendo el", p_casa, " de las veces.",  "\n" )

cat("===========================", "\n")
cat("Resumen de resultados fuera de ",  at, "\n")
cat("===========================", "\n")
g_fuera <- sum(res_fuera$g)/(sum(res_fuera$g)+sum(res_fuera$e)+sum(res_fuera$p))
e_fuera <- sum(res_fuera$e)/(sum(res_fuera$g)+sum(res_fuera$e)+sum(res_fuera$p))
p_fuera <- sum(res_fuera$p)/(sum(res_fuera$g)+sum(res_fuera$e)+sum(res_fuera$p))
cat("En los partidos fuera de casa, el ", at, " viene ganando el", g_fuera, " de las veces.",  "\n" )
cat("                                      ", " viene empatando el", e_fuera, " de las veces.",  "\n" )
cat("                                      ", " viene perdiendo el", p_fuera, " de las veces.",  "\n" )

options(digits = 0)

