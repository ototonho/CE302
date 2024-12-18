# - Rascunho
library(tidyverse)
library(data.table)

#Etapas
# Pull
# Commit
# Push

# qualquer coisa

Notas <- data.frame(Instrumento = c("Violino", "Viola", "Violoncelo", "Contrabaixo", "Flauta", "Clarinete", "Oboé", "Trompete", "Trombone", "Percussão"),
                    Notas = c(56, 59, 76, 38, 50, 37, 62, 93, 62, 52),
                    Limite_Volume = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))


# a) A raiz quadrada da média das notas dos instrumentos é:
  

sqrt_avrg = sqrt(mean(Notas$Notas))
# 7.6485
# CERTA!

# b) A mediana das notas dos instrumentos que atingiram o limite de volume é:


Med_Lim <- Notas %>%
  filter(Limite_Volume == TRUE) %>%
  summarize(mediana = median(Notas))
Med_Lim

# 69
# CERTA!

# sugestão
# median(Notas$Notas[Notas$Limite_Volume == TRUE])


#c) A maior nota entre os instrumentos que não atingiram o limite de volume é:

Med_N_Lim <- Notas %>%
  filter(Limite_Volume == FALSE) %>%
  summarize(maxima = sort(Notas)) %>%
  arrange(desc(maxima)) %>%
  slice(1)
Med_N_Lim

# 59
# CERTA!

# sugestão
# max(Notas$Notas[Notas$Limite_Volume == FALSE])

#d) Considerando que instrumentos com nota entre 40 e 69 precisam de ajustes, o número de instrumentos que precisam de ajustes e atingiram o limite de volume é: 

Inst_ajust <- Notas %>%
  filter(Notas > 40 & Notas < 69)
Inst_ajust
count(Inst_ajust)

# ERRADA, não está considerando limite=true!!
#sugestão 1

# Inst_ajust <- Notas %>%
#  filter(Notas > 40 & Notas < 69 & Limite_Volume == TRUE)
# Inst_ajust
# count(Inst_ajust)


#sugestão 2
# sum(Notas$Notas >= 40 & Notas$Notas <= 69 & Notas$Limite_Volume == TRUE)


#e) O desvio-padrão das notas dos instrumentos que atingiram o limite de volume é:

DesV_Lim <- Notas %>%
  filter(Limite_Volume == TRUE) %>%
  summarize(desvio = sd(Notas))
DesV_Lim

# 14.73
# CERTA!

#################################
## questão 2


Michelin_DF <- read.csv("C:/Breno/CE302/michelin.csv")

  # a função que ela deu ta toda cagada, essa é a certa
# d = 2*R*asin((sin^2*((φ2-φ1)/2)+cosφ1*cosφ2*sin^2*((λ2-λ1)/2))^(1/2))

haversine_distance <- function(φ1,λ1,φ2,λ2) {
  R <- 6371 # Raio da Terra em km
  φ1 <- φ1 * pi / 180
  λ1 <- λ1 * pi / 180
  φ2 <- φ2 * pi / 180
  λ2 <- λ2 * pi / 180
  
  # diferença entre lat e lon
  dlat <- φ2 - φ1
  dlon <- λ2 - λ1
  
  # distancia
  a <- sin(dlat / 2)^2 + cos(φ1) * cos(φ2) * sin(dlon / 2)^2
  d <- 2 * R * asin(sqrt(a))
  return(d)
  
}


# coords do per se

per_se_coords <- Michelin_DF %>%
  filter(Name == "Per Se") %>%
  select(Latitude, Longitude) %>%
  slice(1) %>%
  as.numeric()

# restaurantes 1 star

one_star_rest <- Michelin_DF %>%
  filter(str_detect(Award, "1 Star"))

# add coluna com distancia calculada
one_star_rest <- one_star_rest %>%
mutate(D_do_PerSe = haversine_distance(
  per_se_coords[1], per_se_coords[2], Latitude, Longitude
))

# rest mais proximo
rest_prox <- one_star_rest %>%
  slice_min(order_by = D_do_PerSe, n = 1)

# mostrar resultado

rest_prox %>%
  select(Name, D_do_PerSe)

## mari, 0.98 km
## olhei no maps e a distancia ta certa pelo menos kk
