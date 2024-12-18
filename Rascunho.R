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

