## Prova Final
library(tidyverse)
library(data.table)
#Considere as notas dos instrumentos de uma orquestra descritas no vetor numérico nomeado em Notas e os instrumentos que atingiram o limite de volume descritos no vetor lógico nomeado em LimiteVolume:
Notas <- data.frame(Instrumento = c("Violino", "Viola", "Violoncelo", "Contrabaixo", "Flauta", "Clarinete", "Oboé", "Trompete", "Trombone", "Percussão"),
                    Notas = c(56, 59, 76, 38, 50, 37, 62, 93, 62, 52),
                    Limite_Volume = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
#a) A raiz quadrada da média das notas dos instrumentos é:
sqrt(mean(Notas$Notas))

#b) A mediana das notas dos instrumentos que atingiram o limite de volume é:
Med_Lim <- Notas %>%
  filter(Limite_Volume == TRUE) %>%
  summarize(mediana = median(Notas))
Med_Lim

#c) A maior nota entre os instrumentos que não atingiram o limite de volume é:
Med_N_Lim <- Notas %>%
  filter(Limite_Volume == FALSE) %>%
  summarize(maxima = sort(Notas)) %>%
  arrange(desc(maxima)) %>%
  slice(1)
Med_N_Lim

#d) Considerando que instrumentos com nota entre 40 e 69 precisam de ajustes, o número de instrumentos que precisam de ajustes e atingiram o limite de volume é: 
Inst_ajust <- Notas %>%
  filter(Notas > 40 & Notas < 69)
Inst_ajust
count(Inst_ajust)

#e) O desvio-padrão das notas dos instrumentos que atingiram o limite de volume é:
DesV_Lim <- Notas %>%
  filter(Limite_Volume == TRUE) %>%
  summarize(desvio = sd(Notas))
DesV_Lim


##Questão 2
