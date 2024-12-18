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

##Um entusiasta gastronômico está em uma jornada para explorar restaurantes listados no guia Michelin. O viajante começou sua aventura no Restaurante Per Se em New York, USA. Após saborear as delícias do restaurante, ele segue para a cidade mais próxima que abriga um restaurante reconhecido no guia Michelin.

#Para calcular a distância entre os restaurantes, utilizaremos a equação de Haversine, uma fórmula eficaz para medir distâncias entre pontos de latitude e longitude na superfície esférica da Terra.

#A equação de Haversine para calcular a distância (d) entre dois pontos na superfície de uma esfera (como a Terra) a partir das coordenadas de latitude, em radianos, (φ1, φ1) e longitude (λ1, λ2) é dada por

Michelin_DF <- read.csv("/home/est/apsn24/CE302/michelin.csv")

#a) 
haversine_distance 


## Questão 3

## Abrindo os dados
BoardGames_Cat <- read.csv("/home/est/apsn24/CE302/BoardGames_Cat.csv")
BoardGames_Num <- read.csv("/home/est/apsn24/CE302/BoardGames_Numeric.csv")

BoardGames_Junto <- BoardGames_Cat %>%
  full_join(BoardGames_Num, by = c("id" = "id"))

##a) 
espaços <- BoardGames_Junto %>%
  filter(str_count(sp_name, "\\w+") == 2) %>%
  summarize(conta = sum(average > 7.5))
print(espaços)
sum(espaços)

espaços <- BoardGames_Junto %>%
  filter(name = 2) %>%
  summarize(conta = sum(average > 7.5))
print(espaços)

 
#b) 
exclamação <- sum(BoardGames_Junto$description %like% "!" & BoardGames_Junto$yearpublished > 2010)
print(exclamação)

#c)
M12_M70 <- BoardGames_Junto %>%
  filter(maxplayers == 12 & minplaytime > 70)

sem_K <- M12_M70 %>%
  filter(name %like% "k")

#d) 
Av_1996 <- BoardGames_Junto %>%
  filter(yearpublished == 1996 & maxplaytime == 120) %>%
  group_by(name) %>%
  summarize(conta = sum(users_rated, na.rm = TRUE)) %>%
  arrange(desc(conta)) %>%
  slice(1)
print(Av_1996)

#e) 
MCarac <- BoardGames_Junto %>%
  filter(yearpublished == 1995 & minage %% 2) %>%
  group_by(name) %>%
  summarize(conta = sum(users_rated, na.rm = TRUE)) %>%
  arrange(desc(conta)) %>%
  slice(1)

#Questão 4
install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2024-12-24')
global_holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-24/global_holidays.csv')
monthly_passengers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-24/monthly_passengers.csv')

#a) 
class(global_holidays$Date)
data <- as.Date(global_holidays$Date)
ano <- format(data, "%Y")
mes <- format(data, "%m")
dia <- format(data, "%d")

global_holidays[, ":="(ano = format(data, "%Y"), mes = ormat(data, "%m"), dia = format(data, "%d"))] 

feriados14_HNG <- global_holidays %>%
  filter(ADM_name == "Hungary") %>%
  select(contains(2014)) %>%
  group_by("%m") %>%
  summarize(conta = sum(Name))

feriados14_HNG


#e) Qual país teve a maior quantidade de feriados públicos no ano de 2017?
feri_p_2017 <- global_holidays %>%
  filter(Type == "Public holiday") %>%
  select(contains(2017)) %>%
  summarize(conta = sum(ADM_name, na.rm = TRUE)) %>%
  arrange(desc(conta))
feri_p_2017
