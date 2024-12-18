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


Michelin_DF <- read.csv("/home/est/apsn24/CE302/michelin.csv")

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

###########################################

##
#Considere os bancos de dados BoardGames1 e BoardGames2 para responder as questões que seguem. Os bancos apresentam uma série de informações sobre jogos de tabuleiro.

# Importante: Ao incluir respostas do tipo string, certifique-se de que a mesma corresponde exatamente ao que está contido nos bancos de dados. Caso contrário, o sistema vai considerar que sua resposta está incorreta.

BoardGames_Cat <- read.csv("C:/Breno/CE302/BoardGames_Cat.csv")
BoardGames_Num <- read.csv("C:/Breno/CE302/BoardGames_Numeric.csv")

BoardGames_Junto <- BoardGames_Cat %>%
  full_join(BoardGames_Num, by = c("id" = "id"))
#a) Dentre os jogos cujo nome (name) contém exatamente 2 palavras, quantos possuem nota média (average) maior que 7.5?
## erradaaa
##
BoardGames_Junto <- BoardGames_Junto %>%
  mutate(word_count = str_count(name, "\\S+"))

espaços <- BoardGames_Junto %>%
  filter(word_count == 2) %>%     
  summarize(conta = sum(average > 7.5))
print(espaços)

#  b) Quantos jogos possuem o sinal de exclamação na descrição (description)? Considere apenas os jogos lançados após 2010 (yearpublished).

exclamação <- sum(BoardGames_Junto$description %like% "!" & BoardGames_Junto$yearpublished > 2010)
print(exclamação)

## CERTAAA

#```{r}
#exclamação <- sum(BoardGames_Junto$description %like% "!" & BoardGames_Junto$yearpublished > 2010)
#print(exclamação)
#```

#c) Considere os jogos que admitem no máximo 12 jogadores(as) (maxplayers) e que possuem tempo mínimo de jogo (minplaytime) superior a 70. Dentre estes, quantos não contém a letra K no nome (name)?
  
# erradaa

sem_k <- BoardGames_Junto %>%
  filter(maxplayers <= 12, minplaytime > 70, !str_detect(name, "[Kk]")) %>%
  nrow()

# d) Dentre os jogos lançados em 1996 (yearpublished), qual possui a maior quantidade de avaliações (users_rated)? Considere apenas os jogos cujo tempo máximo de jogo (maxplaytime) é 120 minutos.

# ```{r}
# Av_1996 <- BoardGames_Junto %>%
#   filter(yearpublished == 1996 & maxplaytime == 120) %>%
#   group_by(name) %>%
#   summarize(conta = sum(users_rated, na.rm = TRUE)) %>%
#   arrange(desc(conta)) %>%
#   slice(1)
# print(Av_1996)
# ```
# 
# 
# e) Dentre os jogos lançados em 1995 (yearpublished), qual possui a maior quantidade de caracteres na descrição (description)? Considere apenas os jogos cuja idade mínima para jogar (minage) é dada por um número par.
# 
# 
# ## Questão 4
# 
# Feriados são épocas importantes tanto para confraternizações, descansos e para viagens. Diversos países celebram diferentes feriados e com isso, muitas pessoas aproveitam para viajar.
# 
# Os dados contidos em tuesdata <- tidytuesdayR::tt_load('2024-12-24') contêm duas tabelas, uma com informações sobre feriados globais e outra sobre a quantidade de passageiros mensais em aeroportos.
# 
# Nota: Caso não consiga abrir o pacote, busque no Google alternativas para fazer a leitura da base de dados, por exemplo, direto de uma url.
# 
# Instruções: Não manipule o nome dos feriados, nem dos países. Considere apenas informações completas.
# 
# Acerca dos dados fornecidos, responda as seguintes questões:
#   
#   No ano de 2012 houveram quantos feriados no(a) país Hungary?
#   
#   a) Em qual mês do ano de 2014 houve mais feriados no(a) país Hungary?
#   
#   b) Quantos passageiros (Total) foram registrados no aeroporto do(a) país Uk no mês com maior número de feriados do ano de 2013?
#   
#   c) Qual ano teve a maior média de passageiros do tipo International nos aeroportos do(a) país Switzerland?
#   
#   d) Qual país teve a maior quantidade de feriados públicos no ano de 2017?
#   
#   e) Qual ano teve a maior quantidade de passageiros nos aeroportos do(a) país Lithuania?
