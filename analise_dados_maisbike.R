# Link com os datasets:
  ## http://www.dados.df.gov.br/dataset/viagens-feitas-com-o-sistema-de-bicicletas-compartilhadas-do-distrito-federal


library(tidyverse)
library(lubridate)
library(leaflet)

if(!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "lubridate", "leaflet")

# Importação e limpeza dos dados ------------------------------------------


df_viagens <- read_delim("dados_mais_bike/dados_maisbike_2018.csv", ";")

df_viagens <- df_viagens %>% 
  separate(data_nascimento, c("data_nascimento", "hora_nascimento"), 10) %>% 
  filter(hora_nascimento != "7") %>% 
  select(-hora_nascimento)

df_viagens$data_nascimento <- dmy(df_viagens$data_nascimento)
df_viagens$data_viagem <- dmy(df_viagens$data_viagem)
df_viagens$genero_usuario <- factor(df_viagens$genero_usuario, levels = c("M", "F"), labels = c("M", "F"))


# Manipulação dos dados em 'df_estacoes' ----------------------------------

# criação da variavel duracao_viagem
df_viagens <- df_viagens %>% 
  mutate(duracao_viagem = (horario_final - horario_inicial)) %>% 
  mutate(duracao_viagem = ifelse(duracao_viagem >= 0, duracao_viagem / 60,
                                 (86400 + duracao_viagem) / 60))

# duracao_viagem: convertendo duracao > 1000 para NA
df_viagens <- df_viagens %>% 
  mutate(duracao_viagem = ifelse(duracao_viagem >= 1000, NA, duracao_viagem))

# criação da variável atraso (se duracao_viagem > 60 minutos)
df_viagens <- df_viagens %>% 
  mutate(atraso = ifelse(duracao_viagem > 60, 1, 0))

# Criação do dataset 'df_estacoes' -------------------------------------

df_estacoes <- tibble(estacao = unique(df_viagens$estacao_inicial),
                      estacao2 = unique(df_viagens$estacao_inicial))

df_estacoes <- df_estacoes %>% 
  separate(estacao2, c("numero_estacao", "nome_estacao"), "-")

df_estacoes$numero_estacao <- as.integer(df_estacoes$numero_estacao)

n_partidas <- df_viagens %>% 
  group_by(estacao_inicial) %>% 
  summarise(n_partidas = n())

df_estacoes <- df_estacoes %>% 
  left_join(n_partidas, by = c("estacao" = "estacao_inicial"))

n_devolucoes <- df_viagens %>% 
  group_by(estacao_final) %>% 
  summarise(n_devolucoes = n())

df_estacoes <- df_estacoes %>% 
  left_join(n_devolucoes, by = c("estacao" = "estacao_final"))

rm(n_partidas, n_devolucoes)


df_estacoes$lat <- rep(0, 50)
df_estacoes$lon <- rep(0, 50)

df_estacoes <- df_estacoes %>% filter(estacao != "900 - Estação Teste")

df_estacoes <- df_estacoes %>% arrange(numero_estacao)
for(i in 1:nrow(df_estacoes)){
  cat(paste("df_estacoes[df_estacoes$estacao == \"",df_estacoes$estacao[i],"\",]$lat <- \n", sep = ""))
  cat(paste("df_estacoes[df_estacoes$estacao == \"",df_estacoes$estacao[i],"\",]$lon <- \n", sep = ""))
}

# Inserindo valores de lat/lon das estações --------------

df_estacoes[df_estacoes$estacao == "1 - Memorial JK",]$lat <- -15.783479
df_estacoes[df_estacoes$estacao == "1 - Memorial JK",]$lon <- -47.913372
df_estacoes[df_estacoes$estacao == "2 - Praça Buriti",]$lat <- -15.785651
df_estacoes[df_estacoes$estacao == "2 - Praça Buriti",]$lon <- -47.908750
df_estacoes[df_estacoes$estacao == "3 - Centro de Convenções",]$lat <- -15.786409
df_estacoes[df_estacoes$estacao == "3 - Centro de Convenções",]$lon <- -47.899865
df_estacoes[df_estacoes$estacao == "4 - Torre de TV",]$lat <- -15.789581
df_estacoes[df_estacoes$estacao == "4 - Torre de TV",]$lon <- -47.894312
df_estacoes[df_estacoes$estacao == "5 - Setor Hoteleiro Norte",]$lat <- -15.789029
df_estacoes[df_estacoes$estacao == "5 - Setor Hoteleiro Norte",]$lon <- -47.891015
df_estacoes[df_estacoes$estacao == "6 - Rodoviária",]$lat <- -15.793930
df_estacoes[df_estacoes$estacao == "6 - Rodoviária",]$lon <- -47.881529
df_estacoes[df_estacoes$estacao == "7 - Catedral Metropolitana",]$lat <- -15.797438
df_estacoes[df_estacoes$estacao == "7 - Catedral Metropolitana",]$lon <- -47.874552
df_estacoes[df_estacoes$estacao == "8 - Ministério da Defesa",]$lat <- -15.795867
df_estacoes[df_estacoes$estacao == "8 - Ministério da Defesa",]$lon <- -47.870901
df_estacoes[df_estacoes$estacao == "9 - Ministério da Cultura",]$lat <- -15.798295
df_estacoes[df_estacoes$estacao == "9 - Ministério da Cultura",]$lon <- -47.871807
df_estacoes[df_estacoes$estacao == "10 - Ministério dos Transportes",]$lat <- -15.796830
df_estacoes[df_estacoes$estacao == "10 - Ministério dos Transportes",]$lon <- -47.867795
df_estacoes[df_estacoes$estacao == "11 - Rodoviária 2",]$lat <- -15.794622
df_estacoes[df_estacoes$estacao == "11 - Rodoviária 2",]$lon <- -47.881721
df_estacoes[df_estacoes$estacao == "12 - Rodoviária 3",]$lat <- -15.794242
df_estacoes[df_estacoes$estacao == "12 - Rodoviária 3",]$lon <- -47.883524
df_estacoes[df_estacoes$estacao == "13 - Liberty Mall",]$lat <- -15.785720
df_estacoes[df_estacoes$estacao == "13 - Liberty Mall",]$lon <- -47.883563
df_estacoes[df_estacoes$estacao == "14 - Brasília Shopping",]$lat <- -15.786409
df_estacoes[df_estacoes$estacao == "14 - Brasília Shopping",]$lon <- -47.887898
df_estacoes[df_estacoes$estacao == "15 - Brasil 21",]$lat <- -15.794724
df_estacoes[df_estacoes$estacao == "15 - Brasil 21",]$lon <- -47.895230
df_estacoes[df_estacoes$estacao == "16 - SRTVS",]$lat <- -15.796389
df_estacoes[df_estacoes$estacao == "16 - SRTVS",]$lon <- -47.893437
df_estacoes[df_estacoes$estacao == "17 - Shopping Pátio Brasil",]$lat <- -15.795827
df_estacoes[df_estacoes$estacao == "17 - Shopping Pátio Brasil",]$lon <- -47.890914
df_estacoes[df_estacoes$estacao == "18 - Praça do Povo",]$lat <- -15.798575
df_estacoes[df_estacoes$estacao == "18 - Praça do Povo",]$lon <- -47.889059
df_estacoes[df_estacoes$estacao == "19 - Banco Central",]$lat <- -15.802146
df_estacoes[df_estacoes$estacao == "19 - Banco Central",]$lon <- -47.883785
df_estacoes[df_estacoes$estacao == "20 - SAS",]$lat <- -15.799919
df_estacoes[df_estacoes$estacao == "20 - SAS",]$lon <- -47.880428
df_estacoes[df_estacoes$estacao == "21 - EQS 202 / 203",]$lat <- -15.807616
df_estacoes[df_estacoes$estacao == "21 - EQS 202 / 203",]$lon <- -47.885283
df_estacoes[df_estacoes$estacao == "22 - 102 Sul",]$lat <- -15.805286
df_estacoes[df_estacoes$estacao == "22 - 102 Sul",]$lon <- -47.889203
df_estacoes[df_estacoes$estacao == "23 - STF",]$lat <- -15.801647
df_estacoes[df_estacoes$estacao == "23 - STF",]$lon <- -47.862147
df_estacoes[df_estacoes$estacao == "24 - Galeria",]$lat <- -15.798696
df_estacoes[df_estacoes$estacao == "24 - Galeria",]$lon <- -47.884806
df_estacoes[df_estacoes$estacao == "25 - Deck Sul",]$lat <- -15.837970
df_estacoes[df_estacoes$estacao == "25 - Deck Sul",]$lon <- -47.902858
df_estacoes[df_estacoes$estacao == "26 - Ministério da Saude",]$lat <- -15.799611
df_estacoes[df_estacoes$estacao == "26 - Ministério da Saude",]$lon <- -47.868395
df_estacoes[df_estacoes$estacao == "27 - Ministério do Planejamento",]$lat <- -15.794664
df_estacoes[df_estacoes$estacao == "27 - Ministério do Planejamento",]$lon <- -47.874487
df_estacoes[df_estacoes$estacao == "28 - CNMP - Conselho Nacional do Ministério Público",]$lat <- -15.804550
df_estacoes[df_estacoes$estacao == "28 - CNMP - Conselho Nacional do Ministério Público",]$lon <- -47.867038
df_estacoes[df_estacoes$estacao == "29 - INMETRO",]$lat <- -15.780858
df_estacoes[df_estacoes$estacao == "29 - INMETRO",]$lon <- -47.883277
df_estacoes[df_estacoes$estacao == "30 - TSE",]$lat <- -15.809428
df_estacoes[df_estacoes$estacao == "30 - TSE",]$lon <- -47.868797
df_estacoes[df_estacoes$estacao == "31 - EQS 204/205",]$lat <- -15.812401
df_estacoes[df_estacoes$estacao == "31 - EQS 204/205",]$lon <- -47.890381
df_estacoes[df_estacoes$estacao == "32 - SQS 305",]$lat <- -15.809892
df_estacoes[df_estacoes$estacao == "32 - SQS 305",]$lon <- -47.897377
df_estacoes[df_estacoes$estacao == "33 - EQS 104/304",]$lat <- -15.807126
df_estacoes[df_estacoes$estacao == "33 - EQS 104/304",]$lon <- -47.894236
df_estacoes[df_estacoes$estacao == "34 - Shopping Mall",]$lat <- -15.802090
df_estacoes[df_estacoes$estacao == "34 - Shopping Mall",]$lon <- -47.892749
df_estacoes[df_estacoes$estacao == "35 - CLN 403",]$lat <- -15.780134
df_estacoes[df_estacoes$estacao == "35 - CLN 403",]$lon <- -47.875000
df_estacoes[df_estacoes$estacao == "36 - CLN 204",]$lat <- -15.776928
df_estacoes[df_estacoes$estacao == "36 - CLN 204",]$lon <- -47.877007
df_estacoes[df_estacoes$estacao == "37 - EQN 104/105",]$lat <- -15.774583
df_estacoes[df_estacoes$estacao == "37 - EQN 104/105",]$lon <- -47.883707
df_estacoes[df_estacoes$estacao == "38 - SQN 205",]$lat <- -15.770067
df_estacoes[df_estacoes$estacao == "38 - SQN 205",]$lon <- -47.878153
df_estacoes[df_estacoes$estacao == "39 - CLN 406",]$lat <- -15.765826
df_estacoes[df_estacoes$estacao == "39 - CLN 406",]$lon <- -47.876935
df_estacoes[df_estacoes$estacao == "40 - W2/EQN305/306",]$lat <- -15.771618
df_estacoes[df_estacoes$estacao == "40 - W2/EQN305/306",]$lon <- -47.887756
df_estacoes[df_estacoes$estacao == "41 - Instituto de Artes",]$lat <- -15.763955
df_estacoes[df_estacoes$estacao == "41 - Instituto de Artes",]$lon <- -47.870486
df_estacoes[df_estacoes$estacao == "42 - PAT",]$lat <- -15.759220
df_estacoes[df_estacoes$estacao == "42 - PAT",]$lon <- -47.870397
df_estacoes[df_estacoes$estacao == "43 - Biblioteca Central",]$lat <- -15.761123
df_estacoes[df_estacoes$estacao == "43 - Biblioteca Central",]$lon <- -47.867443
df_estacoes[df_estacoes$estacao == "44 - ICC Sul",]$lat <- -15.765966
df_estacoes[df_estacoes$estacao == "44 - ICC Sul",]$lon <- -47.867058
df_estacoes[df_estacoes$estacao == "45 - Centro Olímpico",]$lat <- -15.765133
df_estacoes[df_estacoes$estacao == "45 - Centro Olímpico",]$lon <- -47.857989
df_estacoes[df_estacoes$estacao == "46 - EQN 408/409",]$lat <- -15.758767
df_estacoes[df_estacoes$estacao == "46 - EQN 408/409",]$lon <- -47.878664
df_estacoes[df_estacoes$estacao == "47 - EQN 410/411",]$lat <- -15.752944
df_estacoes[df_estacoes$estacao == "47 - EQN 410/411",]$lon <- -47.880552
df_estacoes[df_estacoes$estacao == "500 - Deck Sul Kids",]$lat <- -15.837818
df_estacoes[df_estacoes$estacao == "500 - Deck Sul Kids",]$lon <- -47.902195
df_estacoes[df_estacoes$estacao == "501 - Fórum Mundial da Água",]$lat <- -15.835700
df_estacoes[df_estacoes$estacao == "501 - Fórum Mundial da Água",]$lon <- -47.861808

# Criar um 'Shiny' que dê a distribuição de partidas/devolucoes por estacao --------

# Distribuição de partidas/devoluções por hora
df_viagens %>% 
  mutate(hora_partida = hour(horario_inicial)) %>% 
  filter(estacao_inicial == "41 - Instituto de Artes") %>% 
  ggplot()+
  geom_bar(aes(x = hora_partida))+
  scale_x_continuous(breaks = seq(0,23))

df_viagens %>% 
  mutate(hora_devolucao = hour(horario_final)) %>% 
  filter(estacao_inicial == "41 - Instituto de Artes") %>% 
  ggplot()+
  geom_bar(aes(x = hora_devolucao))+
  scale_x_continuous(breaks = seq(0,23))

# Distribuição do uso da estação ao longo do ano
df_viagens %>% 
  group_by(estacao_inicial, data_viagem) %>% 
  summarise(n_viagens = n()) %>% 
  filter(estacao_inicial == "41 - Instituto de Artes") %>% 
  ggplot()+
  geom_line(aes(x = data_viagem, y = n_viagens), stat = "identity")+
  scale_x_date(date_breaks = "month")

# para se obter dia da semana: 
wday("2018-09-18", label = TRUE, locale = "Portuguese_Brazil.1252")




# mapas --------

for(i in 1:nrow(df_estacoes)){
  cat(paste("addMarkers(lng=",df_estacoes$lon[i], ", lat=", df_estacoes$lat[i], ", popup=\"", df_estacoes$estacao[i], "\") %>% \n", sep = ""))
}

leaflet() %>%
  addTiles() %>%
  addMarkers(lng=-47.913372, lat=-15.783479, popup="1 - Memorial JK") %>% 
  addMarkers(lng=-47.90875, lat=-15.785651, popup="2 - Praça Buriti") %>% 
  addMarkers(lng=-47.899865, lat=-15.786409, popup="3 - Centro de Convenções") %>% 
  addMarkers(lng=-47.894312, lat=-15.789581, popup="4 - Torre de TV") %>% 
  addMarkers(lng=-47.891015, lat=-15.789029, popup="5 - Setor Hoteleiro Norte") %>% 
  addMarkers(lng=-47.881529, lat=-15.79393, popup="6 - Rodoviária") %>% 
  addMarkers(lng=-47.874552, lat=-15.797438, popup="7 - Catedral Metropolitana") %>% 
  addMarkers(lng=-47.870901, lat=-15.795867, popup="8 - Ministério da Defesa") %>% 
  addMarkers(lng=-47.871807, lat=-15.798295, popup="9 - Ministério da Cultura") %>% 
  addMarkers(lng=-47.867795, lat=-15.79683, popup="10 - Ministério dos Transportes") %>% 
  addMarkers(lng=-47.881721, lat=-15.794622, popup="11 - Rodoviária 2") %>% 
  addMarkers(lng=-47.883524, lat=-15.794242, popup="12 - Rodoviária 3") %>% 
  addMarkers(lng=-47.883563, lat=-15.78572, popup="13 - Liberty Mall") %>% 
  addMarkers(lng=-47.887898, lat=-15.786409, popup="14 - Brasília Shopping") %>% 
  addMarkers(lng=-47.89523, lat=-15.794724, popup="15 - Brasil 21") %>% 
  addMarkers(lng=-47.893437, lat=-15.796389, popup="16 - SRTVS") %>% 
  addMarkers(lng=-47.890914, lat=-15.795827, popup="17 - Shopping Pátio Brasil") %>% 
  addMarkers(lng=-47.889059, lat=-15.798575, popup="18 - Praça do Povo") %>% 
  addMarkers(lng=-47.883785, lat=-15.802146, popup="19 - Banco Central") %>% 
  addMarkers(lng=-47.880428, lat=-15.799919, popup="20 - SAS") %>% 
  addMarkers(lng=-47.885283, lat=-15.807616, popup="21 - EQS 202 / 203") %>% 
  addMarkers(lng=-47.889203, lat=-15.805286, popup="22 - 102 Sul") %>% 
  addMarkers(lng=-47.862147, lat=-15.801647, popup="23 - STF") %>% 
  addMarkers(lng=-47.884806, lat=-15.798696, popup="24 - Galeria") %>% 
  addMarkers(lng=-47.902858, lat=-15.83797, popup="25 - Deck Sul") %>% 
  addMarkers(lng=-47.868395, lat=-15.799611, popup="26 - Ministério da Saude") %>% 
  addMarkers(lng=-47.874487, lat=-15.794664, popup="27 - Ministério do Planejamento") %>% 
  addMarkers(lng=-47.867038, lat=-15.80455, popup="28 - CNMP - Conselho Nacional do Ministério Público") %>% 
  addMarkers(lng=-47.883277, lat=-15.780858, popup="29 - INMETRO") %>% 
  addMarkers(lng=-47.868797, lat=-15.809428, popup="30 - TSE") %>% 
  addMarkers(lng=-47.890381, lat=-15.812401, popup="31 - EQS 204/205") %>% 
  addMarkers(lng=-47.897377, lat=-15.809892, popup="32 - SQS 305") %>% 
  addMarkers(lng=-47.894236, lat=-15.807126, popup="33 - EQS 104/304") %>% 
  addMarkers(lng=-47.892749, lat=-15.80209, popup="34 - Shopping Mall") %>% 
  addMarkers(lng=-47.875, lat=-15.780134, popup="35 - CLN 403") %>% 
  addMarkers(lng=-47.877007, lat=-15.776928, popup="36 - CLN 204") %>% 
  addMarkers(lng=-47.883707, lat=-15.774583, popup="37 - EQN 104/105") %>% 
  addMarkers(lng=-47.878153, lat=-15.770067, popup="38 - SQN 205") %>% 
  addMarkers(lng=-47.876935, lat=-15.765826, popup="39 - CLN 406") %>% 
  addMarkers(lng=-47.887756, lat=-15.771618, popup="40 - W2/EQN305/306") %>% 
  addMarkers(lng=-47.870486, lat=-15.763955, popup="41 - Instituto de Artes") %>% 
  addMarkers(lng=-47.870397, lat=-15.75922, popup="42 - PAT") %>% 
  addMarkers(lng=-47.867443, lat=-15.761123, popup="43 - Biblioteca Central") %>% 
  addMarkers(lng=-47.867058, lat=-15.765966, popup="44 - ICC Sul") %>% 
  addMarkers(lng=-47.857989, lat=-15.765133, popup="45 - Centro Olímpico") %>% 
  addMarkers(lng=-47.878664, lat=-15.758767, popup="46 - EQN 408/409") %>% 
  addMarkers(lng=-47.880552, lat=-15.752944, popup="47 - EQN 410/411") %>% 
  addMarkers(lng=-47.902195, lat=-15.837818, popup="500 - Deck Sul Kids") %>% 
  addMarkers(lng=-47.861808, lat=-15.8357, popup="501 - Fórum Mundial da Água")


# outros ------

df_viagens <- df_viagens %>% left_join(select(df_estacoes, estacao, lat_inicial = lat, lon_inicial = lon), by = c("estacao_inicial" = "estacao")) %>% 
  left_join(select(df_estacoes, estacao, lat_final = lat, lon_final = lon), by = c("estacao_final" = "estacao"))

# links úteis -------------------------------------------------------------


# https://plot.ly/r/animations/



# http://kateto.net/network-visualization
# https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
# https://blog.dominodatalab.com/geographic-visualization-with-rs-ggmaps/
# https://github.com/dkahle/ggmap
# https://stackoverflow.com/questions/19817295/animating-ggmap-in-r
# http://nbfurey.wixsite.com/nathanbfurey/single-post/2015/01/11/R-package-for-animal-movement-analysis-and-visualization-TLoCoH
# https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
# https://stackoverflow.com/questions/41666472/maps-animation-in-r
# https://www.r-bloggers.com/animate-maps-with-mapmate-r-package-for-map-and-globe-based-still-image-sequences/
# https://leonawicz.github.io/mapmate/articles/networks.html