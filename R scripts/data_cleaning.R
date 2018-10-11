library(tidyverse)
library(lubridate)

# Importing the data and parsing the variables -----------------------

df_rides <- read_delim("data/rawdata_bikedf.csv", ";")

colnames(df_rides) <- c("user_gender", "user_birthdate", "user_residence", "ride_date", 
                        "time_start", "time_end", "station_start", "station_end")

df_rides <- df_rides %>% 
  separate(user_birthdate, c("user_birthdate", "trash"), 10) %>% 
  filter(trash != "7") %>% 
  select(-trash)

df_rides$user_birthdate <- dmy(df_rides$user_birthdate)
df_rides$ride_date <- dmy(df_rides$ride_date)
df_rides$user_gender <- factor(df_rides$user_gender, levels = c("M", "F"), labels = c("M", "F"))

# Creating new variables --------------------------------------------

df_rides <- df_rides %>% 
  mutate(ride_duration = (time_end - time_start)) %>% 
  mutate(ride_duration = ifelse(ride_duration >= 0, ride_duration / 60,
                                (86400 + ride_duration) / 60))

df_rides <- df_rides %>% 
  mutate(ride_duration = ifelse(ride_duration >= 1000, NA, ride_duration))

df_rides <- df_rides %>% 
  mutate(ride_late = ifelse(ride_duration > 60, 1, 0))



# Creating the dataset df_stations ----------------------------------------


df_stations <- tibble(station = unique(df_rides$station_start),
                      temp = unique(df_rides$station_start))

df_stations <- df_stations %>% 
  separate(temp, c("station_number", "station_name"), "-")

df_stations$station_number <- as.integer(df_stations$station_number)

df_stations$lat <- rep(0, 50)
df_stations$lon <- rep(0, 50)

df_stations <- df_stations %>% filter(station != "900 - Estação Teste")

df_stations <- df_stations %>% arrange(station_number)

for(i in 1:nrow(df_stations)){
  cat(paste("df_stations[df_stations$station == \"",df_stations$station[i],"\",]$lat <- \n", sep = ""))
  cat(paste("df_stations[df_stations$station == \"",df_stations$station[i],"\",]$lon <- \n", sep = ""))
}

df_stations[df_stations$station == "1 - Memorial JK",]$lat <- -15.783479
df_stations[df_stations$station == "1 - Memorial JK",]$lon <- -47.913372
df_stations[df_stations$station == "2 - Praça Buriti",]$lat <- -15.785651
df_stations[df_stations$station == "2 - Praça Buriti",]$lon <- -47.908750
df_stations[df_stations$station == "3 - Centro de Convenções",]$lat <- -15.786409
df_stations[df_stations$station == "3 - Centro de Convenções",]$lon <- -47.899865
df_stations[df_stations$station == "4 - Torre de TV",]$lat <- -15.789581
df_stations[df_stations$station == "4 - Torre de TV",]$lon <- -47.894312
df_stations[df_stations$station == "5 - Setor Hoteleiro Norte",]$lat <- -15.789029
df_stations[df_stations$station == "5 - Setor Hoteleiro Norte",]$lon <- -47.891015
df_stations[df_stations$station == "6 - Rodoviária",]$lat <- -15.793930
df_stations[df_stations$station == "6 - Rodoviária",]$lon <- -47.881529
df_stations[df_stations$station == "7 - Catedral Metropolitana",]$lat <- -15.797438
df_stations[df_stations$station == "7 - Catedral Metropolitana",]$lon <- -47.874552
df_stations[df_stations$station == "8 - Ministério da Defesa",]$lat <- -15.795867
df_stations[df_stations$station == "8 - Ministério da Defesa",]$lon <- -47.870901
df_stations[df_stations$station == "9 - Ministério da Cultura",]$lat <- -15.798295
df_stations[df_stations$station == "9 - Ministério da Cultura",]$lon <- -47.871807
df_stations[df_stations$station == "10 - Ministério dos Transportes",]$lat <- -15.796830
df_stations[df_stations$station == "10 - Ministério dos Transportes",]$lon <- -47.867795
df_stations[df_stations$station == "11 - Rodoviária 2",]$lat <- -15.794622
df_stations[df_stations$station == "11 - Rodoviária 2",]$lon <- -47.881721
df_stations[df_stations$station == "12 - Rodoviária 3",]$lat <- -15.794242
df_stations[df_stations$station == "12 - Rodoviária 3",]$lon <- -47.883524
df_stations[df_stations$station == "13 - Liberty Mall",]$lat <- -15.785720
df_stations[df_stations$station == "13 - Liberty Mall",]$lon <- -47.883563
df_stations[df_stations$station == "14 - Brasília Shopping",]$lat <- -15.786409
df_stations[df_stations$station == "14 - Brasília Shopping",]$lon <- -47.887898
df_stations[df_stations$station == "15 - Brasil 21",]$lat <- -15.794724
df_stations[df_stations$station == "15 - Brasil 21",]$lon <- -47.895230
df_stations[df_stations$station == "16 - SRTVS",]$lat <- -15.796389
df_stations[df_stations$station == "16 - SRTVS",]$lon <- -47.893437
df_stations[df_stations$station == "17 - Shopping Pátio Brasil",]$lat <- -15.795827
df_stations[df_stations$station == "17 - Shopping Pátio Brasil",]$lon <- -47.890914
df_stations[df_stations$station == "18 - Praça do Povo",]$lat <- -15.798575
df_stations[df_stations$station == "18 - Praça do Povo",]$lon <- -47.889059
df_stations[df_stations$station == "19 - Banco Central",]$lat <- -15.802146
df_stations[df_stations$station == "19 - Banco Central",]$lon <- -47.883785
df_stations[df_stations$station == "20 - SAS",]$lat <- -15.799919
df_stations[df_stations$station == "20 - SAS",]$lon <- -47.880428
df_stations[df_stations$station == "21 - EQS 202 / 203",]$lat <- -15.807616
df_stations[df_stations$station == "21 - EQS 202 / 203",]$lon <- -47.885283
df_stations[df_stations$station == "22 - 102 Sul",]$lat <- -15.805286
df_stations[df_stations$station == "22 - 102 Sul",]$lon <- -47.889203
df_stations[df_stations$station == "23 - STF",]$lat <- -15.801647
df_stations[df_stations$station == "23 - STF",]$lon <- -47.862147
df_stations[df_stations$station == "24 - Galeria",]$lat <- -15.798696
df_stations[df_stations$station == "24 - Galeria",]$lon <- -47.884806
df_stations[df_stations$station == "25 - Deck Sul",]$lat <- -15.837970
df_stations[df_stations$station == "25 - Deck Sul",]$lon <- -47.902858
df_stations[df_stations$station == "26 - Ministério da Saude",]$lat <- -15.799611
df_stations[df_stations$station == "26 - Ministério da Saude",]$lon <- -47.868395
df_stations[df_stations$station == "27 - Ministério do Planejamento",]$lat <- -15.794664
df_stations[df_stations$station == "27 - Ministério do Planejamento",]$lon <- -47.874487
df_stations[df_stations$station == "28 - CNMP - Conselho Nacional do Ministério Público",]$lat <- -15.804550
df_stations[df_stations$station == "28 - CNMP - Conselho Nacional do Ministério Público",]$lon <- -47.867038
df_stations[df_stations$station == "29 - INMETRO",]$lat <- -15.780858
df_stations[df_stations$station == "29 - INMETRO",]$lon <- -47.883277
df_stations[df_stations$station == "30 - TSE",]$lat <- -15.809428
df_stations[df_stations$station == "30 - TSE",]$lon <- -47.868797
df_stations[df_stations$station == "31 - EQS 204/205",]$lat <- -15.812401
df_stations[df_stations$station == "31 - EQS 204/205",]$lon <- -47.890381
df_stations[df_stations$station == "32 - SQS 305",]$lat <- -15.809892
df_stations[df_stations$station == "32 - SQS 305",]$lon <- -47.897377
df_stations[df_stations$station == "33 - EQS 104/304",]$lat <- -15.807126
df_stations[df_stations$station == "33 - EQS 104/304",]$lon <- -47.894236
df_stations[df_stations$station == "34 - Shopping Mall",]$lat <- -15.802090
df_stations[df_stations$station == "34 - Shopping Mall",]$lon <- -47.892749
df_stations[df_stations$station == "35 - CLN 403",]$lat <- -15.780134
df_stations[df_stations$station == "35 - CLN 403",]$lon <- -47.875000
df_stations[df_stations$station == "36 - CLN 204",]$lat <- -15.776928
df_stations[df_stations$station == "36 - CLN 204",]$lon <- -47.877007
df_stations[df_stations$station == "37 - EQN 104/105",]$lat <- -15.774583
df_stations[df_stations$station == "37 - EQN 104/105",]$lon <- -47.883707
df_stations[df_stations$station == "38 - SQN 205",]$lat <- -15.770067
df_stations[df_stations$station == "38 - SQN 205",]$lon <- -47.878153
df_stations[df_stations$station == "39 - CLN 406",]$lat <- -15.765826
df_stations[df_stations$station == "39 - CLN 406",]$lon <- -47.876935
df_stations[df_stations$station == "40 - W2/EQN305/306",]$lat <- -15.771618
df_stations[df_stations$station == "40 - W2/EQN305/306",]$lon <- -47.887756
df_stations[df_stations$station == "41 - Instituto de Artes",]$lat <- -15.763955
df_stations[df_stations$station == "41 - Instituto de Artes",]$lon <- -47.870486
df_stations[df_stations$station == "42 - PAT",]$lat <- -15.759220
df_stations[df_stations$station == "42 - PAT",]$lon <- -47.870397
df_stations[df_stations$station == "43 - Biblioteca Central",]$lat <- -15.761123
df_stations[df_stations$station == "43 - Biblioteca Central",]$lon <- -47.867443
df_stations[df_stations$station == "44 - ICC Sul",]$lat <- -15.765966
df_stations[df_stations$station == "44 - ICC Sul",]$lon <- -47.867058
df_stations[df_stations$station == "45 - Centro Olímpico",]$lat <- -15.765133
df_stations[df_stations$station == "45 - Centro Olímpico",]$lon <- -47.857989
df_stations[df_stations$station == "46 - EQN 408/409",]$lat <- -15.758767
df_stations[df_stations$station == "46 - EQN 408/409",]$lon <- -47.878664
df_stations[df_stations$station == "47 - EQN 410/411",]$lat <- -15.752944
df_stations[df_stations$station == "47 - EQN 410/411",]$lon <- -47.880552
df_stations[df_stations$station == "500 - Deck Sul Kids",]$lat <- -15.837818
df_stations[df_stations$station == "500 - Deck Sul Kids",]$lon <- -47.902195
df_stations[df_stations$station == "501 - Fórum Mundial da Água",]$lat <- -15.835700
df_stations[df_stations$station == "501 - Fórum Mundial da Água",]$lon <- -47.861808


