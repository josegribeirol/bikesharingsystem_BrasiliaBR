library(tidyverse)
library(lubridate)

# user_gender

df_rides %>% filter(!is.na(user_gender)) %>% 
  ggplot() +
  geom_bar(aes(x = user_gender)) +
  theme_light()+
  labs(x = "Gender of the User", y = "Rides")



# user_birthdate

df_rides %>% filter(!is.na(user_birthdate)) %>% 
  ggplot() +
  geom_histogram(aes(x = user_birthdate)) +
  theme_light()+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")+
  labs(x = "User Birthdate", y = "Rides")


# station_start

df_rides %>% group_by(station_start) %>% summarise(n = n()) %>% 
  ggplot() +
  geom_bar(aes(x = reorder(station_start, n), y = n), stat = "identity")+
  coord_flip()+
  labs(x = "Station of Departure", y = "Rides")+
  theme_light()
