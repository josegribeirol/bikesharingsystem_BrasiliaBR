library(tidyverse)
library(lubridate)

# user_gender

df_rides %>% filter(!is.na(user_gender)) %>% 
  ggplot() +
  geom_bar(aes(x = user_gender)) +
  theme_light()



# user_birthdate

df_rides %>% filter(!is.na(user_birthdate)) %>% 
  ggplot() +
  geom_histogram(aes(x = user_birthdate)) +
  theme_light()+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")


#