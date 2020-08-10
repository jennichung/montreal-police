#### SPATIAL ANALYSIS ##########################################################

source("R/01_helper_functions.R")

#load("data/media_merge.Rdata")
#load("data/ner_merge.Rdata")
#load("data/ner_locations_merge.Rdata")

#media_merge <- media
#ner_merge <- ner
#ner_locations_merge <- ner_locations

#load("data/media_2015.Rdata")
#load("data/ner_2015.Rdata")
#load("data/ner_locations_2015.Rdata")

#media_2015 <- media
#ner_2015 <- ner
#ner_locations_2015 <- ner_locations

#media <-
#  rbind(media_merge, media_2015)

#ner <-
#  rbind(ner_merge, ner_2015)

#ner_locations <-
#  rbind(ner_locations_merge, ner_locations_2015)

#save(media, file = "data/media.Rdata")
#save(ner, file = "data/ner.Rdata")
#save(ner_locations, file = "data/ner_locations.Rdata")

load("data/media.Rdata")
load("data/ner.Rdata")
load("data/ner_locations.Rdata")


# Perform a spatial join to match locations to boroughs

boroughs <- 
  st_read("data/LIMADMIN.shp") %>%
  st_transform(32618)

boroughs <- boroughs[ , -c(1, 2, 3, 5, 6, 7, 9)]

boroughs <-
  boroughs %>% 
  rename(borough = NOM, area = AIRE)

save(boroughs, file = "data/boroughs.Rdata")

load("data/boroughs.Rdata")

ner_locations <-
  ner_locations %>% 
  st_as_sf() %>% 
  st_join(boroughs) %>% 
  filter(!is.na(borough))


# Filter for keyword and plot average sentiment per borough

media %>% 
#  filter(str_detect(text, "systemic")) %>% 
  inner_join(ner_locations) %>% 
  add_count(borough) %>%   # counts the number of matches per borough
  filter(n > 100) %>%      # filters out the boroughs with less than 100 matches, for calculation
  group_by(borough) %>%   
  summarise(sentiment_mean = mean(sentiment, na.rm = TRUE)) %>% 
  inner_join(boroughs) %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = sentiment_mean)) +
  geom_sf(data = boroughs, colour = "black", fill = NA) +
  labs(title = "policing sentiment per borough",
#       subtitle = "keyword = 'systemic'",
       fill = "sentiment")+
  theme_minimal()

ggsave("output/average_systemic.png", width = 8, height = 4, units = "in", dpi = 150)


#  Filter for keyword and plot sentiment as dot density

media %>% 
#  filter(str_detect(text, "systemic")) %>% 
  inner_join(ner_locations) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(colour = sentiment)) +
  geom_sf(data = boroughs, colour = "black", fill = NA) +
  labs(title = "policing sentiment") +
#       subtitle = "keyword = 'systemic'")+
  theme_minimal()

ggsave("output/density_systemic.png", width = 8, height = 4, units = "in", dpi = 150)

