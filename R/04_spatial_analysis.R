#### SPATIAL ANALYSIS ##########################################################

source("R/01_helper_functions.R")

load("data/media.Rdata")
load("data/ner.Rdata")
load("data/ner_locations.Rdata")

# Perform a spatial join to match locations to boroughs
### NEED TO HAVE BOROUGH SPATIAL LAYER TO JOIN HERE

boroughs <- 
  st_read("data/LIMADMIN.shp") %>%
  st_transform(32618)

boroughs <- boroughs[ , -c(1, 2, 3, 6, 7, 9)]

boroughs <-
  boroughs %>% 
  rename(name = NOM, type = TYPE, area = AIRE)

ner_locations <-
  ner_locations %>% 
  st_as_sf() %>% 
  st_join(boroughs) %>% 
  filter(!is.na(name))

# Filter for keyword and plot sentiment

media %>% 
  filter(str_detect(text, "systemic")) %>% 
  inner_join(ner_locations) %>% 
  group_by(name) %>% 
  summarise(sentiment_mean = mean(sentiment, na.rm = TRUE)) %>% 
  inner_join(boroughs) %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = sentiment_mean)) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  labs(title = "policing sentiment per borough",
       subtitle = "keyword = 'systemic'",
       fill = "sentiment")

ggsave("output/policing_systemic.png")


