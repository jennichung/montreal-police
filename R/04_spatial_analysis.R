#### SPATIAL ANALYSIS ##########################################################

# Perform a spatial join to match locations to boroughs
### NEED TO HAVE BOROUGH SPATIAL LAYER TO JOIN HERE
ner_locations <-
  ner_locations %>% 
  st_join(boroughs) %>%
  filter(!is.na(borough))

