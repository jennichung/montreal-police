#### MEDIA ANALYSIS ############################################################

### Sentiment analysis #########################################################

# Create dictionary using the QDAP dictionary -----------------------------

dictionary <- 
  c(SentimentDictionaryBinary(qdapDictionaries::positive.words,
                              qdapDictionaries::negative.words))

# machine_learning_synonyms <- 
#   c("protest", "anti", "affordability", "mobilize", "mobilise", "mobilization", 
#     "mobilisation", "oppose",  "resist", "opposition", "gentrification", 
#     "threaten", "rent", "expensive", "unaffordable", "eviction", "landlord",
#     "threat", "manifestation", "complaint", "disapprove", "evict", 
#     "overtourism", "detriment", "ghost", "nuisance", "consultation", "opponent",
#     "discrimination", "critic", "crisis", "shortage", "blame", "garbage", 
#     "noise", "complain", "concern", "coalition", "hostile", "hostility", 
#     "fairbnb", "activist", "activism", "displace", "illegal", "housing", 
#     "market", "multiple", "listings", "disturbance", "damage", "threat", 
#     "residence", "hotel", "gap", "investment", "poor", "need", "rent hike", 
#     "community led", "housing stock", "nuisance", "garbage", "noise", "party", 
#     "disrespect", "lockbox", "regulation", "unregulate", "scarce", "fines", 
#     "fined", "shut", "backlash", "homeless", "homelessness", "affordability")
# 
# dictionary[["negativeWords"]] <- 
#   c(dictionary[["negativeWords"]], machine_learning_synonyms)


# Assign a score to each article ------------------------------------------

lemmatized_articles <-
  lemmatized_articles %>% 
      mutate(lemmas = unlist(map(lemmas, str_split, " "), recursive = FALSE),
             lemmas = map(lemmas, ~.x[str_detect(.x, "")]))

media <- 
  media %>% 
  mutate(sentiment = map_int(lemmatized_articles$lemmas, ~{
    sum(.x %in% dictionary[["positiveWords"]]) -
      sum(.x %in% dictionary[["negativeWords"]])}) / word_count)

rm(dictionary)


### Named entity recognition and geocoding ##################################### 

# Extract named entities --------------------------------------------------

ner_text <- 
  spacy_parse(media$text) %>% 
  entity_extract(type = "named", concatenator = " ") %>% 
  filter(entity_type == "GPE" |
           entity_type == "FAC" |
           entity_type == "LOC" |
           entity_type == "PERSON" |
           entity_type == "ORG") %>% 
  filter(nchar(entity) > 2) %>% 
  dplyr::select(doc_id, entity)

ner_headline <- 
  spacy_parse(media$headline) %>% 
  entity_extract(type = "named", concatenator = " ") %>% 
  filter(entity_type == "GPE" |
           entity_type == "FAC" |
           entity_type == "LOC" |
           entity_type == "PERSON" |
           entity_type == "ORG") %>% 
  filter(nchar(entity) > 2) %>% 
  dplyr::select(doc_id, entity)

ner <- bind_rows(ner_text, ner_headline) %>% 
  mutate(entity = tolower(gsub("[[:punct:]]", " ", entity)),
         entity = str_squish(entity)) %>% 
  distinct() %>% 
  as_tibble()

save(ner, file = "data/ner.Rdata")
rm(ner_text, ner_headline)


# Compress ner to reduce geolocation API calls ----------------------------

ner_compressed <- 
  ner %>% 
  distinct(entity)


# Compare named entities against database ---------------------------------

upgo_connect()

geolocation_remote <- dplyr::tbl(con, "geolocation")

ner_already_processed <- 
  geolocation_remote %>% 
  filter(entity %in% !!ner_compressed$entity) %>% 
  collect()

ner_to_process <- 
  ner_compressed %>% 
  filter(!entity %in% ner_already_processed$entity)


# Query Google to geocode named entities ----------------------------------

register_google(key = "AIzaSyAFGoWSQnuAtA8E7iXYfabhfy8igRvVqqw", write = TRUE)

iterations <- ceiling(nrow(ner_to_process) / 500)
locations_new <- vector("list", iterations)

for (i in seq_len(iterations)) {
  
  locations_new[[i]] <-
    ner_to_process %>% 
    slice(((i - 1) * 500 + 1):(i * 500)) %>% 
    mutate_geocode(entity, ext = "ca")
  
}

locations_new <- bind_rows(locations_new)


# Upload new results to server (only works with admin privileges)

RPostgres::dbWriteTable(upgo:::.upgo_env$con, "geolocation", locations_new, 
                        append = TRUE)

upgo_disconnect()


# Consolidate locations ---------------------------------------------------

locations <- bind_rows(locations_new, ner_already_processed)

# Remove locations that were not geocoded

locations <- 
  locations %>% 
  filter(!is.na(lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(32618)

# Perform a join to associate each entity with each document id

ner_locations <- inner_join(ner, locations, by = "entity")

# Perform a spatial join to match locations to boroughs
### NEED TO HAVE BOROUGH SPATIAL LAYER TO JOIN HERE

boroughs <- 
  st_read("data/Quartiers_sociologiques_2014.shp") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(32618)

ner_locations <-
  ner_locations %>% 
  st_as_sf() %>% 
  st_join(boroughs) #%>% 
#  filter(!is.na(borough))
  
