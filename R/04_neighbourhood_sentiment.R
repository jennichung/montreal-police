#### NEIGHBOURHOOD SENTIMENT ###################################################

# Determine a community sentiment value per neighbourhood
# Filter dates and rename variables according to what CRI you would like to calculate

for (n in seq_along(cityname)) {
  
  ner_locations[[n]]$doc_id <- 
    as.integer(ner_locations[[n]]$doc_id)
  
  media[[n]]$Date <- 
    as.Date(media[[n]]$Date)
  
  temp <- 
    ner_locations[[n]] %>% 
    dplyr::select(c("doc_id", "neighbourhood")) %>% 
    st_drop_geometry() %>% 
    inner_join(media[[n]] %>% 
                 filter(Date >= "2019-01-01") %>% 
                 # filter(Date >= "2018-01-01") %>% 
                 dplyr::select(c("doc_id", "sentiment")), .) %>% 
    distinct()
  
  neighbourhoods[[n]] <- 
    neighbourhoods[[n]] %>% 
    mutate(sentiment_1yr = NA,
           media_count_1yr = NA,
           CRI_1yr = NA)
  
  if (nrow(temp) != 0) {
    neighbourhoods[[n]] <- 
      neighbourhoods[[n]] %>% 
      left_join(left_join(aggregate(temp$sentiment, list(temp$neighbourhood), 
                                    mean),
                          temp %>% 
                            group_by(neighbourhood) %>% 
                            tally(), by = c("Group.1" = "neighbourhood")), 
                by = c("neighbourhood" = "Group.1")) %>% 
      mutate(sentiment_1yr = x,
             media_count_1yr = n, 
             CRI_1yr = -1 * sentiment_1yr * media_count_1yr) %>% 
      dplyr::select(-c("x", "n"))
  }
  
  rm(temp)
  
}