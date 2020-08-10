###  Create scatterplots

load("data/censusbyborough.Rdata")

media %>% 
  inner_join(ner_locations) %>% 
  add_count(borough) %>%   # counts the number of matches per borough
  filter(n > 100) %>%      # filters out the boroughs with less than 100 matches, for calculation
  group_by(borough) %>% 
  summarise(average_sentiment = mean(sentiment, na.rm = TRUE)) %>% 
  right_join(censusbyborough) %>% 
  filter(!is.na(average_sentiment)) %>%    # filters out the boroughs with less than 100 matches, for plotting
  st_as_sf() %>% 
  ggplot(aes(x = average_sentiment, y = black_pct_pop, colour = borough)) +
  geom_point(alpha = 0.5, size = 5) +
  geom_label_repel(aes(label = borough), size = 3, box.padding = 0.25, point.padding	= 0.5, segment.color = "grey50") +
  scale_x_continuous(name = "Mean Sentiment Score") +
  scale_y_continuous(name = "Black Population (%)") +
  labs(title = "Relationship between policing sentiment and Black population",
       subtitle = "Montreal, per borough") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("output/scatter_black.png", width = 8, height = 4, units = "in", dpi = 150)

