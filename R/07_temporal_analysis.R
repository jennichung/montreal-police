# Plot simple longitudinal trend line

media %>% 
  filter(str_detect(text, "systemic")) %>% 
  filter(date >= "2015-01-01", date <= "2020-06-30") %>%
  group_by(date) %>%
  summarize(avg_sentiment = mean(sentiment)) %>%
  ggplot(aes(date, avg_sentiment)) +
  geom_line() +
  geom_smooth()+
  theme_minimal()+
  tidyquant::coord_x_date(xlim = c("2020-01-01", "2020-06-30"),
               ylim = c(-0.15, 0.025))+
  geom_vline(xintercept = as.numeric(as.Date('2020-05-25')), colour = "red")

ggsave("output/trend.png", width = 8, height = 4, units = "in", dpi = 150)


# Plot number of articles over time, island-wide

media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  group_by(month_yr) %>% 
  count() %>% 
  ggplot(aes(x = month_yr, y = n)) +
  geom_line(color = "#3F2949", 
            lwd = 0.25, 
            show.legend = TRUE)+
  geom_smooth(method = "gam",
              color = "#3F2949", 
              fill = "#CABED0", 
              lwd = 2,
              show.legend = TRUE) +
  xlab("\nDate") +
  ylab("Number of news articles\n") +
  ggtitle("STR discourse throughout the United States over time\n") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


# Plot sentiment over time per borough 

media$date <- as.Date(media$date)
media$year <- format(as.Date(media$date), "%Y")
media$yymm <- format(as.Date(media$date), "%Y-%m")

media %>%
  inner_join(ner_locations) %>% 
  filter(date >= "2015-01-01" & date <= "2020-07-01") %>%
#  add_count(borough) %>%   
#  filter(n > 100) %>%      
  group_by(borough, yymm) %>% 
  summarise(average_sentiment = mean(sentiment, na.rm = TRUE)) %>%
#  filter(!is.na(average_sentiment)) %>%    
  filter(borough == "Montréal-Nord" | 
           borough == "Rivière-des-Prairies-Pointe-aux-Trembles" | 
           borough == "Côte-des-Neiges-Notre-Dame-de-Grâce" |
           borough == "Ville-Marie") %>% 
  ggplot(aes(x = yymm, y = average_sentiment, group = borough)) +
  geom_line(aes(colour = borough)) +
  geom_point(aes(colour = borough)) +
  theme_minimal() +
  theme(legend.position = "bottom")

media %>%
  inner_join(ner_locations) %>% 
  filter(date >= "2015-01-01" & date <= "2020-07-01") %>%
  #  add_count(borough) %>%   
  #  filter(n > 100) %>%      
  group_by(borough, year) %>% 
  summarise(average_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  #  filter(!is.na(average_sentiment)) %>%    
  filter(borough == "Montréal-Nord" | 
           borough == "Mercier-Hochelaga-Maisonneuve" | 
           borough == "Côte-des-Neiges-Notre-Dame-de-Grâce" |
           borough == "Ville-Marie") %>% 
  ggplot(aes(x = year, y = average_sentiment, group = borough)) +
  geom_line(aes(colour = borough)) +
  geom_point(aes(colour = borough)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/line_peryear_selectboroughs.png", width = 8, height = 4, units = "in", dpi = 150)


media %>%
  inner_join(ner_locations) %>% 
  filter(date >= "2015-01-01" & date <= "2020-07-01") %>%
  add_count(borough) %>%   
  filter(n > 100) %>%      
  group_by(borough, year) %>% 
  summarise(average_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  filter(!is.na(average_sentiment)) %>%    
#  filter(borough == "Montréal-Nord") %>% 
  ggplot(aes(x = year, y = average_sentiment, group = borough)) +
  geom_line(aes(colour = borough)) +
  geom_point(aes(colour = borough)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/line_peryear.png", width = 8, height = 4, units = "in", dpi = 150)

